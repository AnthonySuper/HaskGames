{-# LANGUAGE TemplateHaskell
           , DeriveAnyClass
           , DeriveGeneric
           , OverloadedStrings
           , FlexibleContexts
           , MultiParamTypeClasses #-}
module Game.FillBlanks.Server where

    import GHC.Generics
    import qualified Data.Text as T
    import Control.Lens
    import Data.Aeson
    import Data.Semigroup
    import qualified Data.Map as Map
    import Game.FillBlanks.Deck
    import Game.FillBlanks.Game
    import Game.FillBlanks.ServerState
    import Game.FillBlanks.Event
    import Game.Common
    import Data.Maybe
    import Control.Monad (foldM)
    import qualified Game.Backend.Common as GBC
   
    serve :: (MonadGame ServerEvent m)
          => FillBlanksState
          -> PlayerId
          -> ClientEvent
          -> m FillBlanksState
    serve current pid evt =
        case (current ^. commonState . commonStateStatus) of
            AwaitingSubmissions -> serveAwaitEvt current pid evt
            AwaitingJudgement -> serveJudgementEvt current pid evt

    playerConnectState :: (MonadGame ServerEvent m)
                       => FillBlanksState -> PlayerId -> m PlayerState
    playerConnectState _ _ = return $ PlayerState 0

    playerWillConnect :: (MonadGame ServerEvent m)
                      => FillBlanksState -> PlayerId -> m FillBlanksState
    playerWillConnect s _ = return s

    playerDidConnect :: (MonadGame ServerEvent m)
                     => FillBlanksState -> PlayerId -> m FillBlanksState
    playerDidConnect gs pid = do
        ns <- dealCardsG 8 gs pid
        updateScores ns
        return ns
    
    playerDisconnected :: (MonadGame ServerEvent m)
                       => FillBlanksState -> FillBlanksState -> PlayerId -> m FillBlanksState
    playerDisconnected s s' p
        | (isJudge s p) = startRound s'
        | otherwise = return s'
    
    updateScores :: (MonadGame ServerEvent m)
                 => FillBlanksState
                 -> m ()
    updateScores s = broadcast $ UpdateScores scores
        where
            scores = s ^. playerState & Map.map (^. playerStateScore)

    sendError :: (MonadGame ServerEvent m)
              => PlayerId
              -> T.Text
              -> FillBlanksState
              -> m FillBlanksState
    sendError p msg s = (sendPlayer p $ InvalidSend msg) >> return s

    serveJudgementEvt :: (MonadGame ServerEvent m)
                      => FillBlanksState
                      -> PlayerId
                      -> ClientEvent
                      -> m FillBlanksState
    serveJudgementEvt c p e
        | (isJudge c p) = case e of
            SelectWinner w -> case increaseScoreJudgement c w of
                Just c' -> startRound c'
                Nothing -> sendError p "Judgement did not exist" c
            _ -> sendError p "You must select a winner" c
        | otherwise = sendError p "You must select a winner" c
        
    startRound g = do
        let ng = g & (commonState . commonStateCases .~ mempty)
                   & nextJudge
        let (nc, ng') = nextCall ng
        updateScores ng'
        let sr = StartRound (g ^. commonState . commonStateJudge) nc
        broadcast sr
        foldM (dealCardsG $ nc ^. callArity) ng' (ng' ^. playerState & Map.keys)
    
    dealCardsG :: (MonadGame ServerEvent m)
               => Int
               -> FillBlanksState
               -> PlayerId
               -> m FillBlanksState
    dealCardsG i g pid = do
        let (nc, ng) = dealCards i g
        sendPlayer pid $ DealCards nc
        return ng

    serveAwaitEvt :: (MonadGame ServerEvent m)
                  => FillBlanksState
                  -> PlayerId
                  -> ClientEvent
                  -> m FillBlanksState
    serveAwaitEvt c p evt = go evt
        where
            go (SubmitJudgement j) = addSubmission c p j
            go _ = sendError p "You must select a winner" c
    
    addSubmission :: (MonadGame ServerEvent m)
                  => FillBlanksState
                  -> PlayerId
                  -> JudgementCase
                  -> m FillBlanksState
    addSubmission s p j
        | s `judgedBy` p = 
            sendError p "Judges cannot submit responses" s
        | s `hasSubmissionFrom` p = 
            sendError p "You've already submitted" s
        | otherwise = do
            let updated = addJudgement s p j
            if judgeable updated then do
                broadcast $ 
                    StartJudgement (Map.keys $ updated ^. commonState . commonStateCases)
                return $ 
                    updated & commonState . commonStateStatus .~ AwaitingJudgement
            else return updated

    instance GBC.Game PlayerState CommonState ServerEvent ClientEvent where
        serve = serve
        playerConnectState = playerConnectState
        playerWillConnect = playerWillConnect
        playerDidConnect = playerDidConnect
        playerDisconnected = playerDisconnected