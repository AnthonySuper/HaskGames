{-# LANGUAGE TemplateHaskell
           , DeriveAnyClass
           , DeriveGeneric
           , OverloadedStrings #-}
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

   
    serve :: (MonadGame m)
          => FillBlanksState
          -> RecvMessage ClientEvent
          -> m FillBlanksState
    serve current (GameEvent pid evt) =
        go current pid evt
        where
            go = 
                case (current ^. commonState . commonStateStatus) of
                    AwaitingSubmissions -> serveAwaitEvt
                    AwaitingJudgement -> serveJudgementEvt
    serve current (PlayerConnected pid) = do
        ns <- dealCardsG 8 current pid
        updateScores ns
        return ns

    updateScores :: (MonadGame m)
                 => FillBlanksState
                 -> m ()
    updateScores s = broadcast scores
        where
            scores = s ^. playerState & Map.map (^. playerStateScore)

    sendError :: (MonadGame m)
              => PlayerId
              -> T.Text
              -> FillBlanksState
              -> m FillBlanksState
    sendError p msg s = (sendPlayer p $ InvalidSend msg) >> return s

    serveJudgementEvt :: (MonadGame m)
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
    
    dealCardsG :: (MonadGame m)
               => Int
               -> FillBlanksState
               -> PlayerId
               -> m FillBlanksState
    dealCardsG i g pid = do
        let (nc, ng) = dealCards i g
        sendPlayer pid $ DealCards nc
        return ng

    serveAwaitEvt :: (MonadGame m)
                  => FillBlanksState
                  -> PlayerId
                  -> ClientEvent
                  -> m FillBlanksState
    serveAwaitEvt c p evt = go evt
        where
            go (SubmitJudgement j) = addSubmission c p j
            go _ = do
               sendError p "You must select a winner" c
    
    addSubmission :: (MonadGame m)
                  => FillBlanksState
                  -> PlayerId
                  -> JudgementCase
                  -> m FillBlanksState
    addSubmission s p j
        | s `judgedBy` p = do
            sendError p "Judges cannot submit responses" s
        | s `hasSubmissionFrom` p = do
            sendError p "You've already submitted" s
        | otherwise = do
            let updated = addJudgement s p j
            if judgeable updated then do
                broadcast $ 
                    StartJudgement (Map.keys $ updated ^. commonState . commonStateCases)
                return $ 
                    updated & commonState . commonStateStatus .~ AwaitingJudgement
            else return updated