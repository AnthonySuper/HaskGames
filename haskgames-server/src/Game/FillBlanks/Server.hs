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
    import Control.Monad.State.Strict

    serve :: MonadGame ServerEvent ClientEvent GameInfo m
          => Game -> m ()
    serve s = recvEvent >>= logId >>= go
        where
            go (GameEvent pid evt) =
                serveEvent s pid evt >>= serve
            go (PlayerConnected pid) = connectPlayer pid s >>= serve
            logId i = logJSON i >> return i


    serveEvent current pid evt = do
        logJSON (pid, evt)
        case (current ^. gameStatus) of
            AwaitingSubmissions -> serveAwaitEvt current pid evt
            AwaitingJudgement -> serveJudgementEvt current pid evt
   
    connectPlayer pid s = do
        let s' = s & gameActivePlayers . at pid .~ (Just $ Player 0)
        updateScores s'
        ns <- dealCardsG 6 s' pid
        modifyPublic (gameInfoScores .~ (playerScores ns))
        return ns
{-
 playerConnectState :: (MonadGame ServerEvent m)
                       => FillBlanksState -> PlayerId -> m PlayerState
    playerConnectState s _ = do
        logShow ("Getting initial player state with", s) 
        return $ PlayerState 0

    playerWillConnect :: (MonadGame ServerEvent m)
                      => FillBlanksState -> PlayerId -> m FillBlanksState
    playerWillConnect s _ = return s

    playerDidConnect :: (MonadGame ServerEvent m)
                     => FillBlanksState -> PlayerId -> m FillBlanksState
    playerDidConnect gs pid = do
        logShow pid
        ns <- dealCardsG 8 gs pid
        updateScores ns
        sendPlayer pid $
            StartRound (gs ^. commonState . commonStateJudge) (gs ^. commonState . commonStateCurrentCall)
        return ns
    
    playerDisconnected :: (MonadGame ServerEvent m)
                       => FillBlanksState -> FillBlanksState -> PlayerId -> m FillBlanksState
    playerDisconnected s s' p
        | (isJudge s p) = startRound s'
        | otherwise = return s'

-}
    
    updateScores s = broadcast $ UpdateScores scores
        where
            scores = s ^. gameActivePlayers & Map.map (^. playerScore)


    sendError p msg s = (sendPlayer p $ InvalidSend msg) >> return s


    serveJudgementEvt c p e
        | (isJudge c p) = case e of
            SelectWinner w -> case increaseScoreJudgement c w of
                Just c' -> startRound c'
                Nothing -> sendError p "Judgement did not exist" c
            _ -> sendError p "You must select a winner" c
        | otherwise = sendError p "You must select a winner" c
        
    startRound g = do
        let (nc, ng') = runState (clearCases >> nextJudge >> extractCall) g
        updateScores ng'
        let sr = StartRound (ng' ^. gameJudge) nc
        broadcast sr
        foldM (dealCardsG $ nc ^. callArity) ng' (ng' ^. gameActivePlayers & Map.keys)
    
    dealCardsG i g pid = do
        let (nc, ng) = runState (dealCards i) g
        sendPlayer pid $ DealCards nc
        return ng

    serveAwaitEvt c p = go
        where
            go (SubmitJudgement j) = addSubmission c p j
            go _ = sendError p "You must select a winner" c
    
    addSubmission s p j
        | s `judgedBy` p = do
            sendError p "Judges cannot submit responses" s
        | s `hasSubmissionFrom` p = do
            sendError p "You've already submitted" s
        | otherwise = do
            let updated = addJudgement s p j
            if judgeable updated then do
                broadcast $ 
                    StartJudgement (Map.keys $ updated ^. gameCases)
                return $ 
                    updated & gameStatus .~ AwaitingJudgement
            else return updated
