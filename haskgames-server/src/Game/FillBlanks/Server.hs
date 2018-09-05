{-# LANGUAGE TemplateHaskell
           , DeriveAnyClass
           , DeriveGeneric
           , OverloadedStrings
           , FlexibleContexts
           , MultiParamTypeClasses
           , NoMonomorphismRestriction #-}
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
    import Game.Basic
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
        let s' = s & gameActivePlayers . at pid .~ (Just $ PersonalState [] SittingOut 0)
        ns <- dealCardsG 6 s' pid
        modifyPublic (gameInfoScores .~ (playerScores ns))
        return ns

    sendError p msg s = (sendPlayer p $ InvalidSend msg) >> return s

    serveJudgementEvt c p e
        | (isJudge c p) = case e of
            SelectWinner w -> case increaseScoreJudgement c w of
                Just c' -> startRound c'
                Nothing -> sendError p "Judgement did not exist" c
            _ -> sendError p "You must select a winner" c
        | otherwise = sendError p "You must select a winner" c
        
    startRound g = do
        let (nc, ng') = runState (nextTurn) g
        return ng'
    
    dealCardsG i g pid = do
        let (nc, ng) = runState (dealCards i) g
        sendUpdates ng

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
                let updated' = updated & gameStatus .~ AwaitingJudgement
                sendUpdates updated'
            else sendUpdates updated 
             
    sendUpdate :: MonadSender ServerEvent m
               => Game -> PlayerId -> m ()
    sendUpdate g id = do
        let pinfolens = gameActivePlayers . at id 
        let mby = g ^?! pinfolens 
        let evt = UpdateState (toPublicGame g) (fromJust mby)
        sendPlayer id evt

    sendUpdates g = do
        mapM (sendUpdate g) (g ^. gameActivePlayers & Map.keys)
        return g