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
    import Control.Monad (foldM, join)
    import qualified Game.Backend.Common as GBC
    import Control.Monad.State.Strict
    import Debug.Trace 

    serve :: MonadGame ServerEvent ClientEvent GameInfo m
          => Game -> m ()
    serve s = recvEvent >>= logId >>= go
        where
            go (GameEvent pid evt) = do
                logJSON (pid, evt)
                serveEvent s pid evt >>= sendUpdates >>= serve
            go (PlayerConnected pid) = connectPlayer pid s >>= serve
            go (PlayerDisconnected pid) = disconnectPlayer pid s >>= serve

    serveEvent current pid evt
        =   if beingJudged current then 
                serveJudgementEvt current pid evt
            else
                serveAwaitEvt current pid evt
   
    connectPlayer pid s = do
        let (_, ns) = runState (addPlayer pid) s 
        modifyPublic (gameInfoScores .~ (playerScores ns))
        sendUpdates ns

    disconnectPlayer pid s = do
        let (_, ns) = runState (sitOutPlayer pid) s
        sendUpdates ns

    sendError p msg s = 
        (sendPlayer p $ InvalidSend msg) >> logJSON (errLabel, p, msg) >> return s
        where
            errLabel :: T.Text
            errLabel = "Error:"

    serveJudgementEvt c p e
        | c `judgedBy` p = case e of
            SelectWinner w -> case increaseScoreJudgement c w of
                Just c' -> do
                    logJSON ("Awarded round win" :: T.Text, w)
                    logShow c' 
                    startRound c'
                Nothing -> sendError p "Judgement did not exist" c
            _ -> sendError p "You must select a winner" c
        | otherwise = sendError p "You must select a winner" c
        
    startRound g = do
        let (nc, ng') = runState nextTurn g
        return ng'
    
    dealCardsU i g pid = let (nc, ng) = runState (dealCardsTo i pid) g in ng

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
                return $ startJudgement updated
            else return updated 
             
    sendUpdate :: (MonadSender ServerEvent m, MonadLog m)
               => Game -> PlayerId -> m ()
    sendUpdate g id = do
        let ap = traceShowId $ g ^. gamePlayers
        let ps = traceShowId $ Map.lookup id ap
        logJSON ("Got personalState" :: T.Text, ps)
        let evt = UpdateState (toPublicGame g) <$> ps
        sendPlayerMaybe id evt

    sendUpdates g = do
        logMessage "Sending updates"
        let keys = traceShowId (g ^. gamePlayers & Map.keys)
        mapM_ (sendUpdate g) keys 
        logMessage "Sent updates"
        return g