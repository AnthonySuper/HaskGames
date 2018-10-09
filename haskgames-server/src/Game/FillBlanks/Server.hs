{-# LANGUAGE TemplateHaskell
           , DeriveAnyClass
           , DeriveGeneric
           , OverloadedStrings
           , FlexibleContexts
           , MultiParamTypeClasses
           , NoMonomorphismRestriction
           , ConstraintKinds #-}

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

    type GSMonad m
        = ( MonadGame ServerEvent ClientEvent GameInfo m
          , MonadState Game m 
          )

    ifM :: (Monad m) => m Bool -> m a -> m a -> m a
    ifM p t f = p >>= (\p' -> if p' then t else f)

    serve :: (GSMonad m)
          => m ()
    serve = do
        e <- recvEvent
        logJSON e
        go e
        sendUpdates
        serve
        where
            go (GameEvent pid (SendChat msg)) = routeChat pid msg
            go (GameEvent pid evt) = serveEvent pid evt
            go (PlayerConnected pid) = connectPlayer pid
            go (PlayerDisconnected pid) = disconnectPlayer pid

    routeChat pid msg =
        broadcast $ ChatMessage pid msg 

    serveEvent :: (GSMonad m)
               => PlayerId -> ClientEvent -> m ()
    serveEvent pid evt = 
            ifM beingJudgedM 
                (serveJudgementEvt pid evt)
                (serveAwaitEvt pid evt)

    
    connectPlayer :: (GSMonad m)
                  => PlayerId -> m () 
    connectPlayer pid = do
        addPlayer pid
        ns <- get 
        modifyPublic (gameInfoScores .~ (playerScores ns))

    disconnectPlayer :: (GSMonad m)
                     => PlayerId -> m ()
    disconnectPlayer pid = do
        sitOutPlayer pid

    sendError :: (GSMonad m)
              => PlayerId -> T.Text -> m ()
    sendError p msg = 
        (sendPlayer p $ InvalidSend msg) >> logJSON (errLabel, p, msg)
        where
            errLabel :: T.Text
            errLabel = "Error:"
    
    serveJudgementEvt :: (GSMonad m)
                      => PlayerId -> ClientEvent -> m ()
    serveJudgementEvt p e =
        ifM (judgedByM p) (getWinner e) sendError'
        where
            getWinner (SelectWinner w)
                = ifM (increaseScoreJudgementM w) startRound sendError'
            getWinner _ = sendError'
            sendError' = sendError p "Invalid event"
        
    startRound = nextTurn

    serveAwaitEvt p = go
        where
            go (SubmitJudgement j) = addSubmission p j
            go _ = sendError p "You must select a winner"
    
    addSubmission :: (GSMonad m)
                  => PlayerId -> JudgementCase -> m ()
    addSubmission p j =
        ifM (judgedByM p) (sendError p "Judges cannot submit")
            $ ifM (hasSubmissionM p) 
                (sendError p "Already submitted")
                (addSubmission')
        where
            addSubmission' = do
                addJudgementM p j
                s <- judgeableM
                when s startJudgementM
             
    sendUpdate :: (MonadSender ServerEvent m, MonadLog m)
               => Game -> PlayerId -> m ()
    sendUpdate g id = do
        let ap = g ^. gamePlayers
        let ps = Map.lookup id ap
        let evt = UpdateState (toPublicGame g) <$> ps
        sendPlayerMaybe id evt

    sendUpdates :: (GSMonad m) => m ()
    sendUpdates = do
        g <- get 
        keys <- use gamePlayers <&> Map.keys
        mapM_ (sendUpdate g) keys