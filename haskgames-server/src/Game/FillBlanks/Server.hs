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
    import Game.Common
    import Data.Maybe

    data ServerEvent
        = StartRound PlayerId CallCard
        | StartJudgement [JudgementCase]
        | RoundWinner JudgementCase T.Text
        | UpdateScores (Map.Map T.Text Integer)
        | GameWinner T.Text
        | InvalidSend T.Text
        | DealCards [ResponseCard]
        deriving (Show, Read, Eq, Generic, ToJSON)

    data ClientEvent
        = SubmitJudgement JudgementCase
        | SelectWinner JudgementCase

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
        let (cards, ns) = dealCards current
        sendPlayer pid $ DealCards cards
        updateScores ns
        return ns

    updateScores :: (MonadGame m)
                 => FillBlanksState
                 -> m ()
    updateScores s = broadcast scores
        where
            scores = s ^. playerState & Map.map (^. playerStateScore)

    serveJudgementEvt :: (MonadGame m)
                      => FillBlanksState
                      -> PlayerId
                      -> ClientEvent
                      -> m FillBlanksState
    serveJudgementEvt c p e
        | (isJudge c p) = case e of
            SelectWinner w -> case increaseScoreJudgement c w of
                Just c' -> startRound c'
                Nothing -> do
                    sendPlayer p $ InvalidSend "Judgement doesn't exist"
                    return c
            _ -> do
                sendPlayer p $ InvalidSend "You must select a winner" 
                return c
        | otherwise = do
            sendPlayer p $ InvalidSend "Only the judge may work"
            return c
        
    startRound g = do
        let ng = g & (commonState . commonStateCases .~ mempty)
                   & nextJudge
        let (nc, ng') = nextCall ng
        updateScores ng'
        let sr = StartRound (g ^. commonState . commonStateJudge) nc
        broadcast sr
        return ng'
    

    serveAwaitEvt :: (MonadGame m)
                  => FillBlanksState
                  -> PlayerId
                  -> ClientEvent
                  -> m FillBlanksState
    serveAwaitEvt c p evt = go evt
        where
            go (SubmitJudgement j) = addSubmission c p j
            go _ = do
                sendPlayer p $ InvalidSend "You cannot do that now"
                return c
    
    addSubmission :: (MonadGame m)
                  => FillBlanksState
                  -> PlayerId
                  -> JudgementCase
                  -> m FillBlanksState
    addSubmission s p j
        | s `judgedBy` p = do
            sendPlayer p $ InvalidSend "You are the judge, you cannot submit responses"
            return s
        | s `hasSubmissionFrom` p = do
            sendPlayer p $ InvalidSend "You've already submitted"
            return s
        | otherwise = do
            let updated = addJudgement s p j
            if judgeable updated then do
                broadcast $ 
                    StartJudgement (Map.keys $ updated ^. commonState . commonStateCases)
                return $ 
                    updated & commonState . commonStateStatus .~ AwaitingJudgement
            else
                return updated