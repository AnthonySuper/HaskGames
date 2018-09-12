{-# LANGUAGE TemplateHaskell
           , DeriveAnyClass
           , DeriveGeneric
           , FlexibleContexts
           , RankNTypes
           , NoMonomorphismRestriction
           , NamedFieldPuns #-}
module Game.FillBlanks.ServerState where
    
    import Control.Applicative
    import GHC.Generics
    import qualified Data.Text as T
    import Control.Lens
    import Data.Aeson hiding ((.=))
    import Data.Semigroup
    import qualified Data.Map as Map
    import Game.FillBlanks.Deck
    import Game.FillBlanks.Game
    import Game.Basic
    import Game.Common
    import Data.Maybe
    import Control.Monad.State.Class
    import Control.Monad.Except

    data Game
        = Game
        { _gameWinScore :: Int
        , _gameFullDeck :: CardDeck
        , _gameCurrentDeck :: CardDeck
        , _gameActivePlayers :: Map.Map PlayerId PersonalState
        }
        deriving (Show, Read, Eq, Generic)

    makeLenses ''Game

    judgementsMap :: Game -> Map.Map PlayerId JudgementCase
    judgementsMap g =
        g   ^. gameActivePlayers
            & Map.mapMaybe (^? personalStateStatus . _Selector . _WaitingJudgement)

    beingJudged :: Game -> Bool
    beingJudged g = isJust $ 
        g ^? gameActivePlayers . traverse . personalStateStatus . _Judge . _PickingWinner

    winner :: Game -> Maybe PlayerId
    winner gs = fst <$> listToMaybe (Map.toList winners)
        where
            winners = Map.filter isWinner (gs ^. gameActivePlayers)
            isWinner ps = (ps ^. personalStateScore) >= (gs ^. gameWinScore)

    judgementPlayer :: Game -> JudgementCase -> Maybe PlayerId
    judgementPlayer gs c = 
        gs  & judgementsMap
            & Map.toList
            & (^? traverse . filtered ((== c) . (^. _2)) . _1)

    increaseScore :: Game -> PlayerId -> Game
    increaseScore gs i =
        gs & gameActivePlayers . at i . _Just . personalStateScore %~ (+1)

    -- | Increase the score of the player with the given Judgement case
    -- If that player no longer exists, or the judgment case is invalid, returns
    -- @Nothing@. This may be made more explicit in the future. 
    increaseScoreJudgement gs j = increaseScore gs <$> judgementPlayer gs j

    addJudgement :: Game -> PlayerId -> JudgementCase -> Game
    addJudgement c p j = undefined

    addPlayer :: (MonadState Game m)
              => PlayerId -> m ()
    addPlayer pid = do
        s <- get
        case judgeOf s of
            Nothing -> do
                call <- extractCall
                let p = PersonalState [] (Judge $ WaitingCases call) 0
                gameActivePlayers . at pid .= Just p
                return ()
            Just _ -> do
                let p = PersonalState [] (Selector SelectingCards) 0
                gameActivePlayers . at pid .= Just p
                dealCardsTo 6 pid
                return ()

    sitOutPlayer :: (MonadState Game m)
                 => PlayerId -> m ()
    sitOutPlayer pid = do
        s <- get
        if s `judgedBy` pid then do
            moveJudgeStatus >> return ()
        else return ()
        gameActivePlayers . at pid . _Just . personalStateStatus .= SittingOut
        return () 

        
        
    activeJudgementsL
        = gameActivePlayers . traverse . personalStateStatus .
          _Selector . _WaitingJudgement 

    activeJudgements g =
        g ^.. activeJudgementsL

    judgeable :: Game -> Bool
    judgeable g = all canBeJudged cases 
        where
            cases = g ^.. gameActivePlayers . traverse 

    judgeOf :: Game -> Maybe PlayerId
    judgeOf g =
          (g ^. gameActivePlayers & Map.keys)
        & filter (judgedBy g)
        & listToMaybe
            
    judgedBy :: Game -> PlayerId -> Bool
    judgedBy c p = isJust $
            c ^? gameActivePlayers . at p . _Just . personalStateStatus . _Judge

    hasSubmissionFrom :: Game -> PlayerId -> Bool
    hasSubmissionFrom s p = isJust $
        s ^? gameActivePlayers . at p . _Just . personalStateStatus . _Selector . _WaitingJudgement
    
    -- TODO: Make this function put the cards at the bottom of the deck
    setToSelecting :: MonadState Game m => PlayerId -> m ()
    setToSelecting i = 
        gameActivePlayers . at i . _Just . personalStateStatus .= (Selector SelectingCards)
    
    nextTurn :: MonadState Game m => m ()
    nextTurn = do
        call <- moveJudgeStatus
        players <- use gameActivePlayers
        mapM_ (dealNonJudge (call & callArity)) $ Map.keys players

    moveJudgeStatus :: MonadState Game m => m CallCard
    moveJudgeStatus = do 
        call <- extractCall
        nj <- nextJudge <$> get 
        p <-  use gameActivePlayers
        mapM_ setToSelecting (filter (/= nj) $ Map.keys p)
        gameActivePlayers . at nj . _Just . personalStateStatus .= (Judge $ WaitingCases call)
        return call 

    dealNonJudge :: MonadState Game m => Int -> PlayerId -> m ()
    dealNonJudge count id = do
        g <- get
        if g `judgedBy` id then pure () else dealCardsTo count id

    dealCardsTo :: MonadState Game m => Int -> PlayerId -> m ()
    dealCardsTo count id = do
        cards <- dealCards count 
        gameActivePlayers . at id . _Just . personalStateHand %= (<> cards)
        return ()

    -- TODO: Fix 
    nextJudge :: Game -> PlayerId 
    nextJudge g = case currentJudge of
        Just j -> judgeAfter g j
        Nothing -> (g ^. gameActivePlayers & Map.keys & head)
        where
            currentJudge = judgeOf g
    -- TODO: Skip "Sitting out" players
    judgeAfter :: Game -> PlayerId -> PlayerId
    judgeAfter g i = 
        g ^. gameActivePlayers 
        & Map.keys 
        & cycle
        & dropWhile (<= i)
        & (^?! _head)

    dealCards :: (MonadState Game m) 
              => Int -> m [ResponseCard] 
    dealCards i = 
        (use l <&> take i) <* (l %= drop i)
        where
            l = gameCurrentDeck . cardDeckResponses
        
    extractCall :: (MonadState Game m) => m CallCard
    extractCall = (use l <&> head) <* (l %= tail)
        where
            l = gameCurrentDeck . cardDeckCalls
    
    playerScores :: Game -> Map.Map PlayerId Int
    playerScores g = g ^. gameActivePlayers & (Map.map (^. personalStateScore))

    judgementCases :: Game -> [JudgementCase]
    judgementCases g = 
        g ^.. gameActivePlayers . traverse . personalStateStatus . _Selector . _WaitingJudgement 

    toPublicGame :: Game -> PublicGame
    toPublicGame g =
        PublicGame (Map.map personalToImpersonal (g ^. gameActivePlayers))

    startJudgement g
        = g & judgeLens .~ PickingWinner call (activeJudgements g)
        where
            judgeLens = gameActivePlayers . traverse . personalStateStatus . _Judge
            -- TODO: Make safer please
            call = g ^?! judgeLens . _WaitingCases

    gameCurrentCall :: Game -> Maybe CallCard
    gameCurrentCall g
        = g ^? judgeLens . _WaitingCases
            <|> g ^? judgeLens . _PickingWinner . _1 
        where
            judgeLens = gameActivePlayers . traverse . personalStateStatus . _Judge 
