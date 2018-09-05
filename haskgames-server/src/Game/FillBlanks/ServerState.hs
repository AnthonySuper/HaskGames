{-# LANGUAGE TemplateHaskell
           , DeriveAnyClass
           , DeriveGeneric
           , FlexibleContexts
           , RankNTypes
           , NoMonomorphismRestriction
           , NamedFieldPuns #-}
module Game.FillBlanks.ServerState where

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


    data Game
        = Game
        { _gameWinScore :: Int
        , _gameFullDeck :: CardDeck
        , _gameCurrentDeck :: CardDeck
        , _gameStatus :: GameStatus
        , _gameCurrentCall :: Maybe CallCard
        , _gameActivePlayers :: Map.Map PlayerId PersonalState
        }
        deriving (Show, Read, Eq, Generic)

    makeLenses ''Game

    winner :: Game -> Maybe PlayerId
    winner gs = fst <$> listToMaybe (Map.toList winners)
        where
            winners = Map.filter isWinner (gs ^. gameActivePlayers)
            isWinner ps = (ps ^. personalStateScore) >= (gs ^. gameWinScore)

    judgementPlayer :: Game -> JudgementCase -> Maybe PlayerId
    judgementPlayer gs c = 
        gs  & activeJudgements
            & Map.toList
            & filter ((== c) . (^. _2))
            & (^? _head . _1)

    increaseScore :: Game -> PlayerId -> Game
    increaseScore gs i =
        gs & gameActivePlayers . at i . _Just . personalStateScore %~ (+1)


    -- | Increase the score of the player with the given Judgement case
    -- If that player no longer exists, or the judgment case is invalid, returns
    -- @Nothing@. This may be made more explicit in the future. 
    increaseScoreJudgement gs j = increaseScore gs <$> judgementPlayer gs j

    addJudgement :: Game -> PlayerId -> JudgementCase -> Game
    addJudgement c p j = undefined

    activeJudgements g =
        g ^. gameActivePlayers
            & Map.mapMaybe (^? personalStateStatus . _Selector . _WaitingJudgement)

    judgeable :: Game -> Bool
    judgeable s = judgementSize >= (playerSize - 1)
        where
            judgementSize = undefined
            playerSize = Map.size $ s ^. gameActivePlayers

    judgeOf g = 
        g   ^. gameActivePlayers
            & Map.filterWithKey (\x _ -> isJudge g x)
            & Map.toList
            & (^?! _head . _1)
            
    isJudge :: Game -> PlayerId -> Bool
    isJudge c p = isJust $
            c ^? gameActivePlayers . at p . _Just . personalStateStatus . _Judge

    judgedBy = isJudge

    hasSubmissionFrom :: Game -> PlayerId -> Bool
    hasSubmissionFrom s p = isJust $
        s ^? gameActivePlayers . at p . _Just . personalStateStatus . _Selector . _WaitingJudgement
    
    -- TODO: Make this function put the cards at the bottom of the deck
    setToSelecting :: MonadState Game m => PlayerId -> m ()
    setToSelecting i = 
        gameActivePlayers . at i . _Just . personalStateStatus .= (Selector SelectingCards)
    
    nextTurn :: MonadState Game m => m ()
    nextTurn = do
        moveJudgeStatus
        call <- extractCall 
        gameCurrentCall .= Just call
        players <- use gameActivePlayers
        mapM_ (dealNonJudge (call ^. callArity)) $ Map.keys players

    moveJudgeStatus :: MonadState Game m => m ()
    moveJudgeStatus = do 
        nj <- judgeAfter <$> get <*> (judgeOf <$> get)
        p <-  use gameActivePlayers
        mapM_ setToSelecting (filter (/= nj) $ Map.keys p)
        gameActivePlayers . at nj . _Just . personalStateStatus .= (Judge $ WaitingJudgements)

    dealNonJudge :: MonadState Game m => Int -> PlayerId -> m ()
    dealNonJudge count id = do
        g <- get
        if g `judgedBy` id then pure () else dealCardsTo count id

    dealCardsTo :: MonadState Game m => Int -> PlayerId -> m ()
    dealCardsTo count id = do
        cards <- dealCards count 
        gameActivePlayers . at id . _Just . personalStateHand %= (<> cards)
        return ()
    
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
            (fromJust (g ^. gameCurrentCall))
            (g ^. gameStatus)
            (judgementCases g)