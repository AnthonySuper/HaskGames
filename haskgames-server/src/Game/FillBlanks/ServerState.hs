{-# LANGUAGE TemplateHaskell
           , DeriveAnyClass
           , DeriveGeneric
           , FlexibleContexts
           , RankNTypes
           , NoMonomorphismRestriction #-}
module Game.FillBlanks.ServerState where

    import GHC.Generics
    import qualified Data.Text as T
    import Control.Lens
    import Data.Aeson hiding ((.=))
    import Data.Semigroup
    import qualified Data.Map as Map
    import Game.FillBlanks.Deck
    import Game.FillBlanks.Game
    import Game.Common
    import Data.Maybe
    import Control.Monad.State.Class

    data GameStatus
        = AwaitingSubmissions
        | AwaitingJudgement
        deriving (Show, Read, Eq, Ord, Enum, Generic)

    data Player
        = Player
        { _playerScore :: Integer }
        deriving (Show, Read, Eq, Generic)

    makeLenses ''Player

    data Game
        = Game
        { _gameJudge :: PlayerId
        , _gameWinScore :: Integer
        , _gameFullDeck :: CardDeck
        , _gameCurrentDeck :: CardDeck
        , _gameCases :: Map.Map JudgementCase PlayerId
        , _gameStatus :: GameStatus
        , _gameCurrentCall :: Maybe CallCard
        , _gameActivePlayers :: Map.Map PlayerId Player
        }
        deriving (Show, Read, Eq, Generic)

    makeLenses ''Game

    data GamePublic
        = GamePublic
        { _gamePublicDecks :: [T.Text]
        , _gamePublicScores :: Map.Map PlayerId Integer
        }
        deriving (Show, Read, Eq, Generic)

    makeLenses ''GamePublic

    winner :: Game -> Maybe PlayerId
    winner gs = fst <$> listToMaybe (Map.toList winners)
        where
            winners = Map.filter isWinner (gs ^. gameActivePlayers)
            isWinner ps = (ps ^. playerScore) >= (gs ^. gameWinScore)

    judgementPlayer :: Game -> JudgementCase -> Maybe PlayerId
    judgementPlayer gs c = 
        gs ^. gameCases . at c 

    increaseScore :: Game -> PlayerId -> Game
    increaseScore gs i =
        gs & gameActivePlayers . at i . _Just . playerScore %~ (+1)


    -- | Increase the score of the player with the given Judgement case
    -- If that player no longer exists, or the judgment case is invalid, returns
    -- @Nothing@. This may be made more explicit in the future. 
    increaseScoreJudgement gs j = increaseScore gs <$> judgementPlayer gs j

    addJudgement :: Game -> PlayerId -> JudgementCase -> Game
    addJudgement c p j = c & gameCases . at j .~ Just p

    judgeable :: Game -> Bool
    judgeable s = judgementSize >= (playerSize - 1)
        where
            judgementSize = Map.size $ s ^. gameCases
            playerSize = Map.size $ s ^. gameActivePlayers

    isJudge :: Game -> PlayerId -> Bool
    isJudge c p = (c ^. gameJudge) == p

    judgedBy = isJudge

    hasSubmissionFrom :: Game -> PlayerId -> Bool
    hasSubmissionFrom s p = p `elem` (s ^. gameCases & Map.elems)

    
    clearCases :: MonadState Game m => m ()
    clearCases = gameCases .= mempty


    nextJudge :: MonadState Game m => m ()
    nextJudge = do 
        players <- use gameActivePlayers <&> Map.keys
        judge <- use gameJudge
        let ncand = filter (>= judge) players
        gameJudge .= (headBot ncand players)
        where
            headBot a b = case a of
                [] -> head b
                x:xs -> x 
            
        
    dealCards :: (MonadState Game m) 
              => Int -> m [ResponseCard] 
    dealCards i = let l = gameCurrentDeck . cardDeckResponses in
        (use l <&> take i) <* (l %= drop i)
        
    extractCall :: (MonadState Game m) => m CallCard
    extractCall = do
        let l = gameCurrentDeck . cardDeckCalls
        s <- use l <&> head
        l %= tail 
        return s
    
    playerScores :: Game -> Map.Map PlayerId Integer
    playerScores g = g ^. gameActivePlayers & (Map.map (^. playerScore))