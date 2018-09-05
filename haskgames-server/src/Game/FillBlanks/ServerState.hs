{-# LANGUAGE TemplateHaskell, DeriveAnyClass, DeriveGeneric #-}
module Game.FillBlanks.ServerState where

    import GHC.Generics
    import qualified Data.Text as T
    import Control.Lens
    import Data.Aeson
    import Data.Semigroup
    import qualified Data.Map as Map
    import Game.FillBlanks.Deck
    import Game.FillBlanks.Game
    import Game.Common
    import Data.Maybe

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

    nextJudge :: Game -> Game
    nextJudge s = s & gameJudge .~ newJudge
        where
            newJudge = case ncand of
                [] -> head candidates
                x : xs -> x
            currentJudge = s ^. gameJudge
            candidates = Map.keys (s ^. gameActivePlayers)
            ncand = filter (>= currentJudge) candidates
        
    dealCards :: Int -> Game -> ([ResponseCard], Game)
    dealCards i s = (dealt, ns)
        where
            respLens :: Lens' Game [ResponseCard]
            respLens = gameCurrentDeck . cardDeckResponses
            ns = s & respLens %~ (drop i)
            dealt = s ^. respLens & take i

    nextCall :: Game -> (CallCard, Game)
    nextCall s = (nc, ns)
        where
            callLens :: Lens' Game [CallCard]
            callLens = gameCurrentDeck . cardDeckCalls
            ns = s & callLens %~ tail
            nc = s ^. callLens & head

    playerScores :: Game -> Map.Map PlayerId Integer
    playerScores g = g ^. gameActivePlayers & (Map.map (^. playerScore))