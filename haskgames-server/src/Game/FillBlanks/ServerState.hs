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

    data PlayerState
        = PlayerState 
        { _playerStateScore :: Integer
        }
        deriving (Show, Read, Eq, Generic)
    
    makeLenses ''PlayerState

    data GameStatus
        = AwaitingSubmissions
        | AwaitingJudgement
        deriving (Show, Read, Eq, Ord, Enum, Generic)

    data CommonState
        = CommonState
        { _commonStateJudge :: PlayerId
        , _commonStateWinScore :: Integer
        , _commonStateDeck :: CardDeck
        , _commonStateCases :: Map.Map JudgementCase PlayerId
        , _commonStateStatus :: GameStatus
        , _commonStateCurrentCall :: CallCard
        }
        deriving (Show, Read, Eq, Generic)

    makeLenses ''CommonState

    type FillBlanksState = GameState PlayerState CommonState

    winner :: FillBlanksState -> Maybe PlayerId
    winner gs = fst <$> listToMaybe (Map.toList winners)
        where
            winners = Map.filter isWinner (gs ^. playerState)
            winScore = gs ^. commonState . commonStateWinScore
            isWinner ps = (ps ^. playerStateScore) >= winScore

    judgementPlayer :: FillBlanksState -> JudgementCase -> Maybe PlayerId
    judgementPlayer gs c = 
        gs ^. commonState . commonStateCases . at c

    -- | Increase the score of the player with the given id
    -- Note: Silently does nothing if said player doesn't exist
    increaseScore :: FillBlanksState -> PlayerId -> FillBlanksState
    increaseScore g i =
        playerState . at i . _Just . playerStateScore %~ (+1) $ g

    -- | Increase the score of the player with the given Judgement case
    -- If that player no longer exists, or the judgment case is invalid, returns
    -- @Nothing@. This may be made more explicit in the future. 
    increaseScoreJudgement :: FillBlanksState -> JudgementCase -> Maybe FillBlanksState
    increaseScoreJudgement gs j = increaseScore gs <$> judgementPlayer gs j

    addJudgement :: FillBlanksState -> PlayerId -> JudgementCase -> FillBlanksState
    addJudgement c p j = c & commonState . commonStateCases . at j .~ Just p

    judgeable :: FillBlanksState -> Bool
    judgeable s = judgementSize >= (playerSize - 1)
        where
            judgementSize = Map.size (s ^. commonState . commonStateCases)
            playerSize = Map.size (s ^. playerState)

    isJudge :: FillBlanksState -> PlayerId -> Bool
    isJudge c p = (c ^. commonState . commonStateJudge) == p

    judgedBy = isJudge

    hasSubmissionFrom :: FillBlanksState -> PlayerId -> Bool
    hasSubmissionFrom s p = p `elem` (s ^. commonState . commonStateCases & Map.elems)

    nextJudge :: FillBlanksState -> FillBlanksState
    nextJudge s = s & commonState . commonStateJudge .~ newJudge
        where
            newJudge = case ncand of
                [] -> head candidates
                x : xs -> x
            currentJudge = s ^. commonState . commonStateJudge
            candidates = Map.keys (s ^. playerState)
            ncand = filter (>= currentJudge) candidates