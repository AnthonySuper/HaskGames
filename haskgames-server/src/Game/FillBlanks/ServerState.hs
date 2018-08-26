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

    data CommonState
        = CommonState
        { _commonStateJudge :: PlayerId
        , _commonStateWinScore :: Integer
        , _commonStateDeck :: CardDeck
        , _commonStateCases :: Map.Map JudgementCase PlayerId
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