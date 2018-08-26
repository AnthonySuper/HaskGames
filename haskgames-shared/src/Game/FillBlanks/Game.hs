{-# LANGUAGE TemplateHaskell, DeriveAnyClass, DeriveGeneric #-}
module Game.FillBlanks.Game where

    import GHC.Generics
    import qualified Data.Text as T
    import Control.Lens
    import Data.Aeson
    import Data.Semigroup
    import qualified Data.Map as Map
    import Game.FillBlanks.Deck
    import Game.Common
    import Data.Maybe

    data PlayerState
        = PlayerState 
        { _playerStateScore :: Integer
        }
        deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)
    
    makeLenses ''PlayerState

    -- | A judgement case is a set of cards to be judged
    -- It only holds an opaque id so that judges cannot cheat
    data JudgementCase
        = JudgementCase
        { _judgementCaseCards :: [ResponseCard] -- | Response cards to be judged
        , _judgementCaseId :: Integer -- | Opaque id to be judged
        }
        deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON)

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
    increaseJudgementScore :: FillBlanksState -> JudgementCase -> Maybe FillBlanksState
    increaseJudgementScore gs j = increaseScore gs <$> judgementPlayer gs j

    increaseJudgementScore' gs j = fromMaybe gs $
        increaseJudgementScore gs j

    makeLenses ''JudgementCase

    