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

    data CommonState
        = CommonState
        { _commonStateJudge :: PlayerId
        , _commonStateWinScore :: Integer
        , _commonStateDeck :: CardDeck
        }
        deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)
    
    makeLenses ''CommonState

    type FillBlanksState = GameState PlayerState CommonState

    fillBlanksWinner :: FillBlanksState -> Maybe PlayerId
    fillBlanksWinner gs = fst <$> listToMaybe (Map.toList winners)
        where
            winners = Map.filter isWinner (gs ^. playerState)
            winScore = gs ^. commonState . commonStateWinScore
            isWinner ps = (ps ^. playerStateScore) >= winScore
    
    -- | A judgement case is a set of cards to be judged
    -- It only holds an opaque id so that judges cannot cheat
    data JudgementCase
        = JudgementCase
        { _judgementCaseCards :: [ResponseCard] -- | Response cards to be judged
        , _judgementCaseId :: Integer -- | Opaque id to be judged
        }
        deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)

    makeLenses ''JudgementCase

    