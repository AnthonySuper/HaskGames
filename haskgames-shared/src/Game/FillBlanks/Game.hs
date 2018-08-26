{-# LANGUAGE TemplateHaskell, DeriveAnyClass, DeriveGeneric #-}
module Game.FillBlanks.Game where

    import GHC.Generics
    import qualified Data.Text as T
    import Control.Lens
    import Data.Aeson
    import qualified Data.Map as Map
    import Game.FillBlanks.Deck
    import Data.Maybe

    -- | A judgement case is a set of cards to be judged
    -- It only holds an opaque id so that judges cannot cheat
    data JudgementCase
        = JudgementCase
        { _judgementCaseCards :: [ResponseCard] -- | Response cards to be judged
        , _judgementCaseId :: Integer -- | Opaque id to be judged
        }
        deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON)
    
    makeLenses ''JudgementCase

  