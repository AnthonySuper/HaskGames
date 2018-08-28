{-# LANGUAGE TemplateHaskell
           , DeriveAnyClass
           , DeriveGeneric
           , OverloadedStrings #-}
module Game.FillBlanks.Event where
    import qualified Data.Text as T
    import qualified Data.Map as Map
    import Game.FillBlanks.Game
    import Game.FillBlanks.Deck
    import GHC.Generics
    import Data.Aeson


    data ServerEvent
        = StartRound T.Text CallCard
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
        deriving (Show, Read, Eq, Generic, FromJSON, ToJSON)
