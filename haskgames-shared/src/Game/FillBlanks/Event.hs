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
    import Game.Basic


    data ServerEvent
        = UpdateState PublicGame PersonalState 
        | InvalidSend T.Text
        | ChatMessage PlayerId T.Text
        deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)

    data ClientEvent
        = SubmitJudgement JudgementCase
        | SelectWinner JudgementCase
        | SendChat T.Text 
        deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)
