{-# LANGUAGE TemplateHaskell, DeriveAnyClass, DeriveGeneric #-}
module Game.FillBlanks.Coordinator where

    import GHC.Generics
    import qualified Data.Text as T
    import Control.Lens
    import Data.Aeson
    import qualified Data.Map as Map
    import Control.Concurrent.STM.TVar
    import Control.Concurrent.Async (mapConcurrently)
    import Game.FillBlanks.CardCast
    import Game.FillBlanks.Deck
    import Game.Common
    import Game.FillBlanks.ServerState
    import Data.Foldable
    import Data.Maybe

    cardCastsToDeck :: [String] -> IO CardDeck
    cardCastsToDeck s = do
        decks <- mapConcurrently getCardDeck s
        let real = catMaybes decks
        return $ fold real