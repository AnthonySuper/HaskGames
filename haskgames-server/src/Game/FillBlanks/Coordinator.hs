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

    data Configuration
        = Configuration
        { _configWinScore :: Integer
        , _configCardCasts :: [String]
        , _configName :: T.Text
        }
        deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)
    
    makeLenses ''Configuration

    cardCastsToDeck :: [String] -> IO CardDeck
    cardCastsToDeck s = do
        decks <- mapConcurrently getCardDeck s
        let real = catMaybes decks
        return $ fold real

    createGame :: Configuration -> PlayerId -> IO CommonState
    createGame cfg a = do
        deck <- cardCastsToDeck $ cfg ^. configCardCasts
        let (nc, nd) = dealCallCard deck
        return $ CommonState a (cfg ^. configWinScore) nd mempty AwaitingSubmissions nc