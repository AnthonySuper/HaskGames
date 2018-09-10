{-# LANGUAGE TemplateHaskell, DeriveAnyClass, DeriveGeneric, LambdaCase #-}
module Game.FillBlanks.Coordinator where

    import GHC.Generics
    import qualified Data.Text as T
    import Control.Lens
    import Data.Aeson
    import qualified Data.Map as Map
    import Control.Concurrent.STM
    import Control.Concurrent.STM.TVar
    import Control.Concurrent.Async (mapConcurrently)
    import Game.FillBlanks.CardCast
    import Game.FillBlanks.Deck
    import Game.Common
    import Game.FillBlanks.ServerState
    import Data.Foldable
    import Data.Maybe
    import Network.WebSockets
    import Game.Backend.TChan
    import Game.FillBlanks.Game
    import Game.FillBlanks.Event
    import Game.FillBlanks.Server
    import Control.Concurrent
    import System.IO.Unsafe (unsafePerformIO)
    import Control.Monad

    type Backend' = Backend ServerEvent ClientEvent GameInfo

    data Coordination
        = Coordination
        { _coordinationGames :: [Backend']
        , _coordinationCounter :: Int
        }
    makeLenses ''Coordination

    cardCoordinator :: TVar Coordination
    cardCoordinator = unsafePerformIO (newTVarIO $ Coordination [] 0)

    getGames :: IO [Backend']
    getGames 
        = readTVarIO cardCoordinator
        <&> (^. coordinationGames)

    createGame :: [String] -> IO (Game, Backend')
    createGame decks = do 
        cards <- cardCastsToDeck decks
        backend <- atomically $ do
            coordinator <- readTVar cardCoordinator
            backend <- newBackend
            let ns = GameInfo (map T.pack decks) mempty (coordinator ^. coordinationCounter) 
            writeTVar (backend ^. backendState) $ Just ns
            modifyTVar cardCoordinator (coordinationCounter %~ (+1))
            modifyTVar cardCoordinator (coordinationGames %~ (backend :))
            return backend
        let game = Game 10 cards cards mempty
        return (game, backend)


    getInfo :: IO [GameInfo]
    getInfo 
        = (^. coordinationGames) <$> readTVarIO cardCoordinator
        >>= mapM (readTVarIO . (^. backendState))
        <&> catMaybes

    getBackend :: Int -> IO (Maybe Backend')
    getBackend i = do
        coordinator <- readTVarIO cardCoordinator
        withId <- filterM (hasId i) (coordinator ^. coordinationGames)
        return $ listToMaybe withId 

    hasId :: Int -> Backend' -> IO Bool
    hasId i x = do 
        r <- readTVarIO (x ^. backendState)
        let id' = r <&> (^. gameInfoId)
        return $ id' == Just i
    
    cardCastsToDeck :: [String] -> IO CardDeck
    cardCastsToDeck s = do
        decks <- mapConcurrently getCardDeck s
        let real = catMaybes decks
        return $ fold real