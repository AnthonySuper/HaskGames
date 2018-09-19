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

    data CoordinationItem
        = CoordinationItem 
        {   _coordinationItemBackend :: Backend'
        ,   _coordinationItemCreator :: GameCreator 
        }

    makeLenses ''CoordinationItem

    data Coordination
        = Coordination
        { _coordinationGames :: Map.Map T.Text CoordinationItem
        }
    makeLenses ''Coordination

    cardCoordinator :: TVar Coordination
    cardCoordinator = unsafePerformIO (newTVarIO $ Coordination mempty)

    getGames' :: Coordination -> [Backend']
    getGames' = toListOf $ coordinationGames . traverse . coordinationItemBackend 

    getGames :: IO [Backend']
    getGames 
        = readTVarIO cardCoordinator
        <&> (getGames')


    createGame :: GameCreator -> IO (Game, Backend')
    createGame creator = do
        cards <- cardCastsToDeck (creator ^. gameCreatorDecks)
        backend <- atomically $ do
            coordinator <- readTVar cardCoordinator
            backend <- newBackend
            let item = CoordinationItem backend creator 
            let ns = GameInfo mempty (creator ^. gameCreatorName) 
            writeTVar (backend ^. backendState) $ Just ns
            modifyTVar cardCoordinator $
                coordinationGames %~ Map.insert (creator ^. gameCreatorName) item
            return backend
        let game = Game (creator ^. gameCreatorMaxScore) cards cards mempty
        return (game, backend)

    getInfo :: IO [GameInfo]
    getInfo = getGames
        >>= mapM (readTVarIO . (view backendState))
        <&> catMaybes

    getBackend :: T.Text -> IO (Maybe Backend')
    getBackend i = readTVarIO cardCoordinator
            <&> preview (coordinationGames . at i . _Just . coordinationItemBackend)
        

    cardCastsToDeck :: [T.Text] -> IO CardDeck
    cardCastsToDeck s = do
        decks <- mapConcurrently getCardDeck s
        let real = catMaybes decks
        return $ fold real