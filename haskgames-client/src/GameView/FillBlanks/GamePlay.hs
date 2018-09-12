{-# LANGUAGE OverloadedStrings
           , ScopedTypeVariables
           , RecursiveDo
           , FlexibleContexts
           , NoMonomorphismRestriction
           , TemplateHaskell
           , GADTs #-}
module GameView.FillBlanks.GamePlay where
    import Reflex.Dom
    import Game.FillBlanks.Game
    import Game.FillBlanks.Event
    import Data.Aeson
    import qualified Data.Text as T
    import Control.Lens
    import Control.Monad.Fix
    import Data.ByteString (ByteString)
    import qualified Data.ByteString.Lazy as BS
    import qualified Data.Map as Map

    gamePlayWidget :: (MonadWidget t m)
                   => WebSocket t
                   -> m (Event t [BS.ByteString])
    gamePlayWidget ws = do
        state <- gameStateDyn $ ws & _webSocket_recv
        e <- dyn $ state <&> gamePlayDisplay
        return never

    gamePlayDisplay Nothing = return ()
    gamePlayDisplay (Just (pg, ps)) = do 
        el "div" $ text "Game play"
        el "div" $ text (T.concat . Map.keys $ pg ^. publicGameActivePlayers )
        
    gameStateDyn :: forall t m. (Reflex t, MonadHold t m, MonadFix m)
                 => Event t ByteString
                 -> m (Dynamic t (Maybe (PublicGame, PersonalState)))
    gameStateDyn e = foldDyn folder Nothing encoded
        where
            folder (UpdateState pg ps) _ = Just (pg, ps)
            folder _ n = n
            encoded :: Event t ServerEvent
            encoded = fmapMaybe decodeStrict e