{-# LANGUAGE OverloadedStrings
           , ScopedTypeVariables
           , RecursiveDo
           , FlexibleContexts
           , NoMonomorphismRestriction
           , TemplateHaskell
           , GADTs #-}
module GameView.FillBlanks.Main where
    import Reflex.Dom
    import Game.FillBlanks.Game
    import Data.Aeson
    import qualified Data.Text as T
    import Control.Lens
    import Control.Monad.Fix
    import qualified Data.ByteString.Lazy as BS
    import Reflex.Workflow
    import GameView.FillBlanks.GamePicker (gamePickerWidget, encodeCoordination)
    import GameView.FillBlanks.GamePlay (gamePlayWidget)
    import Data.Functor (($>), (<$))

    type WsEvent t = Event t [BS.ByteString]

    type WsWorkflow t m = Workflow t m (WsEvent t)

    pickerWorkflow :: forall t m. 
                    ( MonadWidget t m
                    , DomBuilder t m
                    , PostBuild t m )
                   => WebSocket t
                   -> WsWorkflow t m
    pickerWorkflow ws = Workflow inner
        where
            inner = do
                evt <- gamePickerWidget ws
                return (evt, wsEvent)
            coordEvent = encodeCoordination (ws & _webSocket_recv)
            switchEvent = fmapMaybe (^? _JoinedGame) coordEvent
            wsEvent = switchEvent $> gamePlayWorkflow ws
            
    gamePlayWorkflow :: forall t m. (MonadWidget t m, DomBuilder t m, PostBuild t m)
                     => WebSocket t
                     -> Workflow t m (Event t [BS.ByteString])
    gamePlayWorkflow ws = Workflow . el "div" $ do
        g <- gamePlayWidget ws 
        return (g, never)

    fullWorkflow :: ( Reflex t
                    , Adjustable t m
                    , MonadFix m
                    , MonadHold t m
                    , DomBuilder t m
                    , PostBuild t m
                    , MonadWidget t m
                    )
                 => WebSocket t
                 -> m (Event t [BS.ByteString])
    fullWorkflow ws = do
        r <- workflow $ pickerWorkflow ws
        return $ switchPromptlyDyn r