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
    import GameView.FillBlanks.NamePick (namePickView)

    type WsEvent t = Event t [BS.ByteString]

    type WsWorkflow t m = Workflow t m (WsEvent t)

    namePickWorkflow :: (MonadWidget t m)
                     => WebSocket t
                     -> WsWorkflow t m
    namePickWorkflow ws = Workflow inner
        where
            inner = do
                (continue, evt) <- namePickView ws
                return (evt, continue $> pickerWorkflow ws)

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
                     -> WsWorkflow t m
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
        r <- workflow $ namePickWorkflow ws
        return $ switchPromptlyDyn r