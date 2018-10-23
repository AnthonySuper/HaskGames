{-# LANGUAGE OverloadedStrings
           , ScopedTypeVariables
           , RecursiveDo
           , FlexibleContexts
           , NoMonomorphismRestriction
           , TemplateHaskell
           , GADTs #-}
module GameView.FillBlanks.NamePick (namePickView) where
    import Reflex.Dom
    import Game.FillBlanks.Game
    import Data.Aeson
    import qualified Data.Text as T
    import Control.Lens
    import Control.Monad.Fix
    import qualified Data.ByteString.Lazy as BS
    import Data.Functor (($>), (<$))
    import Reflex.InputWrapper
    import Reflex.Helpers
    import Data.Aeson.Lens
    import Game.Basic

    namePickView :: (MonadWidget t m)
                 => WebSocket t -> m (Event t (), Event t [BS.ByteString])
    namePickView ws = elClass "div" "name-pick" $ do
        let nameTaken = toNameTaken ws
        let nameAccept = toNameAccept ws
        input <- labeledTextInput "Player Name" "player-name" $
            def &
                textInputConfig_setValue .~ (nameTaken $> "")
        b <- button "Submit"
        return $ (nameAccept, nameToEvt <$> tagValue input b)

    toNameTaken ws = 
        ws & _webSocket_recv
        & fmapMaybe (^? _JSON . _NameTaken)

    toNameAccept ws = ws & _webSocket_recv
        & fmapMaybe (^? _JSON . _NameAccepted)
    
    nameToEvt = pure . encode . NameMessage