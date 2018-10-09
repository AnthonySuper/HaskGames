{-# LANGUAGE OverloadedStrings
           , ScopedTypeVariables
           , RecursiveDo
           , FlexibleContexts
           , NoMonomorphismRestriction
           , TemplateHaskell
           , GADTs #-}
module GameView.FillBlanks.NamePick where
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
    import Game.Basic

    namePickView :: (MonadWidget t m)
                 => m (Event t [BS.ByteString])
    namePickView = elClass "div" "name-pick" $ do
        input <- labeledTextInput "Player Name" "player-name" def
        b <- button "Submit"
        return $ nameToEvt <$> tagValue input b 
    
    nameToEvt = pure . encode . NameMessage