{-# LANGUAGE OverloadedStrings
           , ScopedTypeVariables
           , RecursiveDo
           , FlexibleContexts
           , NoMonomorphismRestriction
           , TemplateHaskell
           , GADTs
           , AllowAmbiguousTypes #-}

module Reflex.InputWrapper where 
    import Reflex.Dom
    import Control.Monad (when, (>=>))
    import Control.Lens
    import Data.Maybe (maybe)
    import qualified Data.Text as T
    import Text.Read (readMaybe)
    import Data.Monoid

    labledInput input label name cfg = elClass "div" "pure-control-group" $ do
        let labelAts = ("for" =: name)
        let fieldAts = ("name" =: name)
        elAttr "label" labelAts $ text label
        input $ cfg &
            attributes %~ (mappend $ pure fieldAts)

    
    pureButtonClass text' klass = do
        (el, _) <- elAttr' "a"
            ("class" =: (T.append "pure-button " klass) <> "href" =: "#")
            $ text text'
        return $ domEvent Click el 

    pureButton text' = pureButtonClass text' ""

    labeledTextInput = labledInput textInput

    readMaybeDefault def str = maybe def id (readMaybe str)

    valueAsDefault def v = readMaybeDefault def <$> T.unpack <$> value v
    