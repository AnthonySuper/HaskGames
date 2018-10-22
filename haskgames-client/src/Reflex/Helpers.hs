{-# LANGUAGE OverloadedStrings
           , ScopedTypeVariables
           , RecursiveDo
           , FlexibleContexts
           , NoMonomorphismRestriction
           , TemplateHaskell
           , GADTs
           , AllowAmbiguousTypes #-}

module Reflex.Helpers where 
    import Reflex.Dom
    import Control.Monad (when, (>=>))
    import Control.Lens.Operators
    
    whenDyn :: (MonadWidget t m)
            => Dynamic t Bool
            -> a 
            -> m a
            -> m (Event t a)
    whenDyn filter def act = dyn widgetCreator
        where
            widgetCreator = widgetCreator' <$> filter
            widgetCreator' True = act
            widgetCreator' False = pure def

    dynHold :: (MonadWidget t m)
            => Dynamic t (m (Event t a))
            -> m (Event t a)
    dynHold = dyn >=> switchHold never

    whenDynHold filter def act =
        whenDyn filter def act >>= switchHold never

    tagCurrent = tag . current

    tagValue = tag . current . value

    foldDynAp = foldDyn ($)