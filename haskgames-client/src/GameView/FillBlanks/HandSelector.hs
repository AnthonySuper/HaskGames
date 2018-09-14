{-# LANGUAGE OverloadedStrings
           , ScopedTypeVariables
           , RecursiveDo
           , FlexibleContexts
           , NoMonomorphismRestriction
           , TemplateHaskell
           , GADTs
           , AllowAmbiguousTypes #-}
module GameView.FillBlanks.HandSelector where

    import Reflex.Dom
    import Game.FillBlanks.Game
    import Game.FillBlanks.Event
    import Game.FillBlanks.Deck
    import Control.Lens
    import Data.Functor (($>))

    type CardEvent t = Event t ([ResponseCard] -> [ResponseCard])

    displayResponseCard :: (MonadWidget t m)
                        => ResponseCard
                        -> m (Event t ResponseCard)
    displayResponseCard c = elClass "li" "response-card" $ do
        (e, _) <- el' "h3" $ text (c ^. responseBody)
        return $ domEvent Click e $> c

    responseCardSection :: forall t m. (MonadWidget t m)
                        => Int
                        -> [ResponseCard]
                        -> [ResponseCard]
                        -> m (CardEvent t)
    responseCardSection arity chosen cards
        = do
            k <- elClass "ul" "hand-cards-list" $
                mapM displayResponseCard collection
            return $ 
                ffilter filtering . fmap toAdding $
                    leftmost k
        where
            filtering :: a -> Bool
            filtering = const (arity > (length chosen))
            toAdding :: ResponseCard -> [ResponseCard] -> [ResponseCard]
            toAdding c cs = cs ++ [c]
            collection = filter (not . flip elem chosen) cards