{-# LANGUAGE TemplateHaskell, DeriveAnyClass, DeriveGeneric #-}
module Game.FillBlanks.Game where

    import GHC.Generics
    import qualified Data.Text as T
    import Control.Lens
    import Data.Aeson
    import Data.Semigroup

    type TextRep = T.Text

    data CardSource 
        = CardCastDeck T.Text
        | FillIn
        deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

    data ResponseCard 
        = ResponseCard { _responseBody :: TextRep
                       , _responseSource :: CardSource 
                       }
        deriving (Show, Eq, Generic, ToJSON, FromJSON)

    makeLenses ''ResponseCard
    
    data CallCard
        = CallCard { _callBody :: TextRep 
                   , _callArity :: Int
                   , _callSource :: CardSource
                   }
        deriving (Show, Eq, Generic, ToJSON, FromJSON)

    makeLenses ''CallCard

    data CardDeck =
        CardDeck { _cardDeckCalls :: [CallCard]
                 , _cardDeckResponses :: [ResponseCard]
                 }
        deriving (Show, Eq, Generic, ToJSON, FromJSON)
    
    makeLenses ''CardDeck

    instance Semigroup CardDeck where
        (CardDeck c r) <> (CardDeck c' r') 
            = CardDeck (c <> c') (r <> r')

    instance Monoid CardDeck where
        mappend = (<>)
        mempty = CardDeck mempty mempty
        mconcat cs = CardDeck calls responses
            where
                calls = concatMap (view cardDeckCalls) cs
                responses = concatMap (view cardDeckResponses) cs
    
