{-# LANGUAGE TemplateHaskell, DeriveAnyClass, DeriveGeneric #-}
module Game.FillBlanks.Deck where
    import Data.Aeson
    import Control.Lens
    import GHC.Generics
    import qualified Data.Text as T
    import Data.Semigroup

    data CardSource 
        = CardCastDeck T.Text
        | FillIn
        deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON)

    data ResponseCard 
        = ResponseCard { _responseBody :: T.Text
                       , _responseSource :: CardSource 
                       }
        deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON)

    makeLenses ''ResponseCard
    
    data CallCard
        = CallCard { _callBody :: [T.Text]
                   , _callSource :: CardSource
                   }
        deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON)

    makeLenses ''CallCard

    data CardDeck =
        CardDeck { _cardDeckCalls :: [CallCard]
                 , _cardDeckResponses :: [ResponseCard]
                 }
        deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON)
    
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

    dealCallCard :: CardDeck -> (CallCard, CardDeck)
    dealCallCard d = (nc, nd)
        where
            nd = d & cardDeckCalls %~ tail
            nc = d ^. cardDeckCalls & head

    callArity :: CallCard -> Int
    callArity c = length (c ^. callBody)