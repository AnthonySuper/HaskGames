{-# LANGUAGE TemplateHaskell, DeriveAnyClass, DeriveGeneric #-}
module Game.FillBlanks.Deck where
    import Data.Aeson
    import Control.Lens
    import GHC.Generics
    import qualified Data.Text as T
    import Data.Semigroup

    -- | This type represents where this card came from
    data CardSource 
        = CardCastDeck T.Text
        -- ^ This card is from a CardCast deck and has the given ID
        | FillIn
        -- ^ This card was filled in by the player
        deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON)

    -- | A "response" Card, which fills in a single blank 
    data ResponseCard 
        = ResponseCard { _responseBody :: T.Text
                        -- ^ The body of the response
                       , _responseSource :: CardSource
                       -- ^ Where this card came from 
                       }
        deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON)

    makeLenses ''ResponseCard
    
    -- | A "call" Card, which has one or more blanks for the user to fill in
    data CallCard
        = CallCard { _callBody :: [T.Text]
                    -- ^ The body of the call, broken up by spots to fill in
                   , _callSource :: CardSource
                   -- ^ Where this call came from
                   }
        deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON)

    makeLenses ''CallCard

    -- | A "Deck" consisting of several calls and several responses
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

    -- | Deal a call card, returning a tuple of that card, and the deck without that card
    dealCallCard :: CardDeck -> (CallCard, CardDeck)
    dealCallCard d = (nc, nd)
        where
            nd = d & cardDeckCalls %~ tail
            nc = d ^. cardDeckCalls & head
    
    -- | How many spots to fill in does this 'CallCard' have?
    callArity :: CallCard -> Int
    callArity c = length (c ^. callBody)