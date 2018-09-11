{-# LANGUAGE 
          OverloadedStrings
        , RecordWildCards 
#-}
module Game.FillBlanks.CardCast.Internal where

    import GHC.Generics
    import Data.Aeson
    import Data.Aeson.Types
    import qualified Data.Text as T
    import Data.Maybe (listToMaybe)
    import Control.Monad
    import Game.FillBlanks.Deck
    import qualified Network.Wreq as Req
    import Control.Lens

    type TextRep = T.Text

    data Call = Call [T.Text] T.Text
        deriving (Show, Eq)

    data Response = Response T.Text T.Text
        deriving (Show, Eq)

    data Document 
        = Document { _deckCalls :: [Call]
                   , _deckResponses :: [Response]
                   }
        deriving (Show, Eq)

    instance FromJSON Call where
        parseJSON (Object o) =
            Call <$> bodyA <*> id
            where
                bodyA :: Parser [T.Text] 
                bodyA = o .: "text"
                id = o .: "id"
        parseJSON _ = mzero

    instance FromJSON Response where
        parseJSON (Object o) =
            Response <$> body <*> id
            where 
                body' = listToMaybe <$> o .: "text"
                body = body' >>= maybe mzero pure
                id = o .: "id"
        parseJSON _ = mzero

    instance FromJSON Document where
        parseJSON (Object o) =
            Document <$> o .: "calls" <*> o .: "responses"
        parseJSON _ = mzero

    callToGame :: Call -> CallCard
    callToGame (Call body id)
        = CallCard body $ CardCastDeck id

    responseToGame :: Response -> ResponseCard
    responseToGame (Response body id)
        = ResponseCard body (CardCastDeck id)

    deckToGame :: Document -> CardDeck
    deckToGame Document{..} = 
        CardDeck (callToGame <$> _deckCalls)
                 (responseToGame <$> _deckResponses)
    
    parseCardCast_ p bs = deckToGame <$> p bs

    parseCardCast = parseCardCast_ decode
    parseCardCast' = parseCardCast_ decode'

    cardsUrl s = concat ["https://api.cardcastgame.com/v1/decks/", s, "/cards"]

    getCardDeck :: String -> IO (Maybe CardDeck)
    getCardDeck id = do
        t <- Req.get $ cardsUrl id
        return $ parseCardCast' (t ^. Req.responseBody)
