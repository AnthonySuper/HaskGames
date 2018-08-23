
{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}

module Game.Player where
    import GHC.Generics
    import Data.Aeson
    import qualified Data.Text as T
    import Control.Monad.Random.Strict 

    newtype PlayerId = PlayerId T.Text
        deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)


    data Message a
        = UserMessage PlayerId a
        | PlayerDisconnect PlayerId a
        | PlayerConnect PlayerId
        deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

    newtype GameRunner s a
        = GameRunner (s -> a -> Rand StdGen (s, GameRunner s a))

    runGame :: s -> a -> Rand StdGen (s, GameRunner s a)
    runGame s _ = pure (s, GameRunner runGame)
