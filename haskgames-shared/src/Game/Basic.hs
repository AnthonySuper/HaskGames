{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Game.Basic where
    import GHC.Generics
    import qualified Data.Text as T
    import Data.Aeson
    
    
    type PlayerId = T.Text

    data NameMessage
        = NameMessage { getMsgName :: PlayerId }
        deriving (Show, Eq, Generic, ToJSON, FromJSON)
