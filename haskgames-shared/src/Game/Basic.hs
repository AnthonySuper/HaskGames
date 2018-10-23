{-# LANGUAGE DeriveGeneric, DeriveAnyClass, TemplateHaskell #-}
module Game.Basic where
    import GHC.Generics
    import qualified Data.Text as T
    import Data.Aeson
    import Control.Lens
    
    
    type PlayerId = T.Text

    data NameMessage
        = NameMessage { getMsgName :: PlayerId }
        deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)

    data NameResponse
        = NameTaken
        | NameAccepted
        deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)

    makePrisms ''NameResponse