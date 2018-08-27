{-# LANGUAGE TemplateHaskell, DeriveAnyClass, DeriveGeneric #-}
module Game.FillBlanks.ServerState where

    import GHC.Generics
    import qualified Data.Text as T
    import Control.Lens
    import Data.Aeson
    import qualified Data.Map as Map
    import Control.Concurrent.STM.TVar

    data Configuration
        = Configuration
        { _configWinScore :: Integer
        , _configCardCasts :: [String]
        , _configName :: T.Text
        }
        deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)

    