{-# LANGUAGE TemplateHaskell
           , DeriveGeneric
           , OverloadedStrings
           , DeriveAnyClass #-}
module Game.Common where
    import GHC.Generics
    import qualified Data.Text as T
    import qualified Data.Map as M
    import Control.Lens
    import Data.Aeson


    type PlayerId = T.Text

    data GameState playerState commonState
        = GameState
        { _playerState :: M.Map PlayerId playerState
        , _commonState :: commonState
        }
        deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)
    
    makeLenses ''GameState

    hasPlayer :: PlayerId -> GameState a b -> Bool
    hasPlayer i g = M.member i (g ^. playerState)