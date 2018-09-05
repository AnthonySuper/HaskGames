{-# LANGUAGE TemplateHaskell, DeriveAnyClass, DeriveGeneric #-}
module Game.FillBlanks.Game where

    import GHC.Generics
    import qualified Data.Text as T
    import Control.Lens
    import Data.Aeson
    import qualified Data.Map as Map
    import Game.FillBlanks.Deck
    import Data.Maybe
    import Game.Basic

    data GameStatus
        = AwaitingSubmissions
        | AwaitingJudgement
        deriving (Show, Read, Eq, Ord, Enum, Generic, ToJSON, FromJSON)

    -- | A judgement case is a set of cards to be judged
    -- It only holds an opaque id so that judges cannot cheat
    data JudgementCase
        = JudgementCase
        { _judgementCaseCards :: [ResponseCard] 
        , _judgementCaseId :: Integer
        }
        deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON)
    
    makeLenses ''JudgementCase

    data SelectorState a
        = SelectingCards
        | WaitingJudgement a
        deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)

    makePrisms ''SelectorState

    data JudgeState
        = WaitingJudgements
        | PickingWinner
        deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)

    makePrisms ''JudgeState

    data PlayerStatus a
        = Selector (SelectorState a)
        | Judge JudgeState
        | SittingOut
        deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)

    makePrisms ''PlayerStatus

    -- | The state that belongs exclusively to an individual player
    data PersonalState
        = PersonalState
        { _personalStateHand :: [ResponseCard]
        , _personalStateStatus :: PlayerStatus JudgementCase
        , _personalStateScore :: Int 
        } deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)

    makeLenses ''PersonalState

    data ImpersonalState
        = ImpersonalState
        { _impersonalStateStatus :: PlayerStatus ()
        , _impersonalStateScore :: Int
        } deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)

    makeLenses ''ImpersonalState

    data PublicGame
        = PublicGame
        { _publicGameActivePlayers :: Map.Map PlayerId ImpersonalState
        , _publicGameCurrentCall :: CallCard
        , _publicGameStatus :: GameStatus
        }

    data GameInfo
        = GameInfo
        { _gameInfoDecks :: [T.Text]
        , _gameInfoScores :: Map.Map PlayerId Integer
        }
        deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)

    makeLenses ''GameInfo
    
    personalToImpersonal :: PersonalState -> ImpersonalState
    personalToImpersonal p 
        = ImpersonalState 
            (updateStatus $ p ^. personalStateStatus)
            (p ^. personalStateScore)
        where
            updateStatus :: PlayerStatus JudgementCase -> PlayerStatus ()
            updateStatus (Selector SelectingCards) = Selector SelectingCards
            updateStatus (Selector (WaitingJudgement _)) = Selector (WaitingJudgement ())
            updateStatus (Judge s) = Judge s
            updateStatus SittingOut = SittingOut


    
    