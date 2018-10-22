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
        = WaitingCases CallCard
        | PickingWinner CallCard [JudgementCase]
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
        } deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)

    makeLenses ''PublicGame 

    data GameInfo
        = GameInfo
        { _gameInfoScores :: Map.Map PlayerId Int
        , _gameInfoName :: T.Text
        }
        deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)

    makeLenses ''GameInfo

    data GameCreator
        = GameCreator
        { _gameCreatorDecks :: [T.Text] 
        , _gameCreatorName :: T.Text
        , _gameCreatorPassword :: Maybe T.Text
        , _gameCreatorMaxScore :: Int 
        } deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)
    
    makeLenses ''GameCreator 

    data CoordinationMessage
        = JoinGame T.Text
        | CreateGame GameCreator
        | ListGames
        deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)

    data CoordinationResponse
        = ReadInfo [GameInfo]
        | JoinedGame
        deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)

    makePrisms ''CoordinationResponse

    judgeCard :: PublicGame -> Maybe CallCard
    judgeCard g = listToMaybe $
        (g ^.. publicGameActivePlayers . traverse . impersonalStateStatus . _Judge . _WaitingCases)
        `mappend`
        (g ^.. publicGameActivePlayers . traverse . impersonalStateStatus . _Judge . _PickingWinner . _1)

    isJudge :: PersonalState -> Bool
    isJudge p = isJust $
        p ^? personalStateStatus . _Judge

    isSittingOut :: PersonalState -> Bool
    isSittingOut p = isJust $
        p ^? personalStateStatus . _SittingOut

    isWaitingJudgement :: PersonalState -> Bool
    isWaitingJudgement p = isJust $
        p ^? personalStateStatus . _Selector . _WaitingJudgement

    canBeJudged a = isSittingOut a || isWaitingJudgement a || isJudge a

    judgementCases :: PublicGame -> [JudgementCase]
    judgementCases g =  
        g ^.. publicGameActivePlayers . traverse . 
            impersonalStateStatus . _Judge . 
            _PickingWinner . _2 . traverse

    
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
