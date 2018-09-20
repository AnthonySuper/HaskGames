{-# LANGUAGE TemplateHaskell
           , DeriveAnyClass
           , DeriveGeneric
           , FlexibleContexts
           , RankNTypes
           , NoMonomorphismRestriction
           , NamedFieldPuns #-}
{-|
Module      : Game.FillBlanks.ServerState
Description : State management for a Fill-in-the-blanks type of game
Copyright   : (c) Anthony Super, 2018
License     : AGPL-3
Stability   : experimental
Portability : POSIX

This module contains the definition of the server state for a
"Fill in the blank" type of game, such as @Cards Against Humanity@
or @Apples to Apples.@
-}
module Game.FillBlanks.ServerState where
    
    import Control.Applicative
    import GHC.Generics
    import qualified Data.Text as T
    import Control.Lens
    import Data.Aeson hiding ((.=))
    import Data.Semigroup
    import qualified Data.Map as Map
    import Game.FillBlanks.Deck
    import Game.FillBlanks.Game
    import Game.Basic
    import Game.Common
    import Data.Maybe
    import Control.Monad.State.Class
    import Control.Monad.Except
    import Data.List ((\\))
    import Control.Monad.State.Strict
    import Data.Foldable (concat)
    import Control.Monad (unless)

    -- | Game state for a "Fill in the blanks" sort of game
    data Game
        = Game
        { _gameWinScore :: Int 
        -- ^ The score at which a player is considered to have won the game
        , _gameFullDeck :: CardDeck
        -- ^ The full deck of all the cards in the game
        , _gameCurrentDeck :: CardDeck
        -- ^ The current deck of all the cards in the game
        , _gamePlayers :: Map.Map PlayerId PersonalState
        -- ^ State for all the players in the game
        }
        deriving (Show, Read, Eq, Generic)

    makeLenses ''Game

    needInput :: Game -> [PlayerId]
    needInput = Map.keys . Map.filter needInput' . view gamePlayers
        where
            needInput' v = case v ^. personalStateStatus of
                Selector SelectingCards -> True
                Judge (PickingWinner _ _) -> True
                _ -> False

    -- | Helper Prism for a player with the given id
    playerAt :: T.Text -> Lens' Game (Maybe PersonalState)
    playerAt i = gamePlayers . at i

    -- | Helper traversal for the status of all the players
    traverseStatuses :: Traversal' Game (PlayerStatus JudgementCase)
    traverseStatuses = gamePlayers . traverse . personalStateStatus 

    -- | Is the game contained in the state being judged?
    -- See 'beingJudged'
    beingJudgedM :: (MonadState Game m) => m Bool
    beingJudgedM = do 
        s <- preuse $ traverseStatuses . _Judge . _PickingWinner
        return $ isJust s 

    -- | Is this game being judged?
    -- (IE, is there a Judge who is selecting a JudgementCase to be the winner of a round?)
    beingJudged :: Game -> Bool
    beingJudged g = isJust $ 
        g ^? traverseStatuses . _Judge . _PickingWinner

    -- | Extract the winner of a game if there is one, or 'Nothing' if there isn't. 
    winner :: Game -> Maybe PlayerId
    winner gs = fst <$> listToMaybe (Map.toList winners)
        where
            winners = Map.filter isWinner (gs ^. gamePlayers)
            isWinner = (>= gs ^. gameWinScore) . (^. personalStateScore)

    -- | Get the PlayerID for a given judgement
    judgementPlayer :: Game -> JudgementCase -> Maybe PlayerId
    judgementPlayer gs c = 
        listToMaybe . Map.keys $ Map.filter isCase (gs ^. gamePlayers)
        where
            caseFor :: PersonalState -> Maybe JudgementCase
            caseFor ps = ps ^? personalStateStatus . _Selector . _WaitingJudgement 
            isCase :: PersonalState -> Bool
            isCase ps = Just c == caseFor ps

    -- | Increase the score of a given player by one
    increaseScore :: Game -> PlayerId -> Game
    increaseScore gs i =
        gs & playerAt i . _Just . personalStateScore %~ (+1)

    -- | Increase the score of the player with the given Judgement case
    -- If that player no longer exists, or the judgment case is invalid, returns
    -- @Nothing@. This may be made more explicit in the future. 
    increaseScoreJudgement gs j = increaseScore gs <$> judgementPlayer gs j

    -- | Stateful version of 'increaseScoreJudgement'
    increaseScoreJudgementM :: (MonadState Game m)
                            => JudgementCase -> m Bool
    increaseScoreJudgementM jc = do
        g <- get
        let ng = increaseScoreJudgement g jc
        case ng of
            Just ng' -> do
                put ng'
                return True
            Nothing -> return False 

    -- | Stateful version of 'addJudgement'
    addJudgementM pid jc = do 
        g <- get
        put $ addJudgement g pid jc 

    -- | Add this JudgementCase as the JudgementCase for the given player
    addJudgement :: Game -> PlayerId -> JudgementCase -> Game
    addJudgement c p j = 
        c & playerAt p . _Just . personalStateStatus .~ status
        where
            status = Selector $ WaitingJudgement j 

    -- | Add a new player to the state.
    -- | This also deals them cards, which is nice.
    addPlayer :: (MonadState Game m)
              => PlayerId -> m ()
    addPlayer pid = do
        s <- get
        case judgeOf s of
            Nothing -> do
                call <- extractCall
                let p = PersonalState [] (Judge $ WaitingCases call) 0
                playerAt pid .= Just p
                dealCardsTo 6 pid 
                return ()
            Just _ -> do
                let p = PersonalState [] (Selector SelectingCards) 0
                playerAt pid .= Just p
                dealCardsTo 6 pid
                return ()

    -- | Convert a player to the "sitting out" status.
    -- If that player was previously a judge, it ends the round
    -- in a logical way.
    sitOutPlayer :: (MonadState Game m)
                 => PlayerId -> m ()
    sitOutPlayer pid = do
        s <- get
        if s `judgedBy` pid then do
            moveJudgeStatus >> return ()
        else return ()
        playerAt pid . _Just . personalStateStatus .= SittingOut
        return ()
        
    -- | A prism for all of the JudgementCases that are awaiting Judgement
    activeJudgementsL
        = traverseStatuses .
          _Selector . _WaitingJudgement 

    -- | Get all the active judgements for a game
    activeJudgements g =
        g ^.. activeJudgementsL

    -- | 'judgeable' in the State monad
    judgeableM = judgeable <$> get 

    -- | Is this game able to be judged?
    -- IE, is every non-judge player either AwaitingJudgement or SittingOut?
    judgeable :: Game -> Bool
    judgeable g = all canBeJudged cases 
        where
            cases = g ^.. gamePlayers . traverse 

    -- | Get the playerID for the judge of this game
    judgeOf :: Game -> Maybe PlayerId
    judgeOf g =
          (g ^. gamePlayers & Map.keys)
        & filter (judgedBy g)
        & listToMaybe
            
    -- | 'judgedBy' in the State monad
    judgedByM :: (MonadState Game m) => PlayerId -> m Bool 
    judgedByM = gets . flip judgedBy

    -- | Does this player judge the game?
    judgedBy :: Game -> PlayerId -> Bool
    judgedBy c p = isJust $
            c ^? playerAt p . _Just . personalStateStatus . _Judge

    -- | 'hasSubmissionFrom' in the State monad
    hasSubmissionM :: (MonadState Game m) => PlayerId -> m Bool
    hasSubmissionM p = flip hasSubmissionFrom p <$> get

    -- | Does this game have an active submission from the given player?
    hasSubmissionFrom :: Game -> PlayerId -> Bool
    hasSubmissionFrom s p = isJust $
        s ^? playerAt p . _Just . personalStateStatus . _Selector . _WaitingJudgement
    
    -- | Update a given player's status in the State monad
    changeStatus :: MonadState Game m
                 => PlayerId 
                 -> PlayerStatus JudgementCase 
                 -> m ()
    changeStatus id s =
        playerAt id . _Just . personalStateStatus .= s 

    -- | Set a player to be Selecting cards. 
    setToSelecting :: MonadState Game m => PlayerId -> m ()
    setToSelecting i = changeStatus i (Selector SelectingCards)
    
    -- | Move the game to the next turn, in the state monad
    nextTurn :: MonadState Game m => m ()
    nextTurn = do
        returnCardsToDeck
        call <- moveJudgeStatus
        players <- use gamePlayers
        mapM_ (dealNonJudge (call & callArity)) $ Map.keys players

    -- | Take all the cards in active judgement cases and put them
    -- on the "bottom" of the deck.
    -- Also returns the active "Call" card to the bottom of the deck.
    returnCardsToDeck :: MonadState Game m => m ()
    returnCardsToDeck = do
        resps <- gamePlayers . traverse %%= (runState removeJudgementCalls)
        gameCurrentDeck . cardDeckResponses %= (<> resps)
        s <- get
        let call = s ^.. gamePlayers . traverse . 
                personalStateStatus . _Judge . _PickingWinner . _1
        gameCurrentDeck . cardDeckCalls %= (<> call) 
        return ()

    -- | Given a player that has a 'JudgementCase' they are awaiting judgement on,
    -- this function removes the cards in that 'JudgementCase' from their hand.
    removeJudgementCalls :: MonadState PersonalState m
                         => m [ResponseCard]
    removeJudgementCalls = do
        s <- use personalStateStatus
        let resp = s ^.. _Selector . _WaitingJudgement . judgementCaseCards . traverse
        personalStateHand %= (\\ resp)
        return resp

    -- | Move the judge slot to the next player in line.
    -- This returns the 'CallCard' that is the basis of the next round of judgement
    moveJudgeStatus :: MonadState Game m => m CallCard
    moveJudgeStatus = do 
        call <- extractCall
        nj <- nextJudge <$> get 
        p <-  use gamePlayers
        mapM_ (liftM2 unless (nj ==) setToSelecting) $ Map.keys p
        changeStatus nj (Judge $ WaitingCases call)
        return call 

    -- | Deal the given number of cards to the given player, provided
    -- that said player is not a judge
    dealNonJudge :: MonadState Game m => Int -> PlayerId -> m ()
    dealNonJudge count id = do
        g <- get
        unless (g `judgedBy` id) $
            dealCardsTo count id

    -- | Deal the given number of cards to the given player,
    -- removing them from the active deck at the same time
    dealCardsTo :: MonadState Game m => Int -> PlayerId -> m ()
    dealCardsTo count id = do
        cards <- dealCards count 
        playerAt id . _Just . personalStateHand %= (<> cards)
        return ()

    -- | Given a game, find out which player will be judge next
    nextJudge :: Game -> PlayerId 
    nextJudge g = 
        maybe 
            (g ^. gamePlayers & Map.keys & head)
            (judgeAfter g)
            $ judgeOf g 
    
    -- | Given a game and a player, find out which player is next in line
    -- to be judged after that player.
    -- __TODO__: Ignore "sitting out" players, who cannot become Judges until
    -- they no longer sit out.
    judgeAfter :: Game -> PlayerId -> PlayerId
    judgeAfter g i = head (afterCandidate <> keys)
        where
            keys = g ^. gamePlayers & Map.keys
            afterCandidate =
                keys
                & dropWhile (<= i)

    -- | Remove the given number of 'ResponseCard's from the game's deck
    -- and return them
    dealCards :: (MonadState Game m) 
              => Int -> m [ResponseCard] 
    dealCards i = 
        (use l <&> take i) <* (l %= drop i)
        where
            l = gameCurrentDeck . cardDeckResponses
        
    -- | Remove a 'CallCard' from the game's deck and return it
    extractCall :: (MonadState Game m) => m CallCard
    extractCall = (use l <&> head) <* (l %= tail)
        where
            l = gameCurrentDeck . cardDeckCalls
    
    -- | Conver the players to a map of 'PlayerId' -> score
    playerScores :: Game -> Map.Map PlayerId Int
    playerScores g = g ^. gamePlayers & (Map.map (^. personalStateScore))

    -- | Get all JudgementCases being considered in this game
    judgementCases :: Game -> [JudgementCase]
    judgementCases g = 
        g ^.. traverseStatuses . _Selector . _WaitingJudgement 

    -- | Convert a game to public information
    toPublicGame :: Game -> PublicGame
    toPublicGame g =
        PublicGame (Map.map personalToImpersonal (g ^. gamePlayers))

    -- | Move the game to a state where a judge is actively picking cards,
    -- in a state monad
    startJudgementM = do
        let judgeLens = traverseStatuses . _Judge
        ag <- activeJudgements <$> get 
        call <- preuse $ judgeLens . _WaitingCases 
        case call of
            Just call' -> judgeLens .= PickingWinner call' ag
            Nothing -> return () 

    -- | Move the game to a state where a judge is actively picking cards
    startJudgement g
        = g & judgeLens .~ PickingWinner call (activeJudgements g)
        where
            judgeLens = traverseStatuses . _Judge
            -- TODO: Make safer please
            call = g ^?! judgeLens . _WaitingCases

    -- | Get the current 'CallCard' for the game
    gameCurrentCall :: Game -> Maybe CallCard
    gameCurrentCall g
        = g ^? judgeLens . _WaitingCases
            <|> g ^? judgeLens . _PickingWinner . _1 
        where
            judgeLens = traverseStatuses . _Judge 
