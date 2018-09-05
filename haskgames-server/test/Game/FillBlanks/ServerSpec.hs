{-# LANGUAGE OverloadedStrings, AllowAmbiguousTypes #-}

module Game.FillBlanks.ServerSpec where 
        
    import Test.Hspec
    import Control.Lens
    import Game.FillBlanks.Deck
    import Game.FillBlanks.Game
    import Game.FillBlanks.ServerState
    import Game.Common
    import Data.Monoid
    import qualified Data.Map as Map
    import Game.FillBlanks.Server
    import Game.CommonHelpers
    import Game.FillBlanks.Event

    defaultCard = CallCard "" 1 FillIn
    defaultPlayers = playersToPlayerState $ map Player [0, 2, 1, 2]

    makeAwaiting subs = cs
        where
            cs = Game "1" 2 mempty mempty sl AwaitingSubmissions Nothing defaultPlayers
            sl = Map.fromList subs

    spec :: Spec
    spec =
        waitSubmissionsSpec

    waitSubmissionsSpec = context "when the game is waiting for submissions" $ do
        let oneMore = makeAwaiting $
                        [ (JudgementCase [] 3, "3")
                        , (JudgementCase [] 4, "4") ] 
        let twoMore = makeAwaiting [ (JudgementCase [] 3, "3") ]
        let runTest i p e = runIdentity $ serve i p e
        describe "attempted judgement" $
            it "does nothing when sent from the judge" $ do
                let evt = SelectWinner $ JudgementCase [ResponseCard "" FillIn] 1 
                runTest twoMore "1" evt `shouldBe` twoMore
        describe "submitting a judgement case" $ do
            context "when sending from somebody yet to submit" $ do
                let jc = JudgementCase [ResponseCard "" FillIn] 2
                let evt = SubmitJudgement $ jc
                context "when that is the last judgement" $ do
                    let r = runTest oneMore "2" evt
                    it "modifies the state" $
                        r `shouldNotBe` oneMore
                    it "adds the judgement" $
                        judgementPlayer r jc `shouldBe` Just "2"
                    it "sets the current status to awaiting judgement" $
                        shouldBe
                            (r ^. gameStatus)
                            AwaitingJudgement
                context "when there are more judements" $ do
                    let r = runTest twoMore "2" evt
                    it "modifies the state" $ 
                        r `shouldNotBe` twoMore
                    it "adds the judgement" $
                        judgementPlayer r jc `shouldBe` Just "2"
                    it "does not change the current status" $
                        (r ^. gameStatus)
                            `shouldBe` AwaitingSubmissions
            context "when sending from the judge" $ do
                let jc = JudgementCase [ResponseCard "" FillIn] 1
                let evt = SubmitJudgement $ jc
                let r = runTest oneMore "1" evt
                it "does not change the state" $
                    r `shouldBe` oneMore
            context "when sending from a person who already submitted" $ do
                let jc = JudgementCase [ResponseCard "" FillIn] 1
                let evt = SubmitJudgement $ jc
                let r = runTest oneMore "3" evt
                it "does not change the state" $
                    r `shouldBe` oneMore