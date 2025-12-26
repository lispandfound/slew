module Logic.EventHandlersSpec (spec) where

import Data.Time.Clock.System (SystemTime (..))
import Logic.EventHandlers
import Model.Job
import Test.Hspec

-- | Helper to create a minimal Job for testing
mkTestJob :: Int -> Text -> Job
mkTestJob jobId' name' =
    Job
        { account = "test-account"
        , cpus = Unset
        , endTime = Unset
        , exitCode = ExitCode {status = [], returnCode = Unset}
        , jobId = jobId'
        , jobState = ["RUNNING"]
        , memoryPerNode = Unset
        , name = name'
        , nodeCount = Unset
        , standardOutput = ""
        , standardError = ""
        , nodes = ""
        , partition = "default"
        , startTime = Set (MkSystemTime 0 0)
        , stateReason = ""
        , timeLimit = Unset
        , userName = "testuser"
        }

spec :: Spec
spec = do
    describe "scontrolCommand" $ do
        it "generates correct cancel command" $ do
            let job = mkTestJob 123 "test-job"
            scontrolCommand Cancel job `shouldBe` ("scancel", ["123"])

        it "generates correct suspend command" $ do
            let job = mkTestJob 456 "test-job"
            scontrolCommand Suspend job `shouldBe` ("scontrol", ["suspend", "456"])

        it "generates correct resume command" $ do
            let job = mkTestJob 789 "test-job"
            scontrolCommand Resume job `shouldBe` ("scontrol", ["resume", "789"])

        it "generates correct hold command" $ do
            let job = mkTestJob 100 "test-job"
            scontrolCommand Hold job `shouldBe` ("scontrol", ["hold", "100"])

        it "generates correct release command" $ do
            let job = mkTestJob 200 "test-job"
            scontrolCommand Release job `shouldBe` ("scontrol", ["release", "200"])

        it "generates correct top command" $ do
            let job = mkTestJob 300 "test-job"
            scontrolCommand Top job `shouldBe` ("scontrol", ["top", "300"])

    describe "shouldTriggerRefresh" $ do
        it "returns True for all commands" $ do
            shouldTriggerRefresh Cancel `shouldBe` True
            shouldTriggerRefresh Suspend `shouldBe` True
            shouldTriggerRefresh Resume `shouldBe` True
            shouldTriggerRefresh Hold `shouldBe` True
            shouldTriggerRefresh Release `shouldBe` True
            shouldTriggerRefresh Top `shouldBe` True
