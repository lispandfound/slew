{-# LANGUAGE ScopedTypeVariables #-}

module Logic.JobFilteringSpec (spec) where

import Data.Time.Clock.System (SystemTime (..))
import Logic.JobFiltering
import Model.Job
import Test.Hspec
import Test.QuickCheck

-- | Helper to create a minimal Job for testing
mkTestJob :: Int -> Text -> Text -> Text -> Job
mkTestJob jobId' name' userName' account' =
    Job
        { account = account'
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
        , userName = userName'
        }

spec :: Spec
spec = do
    describe "filterJobs" $ do
        it "returns all jobs when search term is empty" $ do
            let jobs = [mkTestJob 1 "job1" "user1" "acc1", mkTestJob 2 "job2" "user2" "acc2"]
            filterJobs "" jobs `shouldBe` jobs

        it "filters jobs by job ID" $ do
            let jobs = [mkTestJob 123 "test" "user1" "acc1", mkTestJob 456 "test2" "user2" "acc2"]
            filterJobs "123" jobs `shouldBe` [mkTestJob 123 "test" "user1" "acc1"]

        it "filters jobs by name (case insensitive)" $ do
            let jobs = [mkTestJob 1 "MyJob" "user1" "acc1", mkTestJob 2 "other" "user2" "acc2"]
            filterJobs "myjob" jobs `shouldBe` [mkTestJob 1 "MyJob" "user1" "acc1"]

        it "filters jobs by account" $ do
            let jobs = [mkTestJob 1 "job1" "user1" "research", mkTestJob 2 "job2" "user2" "teaching"]
            filterJobs "research" jobs `shouldBe` [mkTestJob 1 "job1" "user1" "research"]

        it "filters jobs by partial match" $ do
            let jobs = [mkTestJob 1 "simulation_v1" "user1" "acc1", mkTestJob 2 "analysis" "user2" "acc2"]
            filterJobs "sim" jobs `shouldBe` [mkTestJob 1 "simulation_v1" "user1" "acc1"]

        it "returns multiple matching jobs" $ do
            let jobs =
                    [ mkTestJob 1 "test1" "user1" "acc1"
                    , mkTestJob 2 "test2" "user2" "acc2"
                    , mkTestJob 3 "other" "user3" "acc3"
                    ]
            length (filterJobs "test" jobs) `shouldBe` 2

        it "filters by job state" $ do
            let job1 = (mkTestJob 1 "job1" "user1" "acc1"){jobState = ["PENDING"]}
                job2 = (mkTestJob 2 "job2" "user2" "acc2"){jobState = ["RUNNING"]}
            filterJobs "PENDING" [job1, job2] `shouldBe` [job1]

    describe "sortJobsBy" $ do
        it "sorts jobs by account name" $ do
            let jobs = [mkTestJob 1 "job1" "user1" "zebra", mkTestJob 2 "job2" "user2" "alpha"]
            sortJobsBy Account jobs `shouldBe` [mkTestJob 2 "job2" "user2" "alpha", mkTestJob 1 "job1" "user1" "zebra"]

        it "sorts jobs by job name" $ do
            let jobs = [mkTestJob 1 "z_job" "user1" "acc1", mkTestJob 2 "a_job" "user2" "acc2"]
            sortJobsBy JobName jobs `shouldBe` [mkTestJob 2 "a_job" "user2" "acc2", mkTestJob 1 "z_job" "user1" "acc1"]

        it "sorts jobs by user name" $ do
            let jobs = [mkTestJob 1 "job1" "zara" "acc1", mkTestJob 2 "job2" "alice" "acc2"]
            sortJobsBy UserName jobs `shouldBe` [mkTestJob 2 "job2" "alice" "acc2", mkTestJob 1 "job1" "zara" "acc1"]

        it "sorts jobs by CPUs" $ do
            let job1 = (mkTestJob 1 "job1" "user1" "acc1"){cpus = Set 8}
                job2 = (mkTestJob 2 "job2" "user2" "acc2"){cpus = Set 4}
                job3 = (mkTestJob 3 "job3" "user3" "acc3"){cpus = Unset}
            sortJobsBy CPUs [job1, job2, job3] `shouldBe` [job3, job2, job1]

    describe "sortListByCat" $ do
        it "compares jobs correctly by category" $ do
            let job1 = mkTestJob 1 "a" "user1" "acc1"
                job2 = mkTestJob 2 "b" "user2" "acc2"
            sortListByCat JobName job1 job2 `shouldBe` LT
            sortListByCat JobName job2 job1 `shouldBe` GT
            sortListByCat JobName job1 job1 `shouldBe` EQ

    describe "Property: filterJobs is idempotent" $ do
        it "applying filter twice gives same result as applying once" $
            property $ \(searchTerm :: String) ->
                let jobs = [mkTestJob 1 "test" "user" "acc", mkTestJob 2 "other" "user" "acc"]
                    filtered = filterJobs (toText searchTerm) jobs
                 in filterJobs (toText searchTerm) filtered == filtered
