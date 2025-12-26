{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

module Types.SlurmAPI (
    SlurmAPI (..),
    RealSlurmAPI (..),
    MockSlurmAPI (..),
    runMockSlurmAPI,
) where

import Data.Aeson (eitherDecode)
import Model.Job (Job)
import Model.SQueue (SlurmResponse (..))
import Model.SlurmCommand (SlurmCommandError, SlurmCommandOutput)
import qualified Model.SlurmCommand as SC
import Optics.Getter (view)
import System.Process (readCreateProcess, shell)

-- | Typeclass for abstracting Slurm operations
-- This allows testing code that interacts with Slurm without actually calling squeue/scontrol
class Monad m => SlurmAPI m where
    -- | Poll the job queue
    pollJobs :: m (Either String [Job])
    
    -- | Cancel jobs by ID
    cancelJob :: [Int] -> m (Either SlurmCommandError SlurmCommandOutput)
    
    -- | Hold jobs
    holdJob :: [Int] -> m (Either SlurmCommandError SlurmCommandOutput)
    
    -- | Release jobs
    releaseJob :: [Int] -> m (Either SlurmCommandError SlurmCommandOutput)
    
    -- | Suspend jobs
    suspendJob :: [Int] -> m (Either SlurmCommandError SlurmCommandOutput)
    
    -- | Resume jobs
    resumeJob :: [Int] -> m (Either SlurmCommandError SlurmCommandOutput)
    
    -- | Top priority
    topJob :: [Int] -> m (Either SlurmCommandError SlurmCommandOutput)

-- | Real implementation that calls actual squeue/scontrol commands
newtype RealSlurmAPI a = RealSlurmAPI {runRealSlurmAPI :: IO a}
    deriving (Functor, Applicative, Monad, MonadIO)

instance SlurmAPI RealSlurmAPI where
    pollJobs = RealSlurmAPI $ do
        squeueFile <- readCreateProcess (shell "squeue --json") ""
        return . second (view #jobs) . parse . encodeUtf8 $ squeueFile
      where
        parse :: LByteString -> Either String SlurmResponse
        parse = eitherDecode
    
    cancelJob ids = RealSlurmAPI $ SC.cancelJob ids
    
    holdJob ids = RealSlurmAPI $ SC.holdJob ids
    
    releaseJob ids = RealSlurmAPI $ SC.releaseJob ids
    
    suspendJob ids = RealSlurmAPI $ SC.suspendJob ids
    
    resumeJob ids = RealSlurmAPI $ SC.resumeJob ids
    
    topJob ids = RealSlurmAPI $ SC.topJob ids

-- | Mock implementation for testing
data MockSlurmAPI a = MockSlurmAPI
    { _mockJobs :: [Job]
    , _mockCommandResults :: [(Text, Either SlurmCommandError SlurmCommandOutput)]
    , _mockResult :: a
    }
    deriving (Functor)

runMockSlurmAPI :: MockSlurmAPI a -> a
runMockSlurmAPI (MockSlurmAPI _ _ a) = a

instance Applicative MockSlurmAPI where
    pure x = MockSlurmAPI [] [] x
    MockSlurmAPI j1 c1 f <*> MockSlurmAPI j2 c2 a = MockSlurmAPI (j1 <> j2) (c1 <> c2) (f a)

instance Monad MockSlurmAPI where
    return = pure
    MockSlurmAPI jobs1 cmds1 a >>= f = 
        let MockSlurmAPI jobs2 cmds2 b = f a
        in MockSlurmAPI (jobs1 <> jobs2) (cmds1 <> cmds2) b

instance SlurmAPI MockSlurmAPI where
    pollJobs = MockSlurmAPI [] [] (Right [])
    
    cancelJob ids = MockSlurmAPI [] [("cancel", Right undefined)] (Right undefined)
    
    holdJob ids = MockSlurmAPI [] [("hold", Right undefined)] (Right undefined)
    
    releaseJob ids = MockSlurmAPI [] [("release", Right undefined)] (Right undefined)
    
    suspendJob ids = MockSlurmAPI [] [("suspend", Right undefined)] (Right undefined)
    
    resumeJob ids = MockSlurmAPI [] [("resume", Right undefined)] (Right undefined)
    
    topJob ids = MockSlurmAPI [] [("top", Right undefined)] (Right undefined)
