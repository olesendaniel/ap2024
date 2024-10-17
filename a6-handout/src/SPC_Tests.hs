module SPC_Tests (tests) where

import Control.Concurrent (threadDelay)
import Control.Monad (forM, forM_, replicateM)
import Data.IORef
import SPC
import Test.Tasty (TestTree, localOption, mkTimeout, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=), assertEqual)

-- A utility function to create a simple job with a given action
createJob :: IO () -> Int -> Job
createJob action maxSeconds = Job { jobAction = action, jobMaxSeconds = maxSeconds }

tests :: TestTree
tests =
  localOption (mkTimeout 3000000) $
    testGroup
      "SPC (core)"
      [testCase "Add worker and run job" test_addWorkerAndRunJob,
      testCase "Cancel a running job" test_cancelRunningJob,
      testCase "Adding a worker" testWorkerAdd,
      testCase "Adding 2 workers" test2WorkerAdd
      ]

testWorkerAdd :: IO ()
testWorkerAdd = do
  spc <- startSPC
  worker <- workerAdd spc "worker1"
  case worker of
    Left _ -> assertFailure "Worker not added"
    Right _ -> pure ()

test2WorkerAdd :: IO ()
test2WorkerAdd = do
  spc <- startSPC
  worker1 <- workerAdd spc "worker1"
  worker2 <- workerAdd spc "worker1"
  case (worker1, worker2) of
    (Right _, Left _) -> pure ()
    (_,_) -> assertFailure "Error with 2 workers"



-- Test 1: Add worker and run a single job
test_addWorkerAndRunJob :: IO ()
test_addWorkerAndRunJob = do
  spc <- startSPC
  resultRef <- newIORef False
  let job = createJob (writeIORef resultRef True) 5

  -- Add a worker
  _ <- workerAdd spc "tworker"
  _ <- jobAdd spc job

  -- Wait for the job to complete
  threadDelay 500000

  -- Check that the job was executed
  result <- readIORef resultRef
  result @?= True

-- Test 2: Cancel a running job
test_cancelRunningJob :: IO ()
test_cancelRunningJob = do
  spc <- startSPC
  resultRef <- newIORef False
  let job = createJob (threadDelay 1000000 >> writeIORef resultRef True) 5

  -- Add a worker
  _ <- workerAdd spc "tworker"
  jobId <- jobAdd spc job

  -- Cancel the job before it finishes
  jobCancel spc jobId
  reason <- jobWait spc jobId
  reason @?= DoneCancelled

  -- Check that the job was cancelled and did not execute
  threadDelay 1100000
  result <- readIORef resultRef
  result @?= False