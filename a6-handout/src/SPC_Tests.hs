module SPC_Tests (tests) where

import Control.Concurrent (threadDelay)
--import Control.Monad (forM, forM_, replicateM)
import Data.IORef
import SPC
import Test.Tasty (TestTree, localOption, mkTimeout, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

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
      testCase "Adding 2 workers" test2WorkerAdd,
      testCase "Cancel after Done" cancelJobAlreadyDone,
      testCase "Timeout simple" timeOutTest,
      testCase "Crash simple" crashTest,
      testCase "Crash then Timeout" crashThenTimeOut,
      testCase "Remove Worker"removeWorkerTest,
      testCase "Add a worker, remove it, then add new worker with the same name" addWorkerSameName,
      testCase "Add Job After worker deletion" addJobAfterWorkerDeletion,
      testCase "Exception after Timeout" exceptionAfterTimeout,
      testCase "Worker executes job timeout, worker executes further job" jobAfterTimeout,
      testCase "Worker executes job exception, worker executes furhter job" jobAfterCrash
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

-- Cancels a job that already finished
cancelJobAlreadyDone :: IO ()
cancelJobAlreadyDone = do
  spc <- startSPC
  resultRef <- newIORef False
  let job = createJob (writeIORef resultRef True) 5

  _ <- workerAdd spc "tworker"
  jobId <- jobAdd spc job

  threadDelay 100000
  jobCancel spc jobId

  reason <- jobWait spc jobId
  reason @?= Done

  result <- readIORef resultRef
  result @?= True

timeOutTest :: IO ()
timeOutTest = do

  spc <- startSPC
  resultRef <- newIORef False
  let job = createJob (threadDelay 1100000 >> writeIORef resultRef True) 1

  _ <- workerAdd spc "tworker"
  jobId <- jobAdd spc job

  threadDelay 1200000

  reason <- jobWait spc jobId
  reason @?= DoneTimeout

  result <- readIORef resultRef
  result @?= False

crashTest :: IO ()
crashTest = do
  spc <- startSPC
  resultRef <- newIORef False
  let job = createJob (error "boom" >> writeIORef resultRef True) 5

  _ <- workerAdd spc "tworker"
  jobId <- jobAdd spc job

  threadDelay 500000

  reason <- jobWait spc jobId
  reason @?= DoneCrashed

  result <- readIORef resultRef
  result @?= False

crashThenTimeOut :: IO ()
crashThenTimeOut = do

  spc <- startSPC
  resultRef <- newIORef False
  let job = createJob (error "boom" >> writeIORef resultRef True) 1

  _ <- workerAdd spc "tworker"
  jobId <- jobAdd spc job

  threadDelay 1100000

  reason <- jobWait spc jobId
  reason @?= DoneCrashed

  result <- readIORef resultRef
  result @?= False

removeWorkerTest :: IO ()
removeWorkerTest = do
  spc <- startSPC
  resultRef <- newIORef False

  let job = createJob (threadDelay 1000000 >> writeIORef resultRef True) 2

  Right worker <- workerAdd spc "tworker"
  jobId <- jobAdd spc job

  workerStop worker

  reason <- jobWait spc jobId
  reason @?= DoneCancelled

  result <- readIORef resultRef
  result @?= False


addWorkerSameName :: IO ()
addWorkerSameName = do
  spc <- startSPC
  Right worker1 <- workerAdd spc "worker1"
  workerStop worker1
  threadDelay 100000
  worker2 <- workerAdd spc "worker1"
  case worker2 of
    Right _ -> pure ()
    _ -> assertFailure "Second worker fail"

addJobAfterWorkerDeletion :: IO ()
addJobAfterWorkerDeletion = do
  spc <- startSPC
  Right worker <- workerAdd spc "worker1"
  resultRef <- newIORef False

  workerStop worker

  threadDelay 100000

  let job = createJob (threadDelay 1000000 >> writeIORef resultRef True) 2

  jobId <- jobAdd spc job

  threadDelay 200000

  _ <- workerAdd spc "worker1"

  reason <- jobWait spc jobId
  reason @?= Done

  result <- readIORef resultRef
  result @?= True


exceptionAfterTimeout :: IO ()
exceptionAfterTimeout = do
  spc <- startSPC
  _ <- workerAdd spc "worker"
  resultRef <- newIORef False

  let job = createJob (threadDelay 11000000 >> error "boom" >> writeIORef resultRef True) 1

  jobId <- jobAdd spc job

  reason <- jobWait spc jobId
  reason @?= DoneTimeout

  result <- readIORef resultRef
  result @?= False


jobAfterTimeout :: IO ()
jobAfterTimeout = do
  spc <- startSPC
  _ <- workerAdd spc "worker"
  resultRef1 <- newIORef False
  resultRef2 <- newIORef False

  let job1 = createJob (threadDelay 1100000 >> writeIORef resultRef1 True) 1

  jobId1 <- jobAdd spc job1

  threadDelay 1200000

  reason1 <- jobWait spc jobId1
  reason1 @?= DoneTimeout

  result1 <- readIORef resultRef1
  result1 @?= False

  let job2 = createJob (writeIORef resultRef2 True) 1

  jobId2 <- jobAdd spc job2
  threadDelay 100000

  reason2 <- jobWait spc jobId2
  reason2 @?= Done

  result2 <- readIORef resultRef2
  result2 @?= True


jobAfterCrash :: IO ()
jobAfterCrash = do
  spc <- startSPC
  _ <- workerAdd spc "worker"
  resultRef1 <- newIORef False
  resultRef2 <- newIORef False

  let job1 = createJob (error "boom" >> writeIORef resultRef1 True) 1

  jobId1 <- jobAdd spc job1

  threadDelay 100000

  reason1 <- jobWait spc jobId1
  reason1 @?= DoneCrashed

  result1 <- readIORef resultRef1
  result1 @?= False

  let job2 = createJob (writeIORef resultRef2 True) 1

  jobId2 <- jobAdd spc job2
  threadDelay 100000

  reason2 <- jobWait spc jobId2
  reason2 @?= Done

  result2 <- readIORef resultRef2
  result2 @?= True