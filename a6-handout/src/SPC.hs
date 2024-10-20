module SPC
  ( -- * SPC startup
    SPC,
    startSPC,

    -- * Job functions
    Job (..),
    JobId,
    JobStatus (..),
    JobDoneReason (..),
    jobAdd,
    jobStatus,
    jobWait,
    jobCancel,

    -- * Worker functions
    WorkerName,
    workerAdd,
    workerStop,
  )
where
import Control.Exception (SomeException, catch)
import Control.Concurrent
  ( forkIO,
    killThread,
    threadDelay, ThreadId,
  )
import Control.Monad (ap, forever, liftM, void)
import GenServer
--import System.Clock.Seconds (Clock (Monotonic), Seconds, getTime)

-- First some general utility functions.

-- | Retrieve Unix time using a monotonic clock. You cannot use this
-- to measure the actual world time, but you can use it to measure
-- elapsed time.
--getSeconds :: IO Seconds
--getSeconds = getTime Monotonic

-- | Remove mapping from association list.
--removeAssoc :: (Eq k) => k -> [(k, v)] -> [(k, v)]
--removeAssoc needle ((k, v) : kvs) =
--  if k == needle
--    then kvs
--    else (k, v) : removeAssoc needle kvs
--removeAssoc _ [] = []

-- Then the definition of the glorious SPC.

-- | A job that is to be enqueued in the glorious SPC.
data Job = Job
  { -- | The IO action that comprises the actual action of the job.
    jobAction :: IO (),
    -- | The maximum allowed runtime of the job, counting from when
    -- the job begins executing (not when it is enqueued).
    jobMaxSeconds :: Int
  }

-- | A unique identifier of a job that has been enqueued.
newtype JobId = JobId Int
  deriving (Eq, Ord, Show)

-- | How a job finished.
data JobDoneReason
  = -- | Normal termination.
    Done
  | -- | The job was killed because it ran for too long.
    DoneTimeout
  | -- | The job was explicitly cancelled, or the worker
    -- it was running on was stopped.
    DoneCancelled
  | -- | The job crashed due to an exception.
    DoneCrashed
  deriving (Eq, Ord, Show)

-- | The status of a job.
data JobStatus
  = -- | The job is done and this is why.
    JobDone JobDoneReason
  | -- | The job is still running.
    JobRunning
  | -- | The job is enqueued, but is waiting for an idle worker.
    JobPending
  | -- | A job with this ID is not known to this SPC instance.
    JobUnknown
  deriving (Eq, Ord, Show)

-- | A worker decides its own human-readable name. This is useful for
-- debugging.
type WorkerName = String

-- | Messages sent to workers. These are sent both by SPC and by
-- processes spawned by the workers.
data WorkerMsg -- TODO: add messages.
  =
    MsgJobToDo Job JobId
  |
    MsgWorkerJobCancel JobId
  |
    MsgChildWorkerDone JobId WorkerName
  |
    MsgJobTimeout JobId WorkerName
  |
    MsgJobCrashed JobId WorkerName
  |
    MsgWorkerRemove


-- Messages sent to SPC.
data SPCMsg
  = -- | Add the job, and reply with the job ID.
    MsgJobAdd Job (ReplyChan JobId)
  | -- | Cancel the given job.
    MsgJobCancel JobId
  | -- | Immediately reply the status of the job.
    MsgJobStatus JobId (ReplyChan JobStatus)
  | -- | Reply when the job is done.
    MsgJobWait JobId (ReplyChan JobDoneReason)
  | -- | Some time has passed.
    MsgTick
  |
    MsgWorkerDone JobId WorkerName JobDoneReason
  |
    MsgWorkerAdd WorkerName (ReplyChan (Either String Worker))
  |
    MsgWorkerDeleted WorkerName

-- | A handle to the SPC instance.
data SPC = SPC (Server SPCMsg)

-- | A handle to a worker.
data Worker = Worker (Server WorkerMsg)

-- | The central state. Must be protected from the bourgeoisie.
data SPCState = SPCState
  { spcJobsPending :: [(JobId, Job)],
    spcJobsRunning :: [(JobId, Job)],
    spcJobsDone :: [(JobId, JobDoneReason)],
    spcJobCounter :: JobId,
    spcWorkerList :: [(Worker, WorkerName, Maybe JobId)],
    spcWaiting :: [(JobId, ReplyChan JobDoneReason)]
    -- TODO: you will need to add more fields.
  }

-- | The monad in which the main SPC thread runs. This is a state
-- monad with support for IO.
newtype SPCM a = SPCM (SPCState -> IO (a, SPCState))

instance Functor SPCM where
  fmap = liftM

instance Applicative SPCM where
  pure x = SPCM $ \state -> pure (x, state)
  (<*>) = ap

instance Monad SPCM where
  SPCM m >>= f = SPCM $ \state -> do
    (x, state') <- m state
    let SPCM f' = f x
    f' state'

-- | Retrieve the state.
get :: SPCM SPCState
get = SPCM $ \state -> pure (state, state)

-- | Overwrite the state.
put :: SPCState -> SPCM ()
put state = SPCM $ \_ -> pure ((), state)

-- | Modify the state.
--modify :: (SPCState -> SPCState) -> SPCM ()
--modify f = do
--  state <- get
--  put $ f state
--
-- | Lift an 'IO' action into 'SPCM'.
io :: IO a -> SPCM a
io m = SPCM $ \state -> do
  x <- m
  pure (x, state)

-- | Run the SPCM monad.
runSPCM :: SPCState -> SPCM a -> IO a
runSPCM state (SPCM f) = fst <$> f state

schedule :: SPCM ()
schedule = do
  state <- get
  let pendingJobsList = spcJobsPending state
  if null pendingJobsList
    then
      pure ()
    else
      let workerList = spcWorkerList state
      in case checkWorkerAvailability workerList of
        Nothing -> pure ()
        Just (Worker s, name) -> do
          let job = snd $ head pendingJobsList
          let jobId = fst $ head pendingJobsList
          let jobMsg = MsgJobToDo job jobId
          io $ sendTo s jobMsg
          let jobsRunningList = spcJobsRunning state
          let jobsPendingList = spcJobsPending state
          put $
            state
              {
                spcJobsPending = removeJob jobId jobsPendingList,
                spcJobsRunning = jobsRunningList ++ [(jobId, job)],
                spcWorkerList = addJobId name jobId workerList

              }

--jobDone :: JobId -> JobDoneReason -> SPCM ()
--jobDone = undefined
--
--workerIsIdle :: WorkerName -> Worker -> SPCM ()
--workerIsIdle = undefined
--
--workerIsGone :: WorkerName -> SPCM ()
--workerIsGone = undefined

checkTimeouts :: SPCM ()
checkTimeouts = pure () -- change in Task 4

getSnd :: (a, b, c) -> b
getSnd (_, x, _) = x

getFst :: (a, b, c) -> a
getFst (x, _, _) = x

getTrd :: (a, b, c) -> c
getTrd (_, _, x) = x

workerExists :: WorkerName -> SPCM Bool
workerExists name = do
  state <- get
  if name `elem` map getSnd (spcWorkerList state)
    then
      pure True
    else
      pure False

handleMsg :: Chan SPCMsg -> SPCM ()
handleMsg c = do
  checkTimeouts
  schedule
  msg <- io $ receive c
  case msg of
    MsgJobAdd job rsvp -> do
      state <- get
      let JobId jobid = spcJobCounter state
      put $
        state
          { spcJobsPending =
              (spcJobCounter state, job) : spcJobsPending state,
            spcJobCounter = JobId $ succ jobid
          }
      io $ reply rsvp $ JobId jobid
    MsgJobStatus jobid rsvp -> do
      state <- get
      io $ reply rsvp $ case ( lookup jobid $ spcJobsPending state,
                               lookup jobid $ spcJobsRunning state,
                               lookup jobid $ spcJobsDone state
                             ) of
        (Just _, _, _) -> JobPending
        (_, Just _, _) -> JobRunning
        (_, _, Just r) -> JobDone r
        _ -> JobUnknown
    MsgWorkerAdd name rsvp -> do
      exist <- workerExists name
      if exist
        then
          io $ reply rsvp $ Left ("Name already exists" ++ show name)
        else do
          state <- get
          tmp <- io $ spawn $ \workerMsg -> workerLoop name c workerMsg Nothing
          let worker = Worker tmp
          let workerList = spcWorkerList state
          put $
            state
              {
                spcWorkerList =
                  workerList ++ [(worker, name, Nothing)]
              }
          io $ reply rsvp $ Right worker
    MsgWorkerDone jobId name jobDoneReason -> do
      state <- get
      let workerList = spcWorkerList state
      let jobsDoneList = spcJobsDone state
      let jobsRunnigList = spcJobsRunning state
      let waitingList = spcWaiting state
      case lookup jobId waitingList of
        Nothing -> pure ()
        Just rsvp ->
          io $ reply rsvp jobDoneReason
      put $
        state
          {
            spcJobsRunning = removeJob jobId jobsRunnigList,
            spcJobsDone = jobsDoneList ++ [(jobId, jobDoneReason)],
            spcWorkerList = updateWorkerList name workerList,
            spcWaiting = updateWaitingList jobId waitingList
          }
    MsgTick ->
      pure ()
    MsgJobCancel jobId -> do
      state <- get
      let jobsPendingList = spcJobsPending state
      let jobsDoneList = spcJobsDone state
      let waitingList = spcWaiting state
      case lookup jobId waitingList of
        Nothing -> pure ()
        Just rsvp ->
          io $ reply rsvp DoneCancelled
      case lookup jobId jobsPendingList of
        Just _ ->
          put $
            state
              {
                spcJobsPending = removeJob jobId jobsPendingList,
                spcJobsDone = jobsDoneList ++ [(jobId,DoneCancelled)],
                spcWaiting = updateWaitingList jobId waitingList
              }
        Nothing ->
          let jobsRunningList = spcJobsRunning state
          in case lookup jobId jobsRunningList of
            Nothing -> pure ()
            Just _ -> do
              let workerList = spcWorkerList state
              let temp = workerJobIdLookup jobId workerList
                in case temp of
                  Nothing -> pure ()
                  Just (Worker s, name) -> do
                    let jobMsg = MsgWorkerJobCancel jobId
                    io $ sendTo s jobMsg
                    put $
                      state
                        {
                          spcJobsRunning = removeJob jobId jobsRunningList,
                          spcWorkerList = updateWorkerList name workerList,
                          spcJobsDone = jobsDoneList ++ [(jobId,DoneCancelled)],
                          spcWaiting = updateWaitingList jobId waitingList
                        }

    MsgJobWait jobId rsvp -> do
      state <- get
      let jobsDoneList = spcJobsDone state
      let waitingList = spcWaiting state
      case lookup jobId jobsDoneList of
        Just reason -> io $ reply rsvp reason
        Nothing -> do
          put $
            state
              {
                spcWaiting = waitingList ++ [(jobId, rsvp)]
              }
    MsgWorkerDeleted name -> do
      state <- get
      let workerList = spcWorkerList state
      let jobDoneList = spcJobsDone state
      let jobsRunningList = spcJobsRunning state
      let waitingList = spcWaiting state
      case getJobIdFromWorker name workerList of
        Nothing -> do
          put $
            state
              {
                spcWorkerList = removeWorker name workerList
              }
        Just jobId -> do
          case lookup jobId waitingList of
            Nothing -> pure ()
            Just rsvp ->
              io $ reply rsvp DoneCancelled
          put $
            state
              {
                spcJobsRunning = removeJob jobId jobsRunningList,
                spcJobsDone = jobDoneList ++ [(jobId, DoneCancelled)],
                spcWorkerList = removeWorker name workerList,
                spcWaiting = updateWaitingList jobId waitingList

              }

-- spcWaiting :: [(JobId, ReplyChan JobDoneReason)]
workerLoop :: WorkerName -> Chan SPCMsg -> Chan WorkerMsg -> Maybe ThreadId -> IO()
workerLoop name cSPCMsg cWMsg tid = do
  todo <- receive cWMsg
  case todo of
    MsgJobToDo job jobId -> do
      newTid <- forkIO $ workerChild name job jobId cWMsg
      _ <- forkIO $ timeoutThread name job jobId cWMsg
      workerLoop name cSPCMsg cWMsg $ Just newTid
    MsgWorkerJobCancel _ ->
      case tid of
        Just t -> do
          killThread t
          workerLoop name cSPCMsg cWMsg Nothing
        Nothing -> do
          workerLoop name cSPCMsg cWMsg Nothing
    MsgChildWorkerDone j n -> do
      send cSPCMsg $ MsgWorkerDone j n Done
      case tid of
        Just t -> do
          killThread t
          workerLoop name cSPCMsg cWMsg Nothing
        Nothing -> do
          workerLoop name cSPCMsg cWMsg Nothing
    MsgJobTimeout j n -> do
      send cSPCMsg $ MsgWorkerDone j n DoneTimeout
      case tid of
        Just t -> do
          killThread t
          workerLoop name cSPCMsg cWMsg Nothing
        Nothing -> do
          workerLoop name cSPCMsg cWMsg Nothing
    MsgJobCrashed j n -> do
      send cSPCMsg $ MsgWorkerDone j n DoneCrashed
      case tid of
        Just t -> do
          killThread t
          workerLoop name cSPCMsg cWMsg Nothing
        Nothing -> do
          workerLoop name cSPCMsg cWMsg Nothing
    MsgWorkerRemove -> do
      send cSPCMsg $ MsgWorkerDeleted name
      case tid of
        Just t -> do
          killThread t
        Nothing -> do
          pure ()

workerChild :: WorkerName -> Job -> JobId -> Chan WorkerMsg -> IO()
workerChild name job jobId cWMsg = do
  let doJob = do
        jobAction job
        send cWMsg $ MsgChildWorkerDone jobId name
      onException :: SomeException -> IO ()
      onException _ =
        send cWMsg $ MsgJobCrashed jobId name
  doJob `catch` onException

timeoutThread :: WorkerName -> Job -> JobId -> Chan WorkerMsg -> IO ()
timeoutThread name job jobId cWMsg = do
  threadDelay $ jobMaxSeconds job * 1000000
  send cWMsg $ MsgJobTimeout jobId name

-- Removes a specific job from list of jobs
removeJob :: JobId -> [(JobId, Job)] -> [(JobId, Job)]
removeJob _ [] = []
removeJob job (c:cs) = if job == fst c
  then
    cs
  else
    c : removeJob job cs

-- changes the Maybe JobId to nothing for a specific WorkerName
updateWorkerList :: WorkerName -> [(Worker, WorkerName, Maybe JobId)] -> [(Worker, WorkerName, Maybe JobId)]
updateWorkerList _ [] = []
updateWorkerList name (c:cs) =
  if name == getSnd c
    then
      (getFst c ,getSnd c, Nothing) : cs
    else
      c : updateWorkerList name cs

addJobId :: WorkerName -> JobId -> [(Worker, WorkerName, Maybe JobId)] -> [(Worker, WorkerName, Maybe JobId)]
addJobId _ _ [] = []
addJobId name jobId (c:cs) = if name == getSnd c
  then
    (getFst c, getSnd c, Just jobId): cs
  else
    c : cs

-- Checks all workers and returns a worker if it's available
checkWorkerAvailability :: [(Worker, WorkerName, Maybe JobId)] -> Maybe(Worker, WorkerName)
checkWorkerAvailability [] = Nothing
checkWorkerAvailability (c:cs) =
  if getTrd c == Nothing
    then
      Just $ (getFst c, getSnd c)
    else
      checkWorkerAvailability cs

getJobIdFromWorker :: WorkerName -> [(Worker, WorkerName, Maybe JobId)] -> Maybe JobId
getJobIdFromWorker _ [] = Nothing
getJobIdFromWorker name (c:cs) =
  if getSnd c == name
    then
      getTrd c
    else
      getJobIdFromWorker name cs

workerJobIdLookup :: JobId -> [(Worker, WorkerName, Maybe JobId)] -> Maybe (Worker, WorkerName)
workerJobIdLookup _ [] = Nothing
workerJobIdLookup jobId (c:cs) = case getTrd c of
  Nothing -> workerJobIdLookup jobId cs
  Just jobId' -> if jobId' == jobId
    then
      Just (getFst c, getSnd c)
    else workerJobIdLookup jobId cs

updateWaitingList :: JobId -> [(JobId, ReplyChan JobDoneReason)] -> [(JobId, ReplyChan (JobDoneReason))]
updateWaitingList _ [] = []
updateWaitingList jobId (c:cs) =
  if jobId == fst c
    then
      updateWaitingList jobId cs
    else
      c : updateWaitingList jobId cs

removeWorker :: WorkerName -> [(Worker, WorkerName, Maybe JobId)] -> [(Worker, WorkerName, Maybe JobId)]
removeWorker _ [] = []
removeWorker name (c:cs) =
  if getSnd c == name
    then
      cs
    else
      c : removeWorker name cs

startSPC :: IO SPC
startSPC = do
  let initial_state =
        SPCState
          { spcJobCounter = JobId 0,
            spcJobsPending = [],
            spcJobsRunning = [],
            spcJobsDone = [],
            spcWorkerList = [],
            spcWaiting = []
          }
  c <- spawn $ \c -> runSPCM initial_state $ forever $ handleMsg c
  void $ spawn $ timer c
  pure $ SPC c
  where
    timer c _ = forever $ do
      threadDelay 1000000 -- 1 second
      sendTo c MsgTick

-- | Add a job for scheduling.
jobAdd :: SPC -> Job -> IO JobId
jobAdd (SPC c) job =
  requestReply c $ MsgJobAdd job

-- | Asynchronously query the job status.
jobStatus :: SPC -> JobId -> IO JobStatus
jobStatus (SPC c) jobid =
  requestReply c $ MsgJobStatus jobid

-- | Synchronously block until job is done and return the reason.
jobWait :: SPC -> JobId -> IO JobDoneReason
jobWait (SPC c) jobid =
  requestReply c $ MsgJobWait jobid

-- | Asynchronously cancel a job.
jobCancel :: SPC -> JobId -> IO ()
jobCancel (SPC c) jobid =
  sendTo c $ MsgJobCancel jobid

-- | Add a new worker with this name. Fails with 'Left' if a worker
-- with that name already exists.
workerAdd :: SPC -> WorkerName -> IO (Either String Worker)
workerAdd (SPC c) name =
  requestReply c $ MsgWorkerAdd name

-- | Shut down a running worker. No effect if the worker is already
-- terminated.
workerStop :: Worker -> IO ()
workerStop (Worker c) =
  sendTo c MsgWorkerRemove