module APL.InterpIO (runEvalIO) where

import APL.Monad
import APL.Util
import System.Directory (removeFile)
import System.IO (hFlush, stdout)

-- Converts a string into a value. Only 'ValInt's and 'ValBool' are supported.
readVal :: String -> Maybe Val
readVal = unserialize

-- 'prompt s' prints 's' to the console and then reads a line from stdin.
prompt :: String -> IO String
prompt s = do
  putStr s
  hFlush stdout
  getLine

-- 'writeDB dbFile s' writes the 'State' 's' to the file 'db'.
writeDB :: FilePath -> State -> IO ()
writeDB db s =
  writeFile db $ serialize s

-- 'readDB db' reads the database stored in 'db'.
readDB :: FilePath -> IO (Either Error State)
readDB db = do
  ms <- readFile db
  case unserialize ms of
    Just s -> pure $ pure s
    Nothing -> pure $ Left "Invalid DB."

-- 'copyDB db1 db2' copies 'db1' to 'db2'.
copyDB :: FilePath -> FilePath -> IO ()
copyDB db db' = do
  s <- readFile db
  writeFile db' s

-- Removes all key-value pairs from the database file.
clearDB :: IO ()
clearDB = writeFile dbFile ""

-- The name of the database file.
dbFile :: FilePath
dbFile = "db.txt"

-- Creates a fresh temporary database, passes it to a function returning an
-- IO-computation, executes the computation, deletes the temporary database, and
-- finally returns the result of the computation. The temporary database file is
-- guaranteed fresh and won't have a name conflict with any other files.
withTempDB :: (FilePath -> IO a) -> IO a
withTempDB m = do
  tempDB <- newTempDB -- Create a new temp database file.
  res <- m tempDB -- Run the computation with the new file.
  removeFile tempDB -- Delete the temp database file.
  pure res -- Return the result of the computation.

runEvalIO :: EvalM a -> IO (Either Error a)
runEvalIO evalm = do
  clearDB
  runEvalIO' envEmpty dbFile evalm
  where
    runEvalIO' :: Env -> FilePath -> EvalM a -> IO (Either Error a)
    runEvalIO' _ _ (Pure x) = pure $ pure x
    runEvalIO' r db (Free (ReadOp k)) = runEvalIO' r db $ k r
    runEvalIO' r db (Free (StateGetOp k)) = do
      temp <- readDB db
      case temp of
        Left e -> pure $ Left e
        Right res -> runEvalIO' r db $ k res
    runEvalIO' r db (Free (StatePutOp s k)) = do
      writeDB db s
      runEvalIO' r db k
    runEvalIO' r db (Free (PrintOp p m)) = do
      putStrLn p
      runEvalIO' r db m
    runEvalIO' r db (Free (TryCatchOp m1 m2)) = do
      result <- runEvalIO' r db m1
      case result of
        Left _ -> runEvalIO' r db m2
        Right _ -> runEvalIO' r db m1
    runEvalIO' r db (Free (KvGetOp v va)) = do
      temp <- readDB db
      case temp of
        Left e -> pure $ Left e
        Right s ->
          case lookup v s of
            Just val -> runEvalIO' r db $ va val
            Nothing -> do
              str <- prompt (show v ++ " not in DB, pick new key")
              case readVal str of
                Just val' -> runEvalIO' r db $ va val'
                Nothing -> pure $ Left "Key not valid"
    runEvalIO' r db (Free (KvPutOp v1 v2 a)) = do
      temp <- readDB db
      case temp of
        Left e -> pure $ Left e
        Right s ->
          case lookup v1 s of
            Just _ -> do
              let s' = keyValueRemove v1 s
              let s'' = s' ++ [(v1,v2)]
              let _ = writeDB db (s'' ++ [(v1,v2)])
                in runEvalIO' r db a
            Nothing -> do
              writeDB db (s ++ [(v1,v2)])
              runEvalIO' r db a
    runEvalIO' r db (Free (TransactionOp m a)) =
      withTempDB $ \tempDB -> do
        copyDB db tempDB
        do
          temp <- runEvalIO' r tempDB m
          case temp of
            Left _ -> runEvalIO' r db a
            Right _ -> do
              copyDB tempDB db
              runEvalIO' r db a
    runEvalIO' _ _ (Free (ErrorOp e)) = pure $ Left e


