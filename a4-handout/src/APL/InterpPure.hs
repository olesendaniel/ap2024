module APL.InterpPure (runEval) where

import APL.Monad

runEval :: EvalM a -> ([String], Either Error a)
runEval = runEval' envEmpty stateInitial
  where
    runEval' :: Env -> State -> EvalM a -> ([String], Either Error a)
    runEval' _ _ (Pure x) = ([], pure x)
    runEval' r s (Free (ReadOp k)) = runEval' r s $ k r
    runEval' r s (Free (StateGetOp k)) = runEval' r s $ k s
    runEval' r _ (Free (StatePutOp s' m)) = runEval' r s' m
    runEval' r s (Free (PrintOp p m)) =
      let (ps, res) = runEval' r s m
       in (p : ps, res)
    runEval' r s (Free (TryCatchOp m1 m2)) = case runEval' r s m1 of
      (_, Left _) -> runEval' r s m2
      (_, Right _) -> runEval' r s m1
    runEval' r s (Free (KvGetOp v va)) = case lookup v s of
      Just val -> runEval' r s $ va val
      Nothing -> ([], Left ("Key not in state: " ++ show v))
    runEval' r s (Free (KvPutOp v1 v2 a)) = case lookup v1 s of
      Just _ -> do
        let s' = keyValueRemove v1 s
        let s'' = s' ++ [(v1, v2)]
         in runEval' r s'' a
      Nothing ->
        let s' = s ++ [(v1, v2)]
         in runEval' r s' a
    runEval' r s (Free (TransactionOp m a)) =
      case runEval' r s (m >> getState) of
          (p,Left _) -> let (p', res) = runEval' r s a
            in (p ++ p', res)
          (p,Right s') -> let (p', res) = runEval' r s' a
            in (p ++ p', res)
    runEval' _ _ (Free (ErrorOp e)) = ([], Left e)

