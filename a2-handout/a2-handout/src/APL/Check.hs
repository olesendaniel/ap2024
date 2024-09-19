{-# LANGUAGE InstanceSigs #-}
module APL.Check (checkExp, Error) where

import APL.AST (Exp (..), VName)
import Control.Monad (ap, liftM)
import APL.Eval (Env, eval, EvalM (..), runEval, Val (..), envEmpty)
import Foreign.C (errnoToIOError)
import Data.Functor.Classes (eq1)

type Error = String

--Error synonymous with String
newtype CheckM a = CheckM (Env -> Either Error a)

instance Functor CheckM where
  fmap :: (a -> b) -> CheckM a -> CheckM b
  fmap = liftM

instance Applicative CheckM where
  pure a = CheckM $ \_env -> Right a
  (<*>) :: CheckM (a -> b) -> CheckM a -> CheckM b 
  (<*>) = ap

instance Monad CheckM where
  (>>=) :: CheckM a -> (a -> CheckM b) -> CheckM b
  CheckM x >>= f = CheckM $ \env ->
    case x env of
      Left err -> Left err
      Right x' -> 
        let CheckM y = f x'
        in y env 

askEnv :: CheckM Env
askEnv = CheckM $ \env -> Right env

failure :: String -> CheckM a
failure s = CheckM $ \_env -> Left s

envLookup :: VName -> Env -> Maybe Error
envLookup v env = case lookup v env of
    Just _ -> Nothing
    Nothing -> Just "Unbound variable"

localEnv :: (Env -> Env) -> CheckM a -> CheckM a
localEnv f (CheckM m) = CheckM $ \env -> m (f env)

check2 :: Exp -> Exp -> CheckM ()
check2 e1 e2 = do 
    check e1 
    check e2

check3 :: Exp -> Exp -> Exp -> CheckM ()
check3 e1 e2 e3 = do 
    check e1 
    check e2
    check e3



runCheckM :: CheckM () -> Maybe Error
runCheckM (CheckM m) = case m envEmpty of 
    Left err -> Just err
    Right _ -> Nothing

check :: Exp -> CheckM ()
check (CstInt _) = pure ()
check (CstBool _) = pure () 
check (Var x) = failure ("Unknown variable: " ++ x)
check (Add e1 e2) = check2 e1 e2
check (Sub e1 e2) = check2 e1 e2 
check (Mul e1 e2) = check2 e1 e2
check (Div e1 e2) = check2 e1 e2
check (Pow e1 e2) = check2 e1 e2
check (Eql e1 e2) = check2 e1 e2
check (If e1 e2 e3) = check3 e1 e2 e3
check (Let v1 e1 e2) = undefined
check (Lambda v e1) = undefined
check (Apply e1 e2) = check2 e1 e2
check (TryCatch e1 e2) = check2 e1 e2
check (Print _ e1) = check e1
check (KvPut e1 e2) = check2 e1 e2
check (KvGet e1 ) = check e1


checkExp :: Exp -> Maybe Error
checkExp = runCheckM . check




{-
-- Old check function:

    Var x -> do 
        env <- askEnv
        case envLookup x env of 
            Just _ -> pure ()
            Nothing -> failure $ "Unknown variable " ++ x
    Let v e1 e2 -> undefined
    Lambda v e1 -> undefined
    _ -> undefined

checkExp :: Exp -> Maybe Error
checkExp e = case e of 
    CstInt _ -> Nothing
    CstBool _ -> Nothing
    Var x -> runCheckM $ check $ Var x
    Add e1 e2 ->
      let x1 = checkExp e1
      in case x1 of 
        Just err -> Just err
        _ -> let x2 = checkExp e2
            in case x2 of 
                Just err -> Just err
                _ -> Nothing

    Sub e1 e2 -> do 
      checkExp e1
      checkExp e2
    Div e1 e2 -> do
      checkExp e1
      checkExp e2
    Mul e1 e2 -> do
      checkExp e1
      checkExp e2
    Pow e1 e2 -> do
      checkExp e1
      checkExp e2
    Eql e1 e2 -> do
      checkExp e1
      checkExp e2
    If e1 e2 e3 -> do
      checkExp e1
      checkExp e2
      checkExp e3
    Let v1 e1 e2 -> undefined
    Lambda v e1 -> undefined
    Apply e1 e2 -> undefined
    TryCatch e1 e2 -> undefined
    Print _ e1 -> undefined
    KvPut e1 e2 -> undefined
    KvGet e1 -> undefined

-}
