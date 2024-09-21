{-# LANGUAGE InstanceSigs #-}
module APL.Check (checkExp, Error) where

import APL.AST (Exp (..), VName)
import Control.Monad (ap, liftM)
import APL.Eval (Env, eval, EvalM (..), runEval, Val (..), envEmpty, envExtend)
import Foreign.C (errnoToIOError)
import Data.Functor.Classes (eq1)
import Test.Tasty.Runners (Outcome(Failure))

type Error = String

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

envLookup :: VName -> Env -> Maybe Val
envLookup v env = case lookup v env of
    Just x -> Just x
    Nothing -> Nothing

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
check (Var x) = do
  env <- askEnv
  case envLookup x env of 
    Just _ -> pure ()
    Nothing -> failure ("Unknown variable: " ++ x)
check (Add e1 e2) = check2 e1 e2
check (Sub e1 e2) = check2 e1 e2
check (Mul e1 e2) = check2 e1 e2
check (Div e1 e2) = check2 e1 e2
check (Pow e1 e2) = check2 e1 e2
check (Eql e1 e2) = check2 e1 e2
check (If e1 e2 e3) = check3 e1 e2 e3
check (Let var e1 e2) = do
  check e1
  localEnv (envExtend var (ValInt 0)) $ check e2
check (Lambda var e1) = localEnv (envExtend var (ValInt 0)) $ check e1
check (Apply e1 e2) = check2 e1 e2
check (TryCatch e1 e2) = check2 e1 e2
check (Print _ e1) = check e1
check (KvPut e1 e2) = check2 e1 e2
check (KvGet e1 ) = check e1


checkExp :: Exp -> Maybe Error
checkExp = runCheckM . check
 