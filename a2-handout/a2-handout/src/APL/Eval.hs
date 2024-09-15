{-# LANGUAGE InstanceSigs #-}
module APL.Eval
  ( Val (..),
    eval,
    runEval,
    Error,
  )
where

import APL.AST (Exp (..), VName)
import Control.Monad (ap, liftM)

data Val
  = ValInt Integer
  | ValBool Bool
  | ValFun Env VName Exp
  deriving (Eq, Show)

type Env = [(VName, Val)]

envEmpty :: Env
envEmpty = []

envExtend :: VName -> Val -> Env -> Env
envExtend v val env = (v, val) : env

envLookup :: VName -> Env -> Maybe Val
envLookup v env = lookup v env

type Error = String

newtype EvalM a s = EvalM (Env -> s -> ([String], Either Error a))

--newtype State s a = State (s -> (a, s))
--newtype Reader env a = Reader (env -> a)
--newtype RS env s a = RS (env -> s -> (a, s))
instance Functor (EvalM s) where
  fmap = liftM

instance Applicative (EvalM s) where
  pure x = EvalM $ \_env -> (["Temp"], Right x)
  (<*>) :: EvalM (a -> b) -> EvalM a -> EvalM b 
  (<*>) = ap

instance Monad (EvalM s) where
  EvalM x >>= f = EvalM $ \env ->
    case x env of
      (s, Left err) -> (s, Left err)
      (s, Right x') ->
        let EvalM y = f x'    
         in y env 

askEnv :: (EvalM s) Env
askEnv = EvalM $ \env -> Right env

localEnv :: (Env -> Env) -> (EvalM s) a -> (EvalM s) a
localEnv f (EvalM m) = EvalM $ \env -> m (f env)

failure :: String -> (EvalM s) a
failure s = EvalM $ \_env -> Left s

catch :: (EvalM s) a -> (EvalM s) a -> (EvalM s) a
catch (EvalM m1) (EvalM m2) = EvalM $ \env ->
  case m1 env of
    Left _ -> m2 env
    Right x -> Right x

runEval :: (EvalM s) a -> ([String], Either Error a)
--runEval :: EvalM a -> Either Error a
runEval (EvalM m) = m envEmpty

evalIntBinOp :: (Integer -> Integer -> (EvalM s) Integer) -> Exp -> Exp -> (EvalM s) Val
evalIntBinOp f e1 e2 = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (ValInt x, ValInt y) -> ValInt <$> f x y
    (_, _) -> failure "Non-integer operand"

evalIntBinOp' :: (Integer -> Integer -> Integer) -> Exp -> Exp -> (EvalM s) Val
evalIntBinOp' f e1 e2 =
  evalIntBinOp f' e1 e2
  where
    f' x y = pure $ f x y

eval :: Exp -> (EvalM s) Val
eval (CstInt x) = pure $ ValInt x
eval (CstBool b) = pure $ ValBool b
eval (Var v) = do
  env <- askEnv
  case envLookup v env of
    Just x -> pure x
    Nothing -> failure $ "Unknown variable: " ++ v
eval (Add e1 e2) = evalIntBinOp' (+) e1 e2
eval (Sub e1 e2) = evalIntBinOp' (-) e1 e2
eval (Mul e1 e2) = evalIntBinOp' (*) e1 e2
eval (Div e1 e2) = evalIntBinOp checkedDiv e1 e2
  where
    checkedDiv _ 0 = failure "Division by zero"
    checkedDiv x y = pure $ x `div` y
eval (Pow e1 e2) = evalIntBinOp checkedPow e1 e2
  where
    checkedPow x y =
      if y < 0
        then failure "Negative exponent"
        else pure $ x ^ y
eval (Eql e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (ValInt x, ValInt y) -> pure $ ValBool $ x == y
    (ValBool x, ValBool y) -> pure $ ValBool $ x == y
    (_, _) -> failure "Invalid operands to equality"
eval (If cond e1 e2) = do
  cond' <- eval cond
  case cond' of
    ValBool True -> eval e1
    ValBool False -> eval e2
    _ -> failure "Non-boolean conditional."
eval (Let var e1 e2) = do
  v1 <- eval e1
  localEnv (envExtend var v1) $ eval e2
eval (Lambda var body) = do
  env <- askEnv
  pure $ ValFun env var body
eval (Apply e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (ValFun f_env var body, arg) ->
      localEnv (const $ envExtend var arg f_env) $ eval body
    (_, _) ->
      failure "Cannot apply non-function"
eval (TryCatch e1 e2) =
  eval e1 `catch` eval e2
