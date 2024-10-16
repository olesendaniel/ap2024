{-# LANGUAGE InstanceSigs #-}
module APL.Eval
  ( Val (..),
    eval,
    runEval,
    Error,
    Env,
    EvalM (..),
    envEmpty,
    askEnv,
    envExtend,
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

newtype EvalM a = EvalM (Env -> ([String], [(Val,Val)]) -> (([String], [(Val,Val)]), Either Error a))

instance Functor EvalM where
  fmap = liftM

instance Applicative EvalM where
  pure x = EvalM $ \_env s -> (s, Right x)
  (<*>) :: EvalM (a -> b) -> EvalM a -> EvalM b
  (<*>) = ap

instance Monad EvalM where
  (>>=) :: EvalM a -> (a -> EvalM b) -> EvalM b
  EvalM x >>= f = EvalM $ \env s ->
    case x env s of
      (s', Left err) -> (s', Left err)
      (s', Right x') ->
        let EvalM y = f x'
         in y env s'


askEnv :: EvalM Env
askEnv = EvalM $ \env s -> (s, Right env)

localEnv :: (Env -> Env) -> EvalM a -> EvalM a
localEnv f (EvalM m) = EvalM $ \env -> m (f env)

failure :: String -> EvalM a
failure s = EvalM $ \_env st -> (st, Left s)

catch :: EvalM a -> EvalM a -> EvalM a
catch (EvalM m1) (EvalM m2) = EvalM $ \env s ->
  case m1 env s of
    (_, Left _) -> m2 env s
    (s', Right x) -> (s', Right x)

runEval :: EvalM a -> ([String], Either Error a)
runEval (EvalM m) =
  case m envEmpty ([], []) of
    ((s',_), Left err) -> (s', Left err)
    ((s',_), Right a) -> (s', Right a)

evalPrint :: String -> EvalM ()
evalPrint s = EvalM $ \_env (st, x) ->
  ((st ++ [s], x), Right ())

evalIntBinOp :: (Integer -> Integer -> EvalM Integer) -> Exp -> Exp -> EvalM Val
evalIntBinOp f e1 e2 = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (ValInt x, ValInt y) -> ValInt <$> f x y
    (_, _) -> failure "Non-integer operand"

evalIntBinOp' :: (Integer -> Integer -> Integer) -> Exp -> Exp -> EvalM Val
evalIntBinOp' f e1 e2 =
  evalIntBinOp f' e1 e2
  where
    f' x y = pure $ f x y

keyValueLookup :: Val -> [(Val, Val)] -> Either Error Val
keyValueLookup v [] = Left ("Key invalid: " ++ show v)
keyValueLookup v (c:cs) = if v == fst c
  then Right $ snd c
  else keyValueLookup v cs

evalKvGet :: Val -> EvalM Val
evalKvGet v = EvalM $ \_env (x, y) ->
  case keyValueLookup v  y of
    v' -> ((x,y), v')

keyValueRemove :: Val -> [(Val, Val)] -> [(Val, Val)]
keyValueRemove _ [] = []
keyValueRemove v (c:cs) = if v == fst c
  then cs
  else c : keyValueRemove v cs

evalKvPut :: Val -> Val -> EvalM ()
evalKvPut v1 v2 = EvalM $ \_env (x, y) ->
  case keyValueLookup v1 y of
    Left _ -> ((x, y ++ [(v1, v2)]), Right ())
    Right _ ->
      let newY = keyValueRemove v1 y
      in ((x, newY ++ [(v1, v2)]), Right ())

eval :: Exp -> EvalM Val
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
eval (Print s e) =
  case e of
    (CstInt v) -> do
      evalPrint (s ++ ": " ++ show v)
      pure (ValInt v)
    (CstBool v) -> do
      evalPrint (s ++ ": " ++ show v)
      pure (ValBool v)
    _ -> do
      evalPrint (s ++ ": #<fun>")
      eval e
eval (KvPut e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  evalKvPut v1 v2
  pure v2
eval (KvGet e) = do
  v1 <- eval e
  evalKvGet v1

