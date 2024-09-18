{-# LANGUAGE InstanceSigs #-}
module APL.Check (checkExp, Error) where

import APL.AST (Exp (..), VName)
import Control.Monad (ap, liftM)
import APL.Eval (Env, eval, EvalM (..), runEval, envEmpty)

type Error = String

--Error synonymous with String
newtype CheckM a = CheckM (Env -> a)

instance Functor CheckM where
  fmap :: (a -> b) -> CheckM a -> CheckM b
  fmap = liftM

instance Applicative CheckM where
  pure a = CheckM $ \_env -> a
  (<*>) :: CheckM (a -> b) -> CheckM a -> CheckM b 
  (<*>) = ap

instance Monad CheckM where
  (>>=) :: CheckM a -> (a -> CheckM b) -> CheckM b
  CheckM x >>= f = CheckM $ \env ->
    let x' = x env 
        CheckM y = f x'
    in y env

--
-- let x' = f $ x env   
    
    
  --CheckM (Just x) >>= f = fx


  --CheckM x >>= f = CheckM $ \env -> 
  --  case x env of
  --    Just e -> Just e
  --    Nothing -> 
  --      let CheckM y = f undefined
  --      in y env 
        

    --y <- x
    --return f y
    --let y =  


{-
check :: Exp -> CheckM ()
check e = CheckM $ \_env -> 
    case runEval $ eval e of 
        (_, Left err) -> CheckM envEmpty err
        (_, Right _) -> undefined
-}

checkExp :: Exp -> Maybe Error
checkExp e = case runEval $ eval e of 
    (_, Left err) -> Just err
    (_, Right _) -> Nothing
