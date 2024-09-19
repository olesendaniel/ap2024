{-# LANGUAGE InstanceSigs #-}
module APL.Check (checkExp, Error) where

import APL.AST (Exp (..), VName)
import Control.Monad (ap, liftM)
import APL.Eval (Env, eval, EvalM (..), runEval, Val (..))

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
{-
askEnv :: CheckM Env
askEnv = CheckM $ \env -> Right env

failure :: String -> CheckM a
failure s = CheckM $ \_env -> Left s 

envLookup :: VName -> Env -> Maybe Error
envLookup v env = case lookup v env of
    Just _ -> Nothing
    Nothing -> Just "Unbound variable"
-}

     -- let x' = x env 
     --   CheckM y = f x'
     -- in y env

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

runCheckM :: CheckM a -> Maybe a
runCheckM (CheckM m) =
  case m of 
    Left err -> Nothing  
    Right x -> Just x

check :: Exp -> CheckM ()
check e =  undefined
     
  
     


checkExp :: Exp -> Maybe Error
checkExp e = runCheckM(check e)
