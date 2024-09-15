module APL.AST
  ( VName,
    Exp (..),
    printExp,
  )
where

type VName = String

data Exp
  = CstInt Integer
  | CstBool Bool
  | Add Exp Exp
  | Sub Exp Exp
  | Mul Exp Exp
  | Div Exp Exp
  | Pow Exp Exp
  | Eql Exp Exp
  | If Exp Exp Exp
  | Var VName
  | Let VName Exp Exp
  | Lambda VName Exp
  | Apply Exp Exp
  | TryCatch Exp Exp
  deriving (Eq, Show)

printExp :: Exp -> String
printExp (CstInt i) = if i < 0 then "(" ++ show i ++ ")" else show i 
printExp (CstBool i) = show i
printExp (Var name) = name
printExp (Add e1 e2) = "(" ++ printExp e1 ++ "+" ++ printExp e2 ++ ")"
printExp (Sub e1 e2) = "(" ++ printExp e1 ++ "-" ++ printExp e2 ++ ")"
printExp (Mul e1 e2) = "(" ++ printExp e1 ++ "*" ++ printExp e2 ++ ")"
printExp (Div e1 e2) = "(" ++ printExp e1 ++ "/" ++ printExp e2 ++ ")"
printExp (Pow e1 e2) = "(" ++ printExp e1 ++ "**" ++ printExp e2 ++ ")"
printExp (Eql e1 e2) = "(" ++ printExp e1 ++ "==" ++ printExp e2 ++ ")"
printExp (If e1 e2 e3) = "(if " ++ printExp e1 ++ " then " ++ printExp e2 ++ " else " ++ printExp e3 ++ ")"
printExp (Let var e1 e2) = "(" ++ "Let " ++ var ++ "=" ++ printExp e1 ++ " in " ++ printExp e2 ++ ")"
printExp (Lambda name e1) = "(\\" ++ name ++ "->" ++ printExp e1 ++ ")"
printExp (Apply e1 e2) = printExp e1 ++ " " ++ printExp e2
printExp (TryCatch e1 e2) = "(try " ++ printExp e1 ++ " catch " ++ printExp e2 ++ ")"
