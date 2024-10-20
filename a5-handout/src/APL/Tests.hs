module APL.Tests
  ( properties
  )
where

import APL.AST (Exp (..), subExp, VName, printExp)
import APL.Error (isVariableError, isDomainError, isTypeError)
import APL.Check (checkExp,)
import Test.QuickCheck
  ( Property
  , Gen
  , Arbitrary (arbitrary, shrink)
  , property
  , cover
  , checkCoverage
  , oneof
  , sized
  , frequency
  , elements
  --, quickCheck
  --, withMaxSuccess
  )
import APL.Parser(parseAPL, keywords)
import APL.Eval(runEval, eval)


instance Arbitrary Exp where
  arbitrary = sized $ \size -> genExp size []

  shrink (Add e1 e2) =
    e1 : e2 : [Add e1' e2 | e1' <- shrink e1] ++ [Add e1 e2' | e2' <- shrink e2]
  shrink (Sub e1 e2) =
    e1 : e2 : [Sub e1' e2 | e1' <- shrink e1] ++ [Sub e1 e2' | e2' <- shrink e2]
  shrink (Mul e1 e2) =
    e1 : e2 : [Mul e1' e2 | e1' <- shrink e1] ++ [Mul e1 e2' | e2' <- shrink e2]
  shrink (Div e1 e2) =
    e1 : e2 : [Div e1' e2 | e1' <- shrink e1] ++ [Div e1 e2' | e2' <- shrink e2]
  shrink (Pow e1 e2) =
    e1 : e2 : [Pow e1' e2 | e1' <- shrink e1] ++ [Pow e1 e2' | e2' <- shrink e2]
  shrink (Eql e1 e2) =
    e1 : e2 : [Eql e1' e2 | e1' <- shrink e1] ++ [Eql e1 e2' | e2' <- shrink e2]
  shrink (If cond e1 e2) =
    e1 : e2 : [If cond' e1 e2 | cond' <- shrink cond] ++ [If cond e1' e2 | e1' <- shrink e1] ++ [If cond e1 e2' | e2' <- shrink e2]
  shrink (Let x e1 e2) =
    e1 : [Let x e1' e2 | e1' <- shrink e1] ++ [Let x e1 e2' | e2' <- shrink e2]
  shrink (Lambda x e) =
    [Lambda x e' | e' <- shrink e]
  shrink (Apply e1 e2) =
    e1 : e2 : [Apply e1' e2 | e1' <- shrink e1] ++ [Apply e1 e2' | e2' <- shrink e2]
  shrink (TryCatch e1 e2) =
    e1 : e2 : [TryCatch e1' e2 | e1' <- shrink e1] ++ [TryCatch e1 e2' | e2' <- shrink e2]
  shrink _ = []

genVar :: Gen VName
genVar = do
  c1 <- elements ['a' .. 'z']
  c2 <- elements $ ['a' .. 'z'] ++ ['0' .. '9']
  c3 <- elements $ ['a' .. 'z'] ++ ['0' .. '9']
  c4 <- elements $ ['a' .. 'z'] ++ ['0' .. '9']
  tmp <- elements [c1 : c2 : [], c1 : c2 : c3 : [], c1 : c2 : c3 : c4 : []]
  if tmp `elem` keywords
    then genVar
  else pure tmp



genExp :: Int -> [VName] -> Gen Exp
genExp 0 _ =
    oneof [ CstInt <$> abs <$> arbitrary
          , CstBool <$> arbitrary]

genExp size v =
  do
    v' <- genVar
    frequency $
      [(100, CstInt <$> abs <$> arbitrary),
      (100, CstBool <$> arbitrary),
      (100, Add <$> genExp halfSize v <*> genExp halfSize v),
      (100, Sub <$> genExp halfSize v <*> genExp halfSize v),
      (100, Mul <$> genExp halfSize v <*> genExp halfSize v),
      (100, Div <$> genExp halfSize v <*> genExp halfSize v),
      (100, Pow <$> genExp halfSize v <*> genExp halfSize v),
      (100, Eql <$> genExp halfSize v <*> genExp halfSize v),
      (100, If <$> genExp thirdSize v <*> genExp thirdSize v <*> genExp thirdSize v),
      (5, Var <$> genVar),
      (150, Let <$> pure v' <*> genExp halfSize (v' : v) <*> genExp halfSize (v' : v)),
      (150, Lambda <$> pure v' <*> genExp (size - 1) (v' : v)),
      (100, Apply <$> genExp halfSize v <*> genExp halfSize v),
      (100, TryCatch <$> genExp halfSize v <*> genExp halfSize v)
      ] ++
      case v of
        [] -> []
        _ -> [(100, Var <$> elements v)]

  where
    halfSize = size `div` 2
    thirdSize = size `div` 3

expCoverage :: Exp -> Property
expCoverage e = checkCoverage
  . cover 20 (any isDomainError (checkExp e)) "domain error"
  . cover 20 (not $ any isDomainError (checkExp e)) "no domain error"
  . cover 20 (any isTypeError (checkExp e)) "type error"
  . cover 20 (not $ any isTypeError (checkExp e)) "no type error"
  . cover 5 (any isVariableError (checkExp e)) "variable error"
  . cover 70 (not $ any isVariableError (checkExp e)) "no variable error"
  . cover 50 (or [2 <= n && n <= 4 | Var v <- subExp e, let n = length v]) "non-trivial variable"
  $ ()

parsePrinted :: Exp -> Bool
parsePrinted e =
  let print1 = printExp e
  in case parseAPL "" print1 of
    Left _ -> False
    Right e1 -> e == e1


onlyCheckedErrors :: Exp -> Bool
onlyCheckedErrors e = do
  let listOfErrors = checkExp e
  let res = runEval $ eval e
  case res of
    Left err -> err `elem` listOfErrors
    Right _ -> True

properties :: [(String, Property)]
properties =
  [ ("expCoverage", property expCoverage)
  , ("onlyCheckedErrors", property onlyCheckedErrors)
  , ("parsePrinted", property parsePrinted)
  ]
