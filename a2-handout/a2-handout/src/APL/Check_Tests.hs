module APL.Check_Tests (tests) where

import APL.AST (Exp (..))
import APL.Check (checkExp)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

-- Assert that the provided expression should pass the type checker.
testPos :: Exp -> TestTree
testPos e =
  testCase (show e) $
    checkExp e @?= Nothing

-- Assert that the provided expression should fail the type checker.
testNeg :: Exp -> TestTree
testNeg e =
  testCase (show e) $
    case checkExp e of
      Nothing -> assertFailure "expected error"
      Just _ -> pure ()

tests :: TestTree
tests =
  testGroup
    "Checking"
    [
      testCase "test1" $
        checkExp (CstInt 2)
        @?= Nothing,
      --
      testCase "Fail" $
        checkExp (Var "x")
        @?= Just "Unknown variable: x",
      --
      testCase "Lambda" $
        checkExp (Lambda "x" (Var "x"))
        @?= Nothing,
      --
      testCase "Lambda Fail" $
        checkExp (Lambda "x" (Var "y"))
        @?= Just "Unknown variable: y",
      --
      testCase "Lambda Simple" $
        checkExp (Let "x" (CstInt 2) (Lambda "y" (Add (Var "x") (Var "y"))))
        @?= Nothing,
      --
      testCase "Lambda Simple 2" $
        checkExp (Let "x" (CstInt 2) (Lambda "y" (Add (Var "x") (CstInt 2))))
        @?= Nothing,
      --
      testCase "Let" $
        checkExp (Let "x" (Add (CstInt 2) (CstInt 3)) (Var "x"))
        @?= Nothing,
      --
      testCase "Let 2" $
        checkExp (Let "x" (Add (CstInt 2) (CstInt 3))  (Add (CstInt 3) (Var "x")))
        @?= Nothing,
      --
      testCase "Let 3" $
        checkExp (Let "x" (Add (CstInt 2) (CstInt 3))  (Let "y" (Add (CstInt 4) (Var "x")) (Var "y")))
        @?= Nothing

        
    ]
