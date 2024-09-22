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
      -- Test case: Checking a constant integer (should pass)
      testPos (CstInt 2),

       -- Test case: Checking a constant bool (should pass)
      testPos (CstBool True),

      -- Test case: Using an undefined variable (should fail)
      testNeg (Var "x"),

      -- Test case: Lambda function with a bound variable (should pass)
      testPos (Lambda "x" (Var "x")),

      -- Test case: Lambda function with an unbound variable (should fail)
      testNeg (Lambda "x" (Var "y")),

      -- Test case: Let expression with a Lambda using an outer variable (should pass)
      testPos (Let "x" (CstInt 2) (Lambda "y" (Add (Var "x") (Var "y")))),

      -- Test case: Let expression defining a variable and using it (should pass)
      testPos (Let "x" (Add (CstInt 2) (CstInt 3)) (Var "x")),

      -- Test case: Nested Let expressions (should pass)
      testPos (Let "x" (Add (CstInt 2) (CstInt 3)) (Let "y" (Add (CstInt 4) (Var "x")) (Var "y"))),

      -- Test case: Let expression with variable shadowing (should pass)
      testPos (Let "x" (CstInt 2) (Let "x" (CstInt 3) (Var "x"))),

      -- Test case: Nested Let expression with multiple variables (should pass)
      testPos (Let "x" (CstInt 1) (Let "y" (CstInt 2) (Add (Var "x") (Var "y")))),

      -- Test case: Adding an undefined variable to an integer (should fail)
      testNeg (Add (Var "x") (CstInt 3)),

      -- Test case: Lambda shadowing an outer variable (should pass)
      testPos (Let "x" (CstInt 2) (Lambda "x" (Add (Var "x") (CstInt 3)))),

      -- Test case: Lambda with an undefined variable (should fail)
      testNeg (Lambda "x" (Add (Var "x") (Var "y"))),

      -- Test case: Applying a lambda to an integer (should pass)
      testPos (Apply (Lambda "x" (Add (Var "x") (CstInt 3))) (CstInt 5)),

      -- Test case: Nested Let bindings with multiple variables (should pass)
      testPos (Let "x" (CstInt 1) (Let "y" (CstInt 2) (Let "z" (Add (Var "x") (Var "y")) (Var "z")))),

      -- Test case: Let with undefined variable in body (should fail)
      testNeg (Let "x" (CstInt 1) (Add (Var "x") (Var "y"))),

      -- Test If and check3 helper function (should pass)
      testPos (If (Eql (CstBool True) (CstBool True)) (Add (CstInt 2)(CstInt 3)) (Sub (CstInt 2)(CstInt 3))),

      -- Test KvPut/kvGet (should pass)
      testPos (Apply (KvPut (CstInt 0)(CstInt 10))(KvGet (CstInt 0))),

      -- Test kvGet (should fail)
      testNeg (KvGet (CstInt 0))
    ]
