module APL.Eval_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (Val (..), envEmpty, eval)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
--import Data.Type.Equality (apply)

-- -- Consider this example when you have added the necessary constructors.
-- -- The Y combinator in a form suitable for strict evaluation.
-- yComb :: Exp
-- yComb =
--   Lambda "f" $
--     Apply
--       (Lambda "g" (Apply (Var "g") (Var "g")))
--       ( Lambda
--           "g"
--           ( Apply
--               (Var "f")
--               (Lambda "a" (Apply (Apply (Var "g") (Var "g")) (Var "a")))
--           )
--       )

-- fact :: Exp
-- fact =
--   Apply yComb $
--     Lambda "rec" $
--       Lambda "n" $
--         If
--           (Eql (Var "n") (CstInt 0))
--           (CstInt 1)
--           (Mul (Var "n") (Apply (Var "rec") (Sub (Var "n") (CstInt 1))))

tests :: TestTree
tests =
  testGroup
    "Evaluation"
    [ testCase "Add" $
        eval envEmpty (Add (CstInt 2) (CstInt 5))
          @?= Right (ValInt 7),
      --
      testCase "Add (wrong type)" $
        eval envEmpty (Add (CstInt 2) (CstBool True))
          @?= Left "Non-integer operand",
      --
      testCase "Sub" $
        eval envEmpty (Sub (CstInt 2) (CstInt 5))
          @?= Right (ValInt (-3)),
      --
      testCase "Mul" $
        eval envEmpty (Mul (CstInt 2) (CstInt 5))
          @?= Right (ValInt 10),
      --
      testCase "Div" $
        eval envEmpty (Div (CstInt 7) (CstInt 3))
          @?= Right (ValInt 2),
      --
      testCase "Div0" $
        eval envEmpty (Div (CstInt 7) (CstInt 0))
          @?= Left "Division by zero",
      --
      testCase "Pow" $
        eval envEmpty (Pow (CstInt 2) (CstInt 3))
          @?= Right (ValInt 8),
      --
      testCase "Pow0" $
        eval envEmpty (Pow (CstInt 2) (CstInt 0))
          @?= Right (ValInt 1),
      --
      testCase "Pow negative" $
        eval envEmpty (Pow (CstInt 2) (CstInt (-1)))
          @?= Left "Negative exponent",
      --
      testCase "Eql (right failure)" $
        eval envEmpty (Eql (CstInt 2) (Div (CstInt 2) (CstInt 0)))
          @?= Left "Division by zero",
      --
      testCase "Eql (left failure)" $
        eval envEmpty (Eql (Div (CstInt 2) (CstInt 0)) (CstInt 3))
          @?= Left "Division by zero",
      --
      testCase "Eql (false)" $
        eval envEmpty (Eql (CstInt 2) (CstInt 3))
          @?= Right (ValBool False),
      --
      testCase "Eql (2 bools)" $
        eval envEmpty (Eql (CstBool True) (CstBool True))
          @?= Right (ValBool True),
      --
      testCase "Eql (int and bool)" $
        eval envEmpty (Eql (CstInt 2) (CstBool True))
          @?= Left "Invalid operands to equality",
      --
      testCase "Eql (true)" $
        eval envEmpty (Eql (CstInt 2) (CstInt 2))
          @?= Right (ValBool True),
      --
      testCase "If" $
        eval envEmpty (If (CstBool True) (CstInt 2) (Div (CstInt 7) (CstInt 0)))
          @?= Right (ValInt 2),
      --
      testCase "If (false)" $
        eval envEmpty (If (CstBool False) (CstInt 2) (Add (CstInt 7) (CstInt 1)))
          @?= Right (ValInt 8),
      --
      testCase "If (non-boolean failure)" $
        eval envEmpty (If (CstInt 2) (CstInt 2) (Div (CstInt 7) (CstInt 7)))
          @?= Left "Non-boolean conditional.",
      --
      testCase "If failure" $
        eval envEmpty (If (Div (CstInt 2) (CstInt 0)) (CstInt 2) (Div (CstInt 7) (CstInt 7)))
          @?= Left "Division by zero",
      --
      testCase "Let" $
        eval envEmpty (Let "x" (Add (CstInt 2) (CstInt 3)) (Var "x"))
          @?= Right (ValInt 5),
      --
      testCase "Let (shadowing)" $
        eval
          envEmpty
          ( Let
              "x"
              (Add (CstInt 2) (CstInt 3))
              (Let "x" (CstBool True) (Var "x"))
          )
          @?= Right (ValBool True),
      --
      testCase "Let error" $
        eval envEmpty (Let "x" (Div (CstInt 2) (CstInt 0)) (Var "x"))
          @?= Left "Division by zero",
      --
      testCase "Lambda" $
        eval envEmpty (Let "x" (CstInt 2) (Lambda "y" (Add (Var "x") (Var "y"))))
          @?= Right (ValFun [("x", ValInt 2)] "y" (Add (Var "x") (Var"y"))),
      -- 
      testCase "Lambda, empty env" $
        eval envEmpty (Lambda "x" (CstInt 7)) 
          @?= Right (ValFun [] "x" (CstInt 7)),
      --
      testCase "Apply" $
        eval envEmpty (Apply (Let "x" (CstInt 2) (Lambda "y" (Add (Var "x") (Var "y")))) (CstInt 3))
          @?= Right (ValInt 5),
      --
      testCase "Apply, no ValFun" $
        eval envEmpty (Apply (CstInt 2) (CstInt 2)) 
          @?= Left "error non ValFun",
      --
      testCase "Apply, ValFun as second Exp" $
        eval envEmpty (Apply (CstInt 3) (Let "x" (CstInt 2) (Lambda "y" (Add (Var "x") (Var "y"))))) 
          @?= Left "error non ValFun",
      --
      testCase "Apply, Both Lambda (ValFun) Inputs" $
        eval envEmpty (Apply (Lambda "x" (Add (Var "x") (Var "y"))) (Lambda "y" (Add (Var "x") (Var "y"))))
          @?= Left "Unknown variable: y",
      --
      testCase "Apply error" $
        eval envEmpty (Apply (Lambda "x" (Add (Var "x") (Var "y"))) (Div (CstInt 3) (CstInt 0)))
          @?= Left "Division by zero",
      --
      testCase "Apply error 2" $
        eval envEmpty (Apply (Div (CstInt 3) (CstInt 0)) (Div (CstInt 3) (CstInt 0)))
          @?= Left "Division by zero",
      --
      testCase "Try-Catch" $
        eval envEmpty (TryCatch (CstInt 0) (CstInt 1))
          @?= Right (ValInt 0),
      --
      testCase "Try-Catch Var Missing" $
        eval envEmpty (TryCatch (Var "missing") (CstInt 1))
          @?= Right (ValInt 1),
      --
      testCase "Try-Catch e1/e2 Errors" $
        eval envEmpty (TryCatch (Var "missing") (Var "missing"))
        @?= Left "Unknown variable: missing"

    ]
