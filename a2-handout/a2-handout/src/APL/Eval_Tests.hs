module APL.Eval_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (Error, Val (..), eval, runEval)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
{-
eval' :: Exp -> (([String], [(Val,Val)]), Either Error a)
eval' = runEval . eval

evalTests :: TestTree
evalTests =
  testGroup
    "Evaluation"
    [ testCase "Add" $
        eval' (Add (CstInt 2) (CstInt 5))
          @?= Right (ValInt 7),
      --
      testCase "Add (wrong type)" $
        eval' (Add (CstInt 2) (CstBool True))
          @?= Left "Non-integer operand",
      --
      testCase "Sub" $
        eval' (Sub (CstInt 2) (CstInt 5))
          @?= Right (ValInt (-3)),
      --
      testCase "Div" $
        eval' (Div (CstInt 7) (CstInt 3))
          @?= Right (ValInt 2),
      --
      testCase "Div0" $
        eval' (Div (CstInt 7) (CstInt 0))
          @?= Left "Division by zero",
      --
      testCase "Pow" $
        eval' (Pow (CstInt 2) (CstInt 3))
          @?= Right (ValInt 8),
      --
      testCase "Pow0" $
        eval' (Pow (CstInt 2) (CstInt 0))
          @?= Right (ValInt 1),
      --
      testCase "Pow negative" $
        eval' (Pow (CstInt 2) (CstInt (-1)))
          @?= Left "Negative exponent",
      --
      testCase "Eql (false)" $
        eval' (Eql (CstInt 2) (CstInt 3))
          @?= Right (ValBool False),
      --
      testCase "Eql (true)" $
        eval' (Eql (CstInt 2) (CstInt 2))
          @?= Right (ValBool True),
      --
      testCase "If" $
        eval' (If (CstBool True) (CstInt 2) (Div (CstInt 7) (CstInt 0)))
          @?= Right (ValInt 2),
      --
      testCase "Let" $
        eval' (Let "x" (Add (CstInt 2) (CstInt 3)) (Var "x"))
          @?= Right (ValInt 5),
      --
      testCase "Let (shadowing)" $
        eval'
          ( Let
              "x"
              (Add (CstInt 2) (CstInt 3))
              (Let "x" (CstBool True) (Var "x"))
          )
          @?= Right (ValBool True),
      --
      testCase "Lambda/Apply" $
        eval'
          (Apply (Lambda "x" (Mul (Var "x") (Var "x"))) (CstInt 4))
          @?= Right (ValInt 16),
      --
      testCase "TryCatch" $
        eval'
          (TryCatch (Div (CstInt 7) (CstInt 0)) (CstBool True))
          @?= Right (ValBool True)
    ]-}
evalMonad :: TestTree
evalMonad =
  testGroup
    "Evaluation" 
    [
      testCase "runEval Simple" $ 
        runEval 
          (eval $ Print "foo" $ CstInt 2)
          @?= (["foo: 2"],Right (ValInt 2)),
      --
      testCase "runEval Simple" $ 
        runEval 
          (eval $ Print "foo" $ CstBool True )
          @?= (["foo: True"] ,Right (ValBool True)),
      --
      testCase "runEval Let" $
        runEval
          (eval $ Let "x" (Print "foo" $ CstInt 2) (Print "bar" $ CstInt 3))
          @?= (["foo: 2","bar: 3"],Right (ValInt 3)),
      --
      testCase "runEval function" $
        runEval 
          (eval $ Print "foo" $ Lambda "x" (Var "x"))
          @?= (["foo: #<fun>"],Right (ValFun [] "x" (Var "x"))),
      --
      testCase "runEval Error" $
        runEval
          (eval $ Let "x" (Print "foo" $ CstInt 2) (Var "bar"))
          @?= (["foo: 2"],Left "Unknown variable: bar"),
      --
      testCase "evalKvGet" $
        runEval
          (eval $ Let "x" (KvPut (CstInt 0) (CstBool True))(KvGet (CstInt 0)))
          @?= ([],Right (ValBool True))



      
    ]

tests :: TestTree
tests = testGroup "APL" [evalMonad]
