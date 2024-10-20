module APL.Eval_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (Error, Val (..), eval, runEval)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

eval' :: Exp -> Either Error Val
eval' = snd . runEval . eval



oldEvalTests :: TestTree
oldEvalTests =
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
          @?= Right (ValBool True),
      --
      testCase "Eql bool int" $
        eval'
          (Eql (CstBool True) (CstInt 3))
          @?= Left "Invalid operands to equality",
      --
      testCase "Apply non function" $
        eval'
          (Apply (CstBool True) (CstInt 3))
          @?= Left "Cannot apply non-function",
      --
      testCase "If without Bool" $
        eval'
          (If (CstInt 0) (CstInt 0) (CstInt 0))
          @?= Left "Non-boolean conditional.",
      --
      testCase "false" $
        eval'
          (If (CstBool False) (CstInt 1) (CstInt 0))
          @?= Right (ValInt 0),
      --
      testCase "kvPut another test" $
        eval'
          (Let "x" (KvPut (CstInt 10) (CstBool True))(KvGet (CstInt 1)))
          @?= Left "Key invalid: ValInt 1",
      --
      testCase "kvPut override else part" $
        eval'
          (Let "x" (Let "y" (KvPut (CstInt 10) (CstBool True))(KvPut (CstInt 11) (CstBool True)))(KvPut (CstInt 11) (CstBool False)))
          @?= Right (ValBool False)

    ]
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
          @?= ([],Right (ValBool True)),
      --
      testCase "KvPutOverride" $
        runEval
          (eval $ Let "X" (Let "y" (KvPut (CstInt 0) (CstInt 1)) (KvPut (CstInt 0) (CstInt 2))) (KvGet (CstInt 0)))

          @?= ([],Right (ValInt 2)),
      --
      testCase "evalKvGet invalid key" $
        runEval
          (eval $ KvGet (CstInt 0))
          @?= ([],Left "Key invalid: ValInt 0"),
      --
      testCase "Eql (bool)" $
        runEval
          (eval $ Eql (CstBool True) (CstBool True))
          @?= ([],Right (ValBool True)),
      --
      testCase "TryCatch e1 saves var and fails" $
        runEval
          (eval $ TryCatch (Apply (Let "x" (CstInt 10) (CstInt 10)) (Div (CstInt 3) (CstInt 0))) (Add (Var "x") (CstInt 1)))
          @?= ([],Left "Unknown variable: x"),
      --
      testCase "TryCatch e1 has kvput and fails" $
        runEval
          (eval $ TryCatch (Apply (KvPut (CstInt 0) (CstInt 10)) (Div (CstInt 3) (CstInt 0))) (Add (KvGet (CstInt 0)) (CstInt 1)))
          @?= ([],Left "Key invalid: ValInt 0"),
      --
      testCase "TryCatch e1 lambda fail" $
        runEval
          (eval $ TryCatch (Apply (Lambda "x" (CstInt 10))(Div (CstInt 3) (CstInt 0))) (Add (Var "x") (CstInt 1)))
          @?= ([],Left "Unknown variable: x"),
      --
      testCase "Trycatch print in e1" $
        runEval
          (eval $ TryCatch (Apply (Print "Var in e1" (CstInt 2))(Div (CstInt 3) (CstInt 0))) (Add (CstInt 1) (CstInt 1)))
          @?= ([],Right (ValInt 2))







    ]

tests :: TestTree
tests = testGroup "APL" [evalMonad, oldEvalTests]
