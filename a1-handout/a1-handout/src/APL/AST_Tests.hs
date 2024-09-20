module APL.AST_Tests (tests) where

import APL.AST (Exp (..), printExp)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Prettyprinting"
    [testCase "Basic" $
        printExp (CstInt 1)
          @?= "1",
      --
      testCase "Bool" $
        printExp (CstBool True)
          @?= "True",
      --
      testCase "Add" $
        printExp (Add (CstInt 2) (CstInt 5))
          @?= "(2+5)",
      --
      testCase "Sub" $
        printExp (Sub (CstInt 2) (CstInt 5))
          @?= "(2-5)",
      --
      testCase "Mul" $
        printExp (Mul (CstInt 2) (CstInt 5))
          @?= "(2*5)",
      --
      testCase "Div" $
        printExp (Div (CstInt 7) (CstInt 3))
          @?= "(7/3)",
      --
      testCase "Pow" $
        printExp (Pow (CstInt 2) (CstInt 3))
          @?= "(2**3)",
      --
      testCase "Pow negative" $
        printExp (Pow (CstInt 2) (CstInt (-1)))
          @?= "(2**(-1))",
      --
      testCase "If" $
        printExp (If (CstBool True) (CstInt 2) (Div (CstInt 7) (CstInt 0)))
          @?= "(if True then 2 else (7/0))",
      --
      testCase "If Eql" $
        printExp (If (Eql (CstInt 2) (CstInt 2)) (CstInt 2) (Div (CstInt 7) (CstInt 7)))
          @?= "(if (2==2) then 2 else (7/7))",
      --
      testCase "Eql (true)" $
        printExp (Eql (CstInt 2) (CstInt 3))
          @?= "(2==3)",
      --
      testCase "TryCatch" $
        printExp (TryCatch (CstInt 2) (CstInt 3))
          @?= "(try 2 catch 3)",
      --
      testCase "TryCatch and Apply" $
        printExp (TryCatch (Apply (CstInt 2) (CstInt 3)) (CstInt 3))
          @?= "(try 2 3 catch 3)",
      --
      testCase "Apply on int" $
        printExp (Apply (CstInt 1) (CstInt 2))
          @?= "1 2",
      --
      testCase "Apply on var" $
        printExp (Apply (Var "fort") (Var "nite"))
          @?= "fort nite",
      --
      testCase "Apply stacked 1" $
        printExp (Apply (Apply (CstInt 2) (CstInt 3)) (CstInt 3))
          @?= "2 3 3",
      --
      testCase "Apply stacked 2" $
        printExp (Apply (CstInt 2) (Apply (CstInt 3) (CstInt 3)))
          @?= "2 3 3",
      --
      testCase "Let" $
        printExp (Let "x" (Add (CstInt 2) (CstInt 3)) (Var "x"))
          @?= "(Let x=(2+3) in x)",
      --
      testCase "Let and Lambda" $
        printExp (Let "x" (CstInt 2) (Lambda "y" (Add (Var "x") (Var "y"))))
          @?= "(Let x=2 in (\\y->(x+y)))",
      --
      testCase "Apply" $
        printExp (Apply (Let "x" (CstInt 2) (Lambda "y" (Add (Var "x") (Var "y")))) (CstInt 3))
        @?= "(Let x=2 in (\\y->(x+y))) 3",
      --
      testCase "Negatives" $
        printExp (Add (CstInt 3) (CstInt (-2)))
        @?= "(3+(-2))",
      --
      testCase "Apply like in feedback" $
        printExp (Apply (Let "x" (CstInt 4) (Var "f")) (Var "x"))
        @?= "(Let x=4 in f) x"
          ]

