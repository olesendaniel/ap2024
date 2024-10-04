module APL.Interp_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (eval)
import APL.InterpIO (runEvalIO)
import APL.InterpPure (runEval)
import APL.Monad
import APL.Util (captureIO)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

eval' :: Exp -> ([String], Either Error Val)
eval' = runEval . eval

evalIO' :: Exp -> IO (Either Error Val)
evalIO' = runEvalIO . eval

tests :: TestTree
tests = testGroup "Free monad interpreters" [pureTests, ioTests, tryCatchTests, putGetTests, dbTests, transactionTests, transactionTestsIO]

get0 :: Exp
get0 = KvGet (CstInt 0)

goodPut :: EvalM ()
goodPut = evalKvPut (ValInt 0) (ValInt 1)

badPut :: Free EvalOp b
badPut = evalKvPut (ValInt 0) (ValBool False) >> failure "die"


pureTests :: TestTree
pureTests =
  testGroup
    "Pure interpreter"
    [ testCase "localEnv" $
        runEval
          ( localEnv (const [("x", ValInt 1)]) $
              askEnv
          )
          @?= ([], Right [("x", ValInt 1)]),
      --
      testCase "Let" $
        eval' (Let "x" (Add (CstInt 2) (CstInt 3)) (Var "x"))
          @?= ([], Right (ValInt 5)),
      --
      testCase "Let (shadowing)" $
        eval'
          ( Let
              "x"
              (Add (CstInt 2) (CstInt 3))
              (Let "x" (CstBool True) (Var "x"))
          )
          @?= ([], Right (ValBool True)),
      --
      testCase "State" $
        runEval
          ( do
              putState [(ValInt 0, ValInt 1)]
              modifyState $ map (\(key, _) -> (key, ValInt 5))
              getState
          )
          @?= ([], Right [(ValInt 0, ValInt 5)]),
      --
      testCase "Print" $
        runEval (evalPrint "test")
          @?= (["test"], Right ()),
      --
      testCase "Error" $
        runEval
          ( do
              _ <- failure "Oh no!"
              evalPrint "test"
          )
          @?= ([], Left "Oh no!"),
      --
      testCase "Div0" $
        eval' (Div (CstInt 7) (CstInt 0))
          @?= ([], Left "Division by zero")
    ]

ioTests :: TestTree
ioTests =
  testGroup
    "IO interpreter"
    [ testCase "print" $ do
        let s1 = "Lalalalala"
            s2 = "Weeeeeeeee"
        (out, res) <-
          captureIO [] $
            runEvalIO $ do
              evalPrint s1
              evalPrint s2
        (out, res) @?= ([s1, s2], Right ())
        -- NOTE: This test will give a runtime error unless you replace the
        -- version of `eval` in `APL.Eval` with a complete version that supports
        -- `Print`-expressions. Uncomment at your own risk.
        -- testCase "print 2" $ do
        --    (out, res) <-
        --      captureIO [] $
        --        evalIO' $
        --          Print "This is also 1" $
        --            Print "This is 1" $
        --              CstInt 1
        --    (out, res) @?= (["This is 1: 1", "This is also 1: 1"], Right $ ValInt 1)
    ]

tryCatchTests :: TestTree
tryCatchTests =
  testGroup
    "try catch interpreter"
    [ testCase "test" $ do
        runEval $ Free $ TryCatchOp (failure "Oh no!") (pure "Success!")
        @?= ([], Right "Success!"),
    --
      testCase "test2" $ do
        runEval $ eval $ TryCatch (CstInt 5) (CstInt 1 `Div` CstInt 0)
        @?= ([], Right $ ValInt 5),
    --
      testCase "test3" $ do
        res <- runEvalIO $ eval $ TryCatch (CstInt 0 `Eql` CstBool True) (CstInt 1 `Div` CstInt 0)
        res @?= Left "Division by zero",
    --
    testCase "TryCatch with failed KvPutOp" $ do
    let m1 = Free $ KvPutOp (ValInt 0) (ValInt 1) (failure "Error in m1")
    let m2 = pure (ValInt 42)
    runEval (Free $ TryCatchOp m1 m2)
    @?= ([], Right (ValInt 42)),
    --
    testCase "TryCatch without failure" $ do
    let m1 = Free $ KvPutOp (ValInt 0) (ValInt 1) (evalKvGet (ValInt 0))
    let m2 = pure (ValInt 42)
    runEval (Free $ TryCatchOp m1 m2)
    @?= ([], Right (ValInt 1))

      ]
putGetTests :: TestTree
putGetTests =
  testGroup
    "Put get testing"
    [
      testCase "simple" $ do
        runEval $ Free $ KvPutOp (ValInt 0) (ValInt 1) (evalKvGet (ValInt 0))
        @?= ([],Right (ValInt 1)),
      --
      testCase "simple 2" $ do
        runEval $ Free $ KvPutOp (ValInt 0) (ValInt 1) (evalKvGet (ValInt 1))
        @?= ([],Left "Key not in state: ValInt 1"),
      --
      testCase "simple" $ do
        runEval $ Free $ KvPutOp (ValInt 0) (ValBool False) (evalKvGet (ValInt 0))
        @?= ([],Right (ValBool False)),
      --
      testCase "Missing key in KvGet" $ do
      (_, res) <- captureIO ["ValInt 5"] $
                  runEvalIO $ Free $ KvGetOp (ValInt 0) $ \val -> pure val
      res @?= Right (ValInt 5),
      --
      testCase "Invalid key-value input" $ do
      (_, res) <- captureIO ["joe nuts"] $
                  runEvalIO $ Free $ KvGetOp (ValInt 0) $ \val -> pure val
      res @?= Left "Key not valid"

    ]
dbTests :: TestTree
dbTests =
  testGroup
    "Tests with database"
    [
      testCase "simple" $ do
        res <- runEvalIO $ Free $ KvPutOp (ValInt 0) (ValInt 1) (evalKvGet (ValInt 0))
        res @?= Right (ValInt 1),
      --
      --testCase "simple 2" $ do
      --  res <- runEvalIO $ Free $ KvPutOp (ValInt 0) (ValInt 1) (evalKvGet (ValInt 1))
      --  res @?= Left "ValInt 1 not in DB",
      --
      testCase "Missing key test" $ do
        (_, res) <-
          captureIO ["ValInt 1"] $
            runEvalIO $
              Free $ KvGetOp (ValInt 0) $ \val -> pure val
        res @?= Right (ValInt 1)
    ]
transactionTests :: TestTree
transactionTests =
  testGroup
    "Tests with transaction"
    [
      testCase "invalid" $ do
        runEval $ transaction (evalKvPut (ValInt 0) (ValBool False) >> failure "die") >> eval (KvGet (CstInt 0))
        @?= ([],Left "Key not in state: ValInt 0"),
      --
      testCase "Valid" $ do
        runEval $ transaction (evalKvPut (ValInt 0) (ValInt 1)) >> eval (KvGet (CstInt 0))
        @?= ([],Right (ValInt 1)),
      --
      testCase "Print" $ do
        runEval $ transaction (evalPrint "weee" >> failure "oh shit")
        @?= (["weee"],Right ()),
      --
      testCase "Print" $ do
        runEval $ transaction (evalPrint "weee" >> failure "oh shit") >> evalPrint "weee2"
        @?= (["weee", "weee2"],Right ()),
      --
      testCase "Print" $ do
        runEval $ transaction (evalPrint "weee" >> evalPrint "weee2")
        @?= (["weee", "weee2"],Right ()),
      --
      testCase "Nested" $ do
        runEval $ transaction (goodPut >> transaction badPut) >> eval get0
        @?= ([],Right (ValInt 1)),
            --
      testCase "Nested 2" $ do
        runEval $ transaction (transaction badPut) >> eval get0
        @?= ([],Left "Key not in state: ValInt 0"),
      --
      testCase "Successful transaction" $ do
      runEval $ transaction (evalKvPut (ValInt 0) (ValInt 1)) >> eval (KvGet (CstInt 0))
      @?= ([], Right (ValInt 1)),
      --
      testCase "Transaction rollback on failure" $ do
      runEval $ transaction (evalKvPut (ValInt 0) (ValInt 1) >> failure "Fail") >> eval (KvGet (CstInt 0))
      @?= ([], Left "Key not in state: ValInt 0")
    ]
transactionTestsIO :: TestTree
transactionTestsIO =
  testGroup
    "Tests with transaction"
      [
      testCase "Valid" $ do
        res <- runEvalIO $ transaction (evalKvPut (ValInt 0) (ValInt 1)) >> eval (KvGet (CstInt 0))
        res @?= Right (ValInt 1),
      --
      testCase "Print" $ do
        res <- runEvalIO $ transaction (evalPrint "weee" >> failure "oh shit")
        res @?= Right (),
      --
      testCase "Print" $ do
        res <- runEvalIO $ transaction (evalPrint "weee" >> failure "oh shit") >> evalPrint "weee2"
        res @?= Right (),
      --
      testCase "Print" $ do
        res <- runEvalIO $ transaction (evalPrint "weee" >> evalPrint "weee2")
        res @?= Right (),
      --
      testCase "Nested" $ do
        res <- runEvalIO $ transaction (goodPut >> transaction badPut) >> eval get0
        res @?= Right (ValInt 1),
      --
      testCase "IO-based missing key recovery" $ do
      (_, res) <- captureIO ["ValInt 10"] $
                  runEvalIO $ evalKvGet (ValInt 0)
      res @?= Right (ValInt 10)

      ]
