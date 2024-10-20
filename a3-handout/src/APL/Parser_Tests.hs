module APL.Parser_Tests (tests) where

import APL.AST (Exp (..))
import APL.Parser (parseAPL)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

parserTest :: String -> Exp -> TestTree
parserTest s e =
  testCase s $
    case parseAPL "input" s of
      Left err -> assertFailure err
      Right e' -> e' @?= e

parserTestFail :: String -> TestTree
parserTestFail s =
  testCase s $
    case parseAPL "input" s of
      Left _ -> pure ()
      Right e ->
        assertFailure $
          "Expected parse error but received this AST:\n" ++ show e

tests :: TestTree
tests =
  testGroup
    "Parsing"
    [ testGroup
        "Constants"
        [ parserTest "123" $ CstInt 123,
          parserTest "(123)" $ CstInt 123,
          parserTest " 123" $ CstInt 123,
          parserTest "-123" $ CstInt (-123),
          parserTest "123 " $ CstInt 123,
          parserTestFail "123f",
          parserTest "true" $ CstBool True,
          parserTest "false" $ CstBool False
        ],
      testGroup
        "Basic operators"
        [ parserTest "x+y" $ Add (Var "x") (Var "y"),
          parserTest "x    +    y" $ Add (Var "x") (Var "y"),
          parserTest "x-y" $ Sub (Var "x") (Var "y"),
          parserTest "x*y" $ Mul (Var "x") (Var "y"),
          parserTest "x/y" $ Div (Var "x") (Var "y")
        ],
      testGroup
        "Operator priority (Add more tests here)"
        [ parserTest "x+y+z" $ Add (Add (Var "x") (Var "y")) (Var "z"),
          parserTest "x+y-z" $ Sub (Add (Var "x") (Var "y")) (Var "z"),
          parserTest "x+y*z" $ Add (Var "x") (Mul (Var "y") (Var "z")),
          parserTest "x*y*z" $ Mul (Mul (Var "x") (Var "y")) (Var "z"),
          parserTest "x/y/z" $ Div (Div (Var "x") (Var "y")) (Var "z")
        ],
      testGroup
        "Power operator (right-associative)"
        [ parserTest "x**y**z" $ Pow (Var "x") (Pow (Var "y") (Var "z")),
          parserTest "x*y**z" $ Mul (Var "x") (Pow (Var "y") (Var "z")),
          parserTest "x**y" $ Pow (Var "x") (Var "y")
        ],
      testGroup
        "Equality operator (left-associative)"
        [ parserTest "x==y==z" $ Eql (Eql (Var "x") (Var "y")) (Var "z"),
          parserTest "x==y" $ Eql (Var "x") (Var "y")
        ],
      testGroup
        "Print, Put, and Get commands"
        [ parserTest "print \"foo\" x" $ Print "foo" (Var "x"),
          parserTest "put x y" $ KvPut (Var "x") (Var "y"),
          parserTest "get x" $ KvGet (Var "x"),
          parserTest "get x + y" $ Add (KvGet (Var "x")) (Var "y"),
          parserTest "print \"Hello, World!\" x" $ Print "Hello, World!" (Var "x"),
          parserTest "put (x) y" $ KvPut (Var "x") (Var "y"),
          parserTest "print \"😊\" x" $ Print "😊" (Var "x"),
          parserTest "print \"let var in print\" x" $ Print "let var in print" (Var "x"),
          parserTest "print \"thingy\" (3+5)" $ Print "thingy" (Add (CstInt 3) (CstInt 5)),
          parserTest "print \"\n\" x" $ Print "\n" (Var "x")
        ],
      testGroup
        "Conditional expressions"
        [ parserTest "if x then y else z" $ If (Var "x") (Var "y") (Var "z"),
          parserTest "if x then y else if x then y else z" $
            If (Var "x") (Var "y") $
              If (Var "x") (Var "y") (Var "z"),
          parserTest "if x then (if x then y else z) else z" $
            If (Var "x") (If (Var "x") (Var "y") (Var "z")) (Var "z"),
          parserTest "1 + if x then y else z" $
            Add (CstInt 1) (If (Var "x") (Var "y") (Var "z"))
        ],
      testGroup
        "Lexing edge cases"
        [ parserTest "2 " $ CstInt 2,
          parserTest " 2" $ CstInt 2
        ],
      testGroup
        "Function Application"
        [ parserTest "x y" $ Apply (Var "x") (Var "y"),
          parserTest "x y z" $ Apply (Apply (Var "x") (Var "y")) (Var "z"),
          parserTest "x 1" $ Apply (Var "x") (CstInt 1),
          parserTest "x(y z)" $  Apply (Var "x") (Apply (Var "y") (Var "z")),
          parserTestFail  "x if x then y else z",
          parserTest  "x (if x then y else z)" $ Apply (Var "x") (If (Var "x") (Var "y") (Var "z")),
          parserTest "f (x + y)" $ Apply (Var "f") (Add (Var "x") (Var "y")),
          parserTest "f x + y" $ Add (Apply (Var "f") (Var "x")) (Var "y"),
          parserTest "f x - g y" $ Sub (Apply (Var "f") (Var "x")) (Apply (Var "g") (Var "y"))

        ],
      testGroup
        "TryCatch"
        [ parserTest "try x catch y" $ TryCatch (Var "x") (Var "y"),
          parserTest "try 7 + 10 catch y" $ TryCatch (Add (CstInt 7) (CstInt 10)) (Var "y"),
          parserTest "try 7 + false catch y" $ TryCatch (Add (CstInt 7) (CstBool False)) (Var "y")

        ],
      testGroup
        "Let binding"
        [ parserTest "let x = y in z" $  Let "x" (Var "y") (Var "z"),
          parserTest "(let x = y in z)" $  Let "x" (Var "y") (Var "z"),
          parserTestFail "let true = y in z",
          parserTestFail "x let v = 2 in v",
          parserTestFail "let 😊 = 2 in v",
          parserTestFail "let x = 😊 in v",
          parserTest "let x = x in v" $ Let "x" (Var "x") (Var "v"),
          parserTestFail "let if = x in v",
          parserTest "let x = 1 in let y = 2 in x + y" $ Let "x" (CstInt 1) (Let "y" (CstInt 2) (Add (Var "x") (Var "y")))

        ],
      testGroup
        "Lambda"
        [ parserTest "\\x -> y" $ Lambda "x" (Var "y"),
          parserTest "\\x -> x + x" $ Lambda "x" (Add (Var "x") (Var "x")),
          parserTest "\\x -> (x + x)" $ Lambda "x" (Add (Var "x") (Var "x")),
          parserTest "\\x -> (\\y -> z)" $ Lambda "x" (Lambda "y" (Var "z")),
          parserTest "(\\x -> (x ** 2)) 3" $ Apply (Lambda "x" (Pow (Var "x") (CstInt 2))) (CstInt 3)
        ],
      testGroup
        "Misc tests"
        [ parserTestFail "*",
          parserTestFail "/",
          parserTestFail "**",
          parserTestFail "+",
          parserTestFail "?",
          parserTestFail "😊",
          parserTestFail "",
          parserTestFail " ",
          parserTest "TRUE" $ Var "TRUE",
          parserTest "TR UE" $ Apply (Var "TR") (Var "UE"),
          parserTest "tr ue" $ Apply (Var "tr") (Var "ue"),
          parserTestFail "in",
          parserTestFail "let",
          parserTestFail "\\n"

        ]

    ]
