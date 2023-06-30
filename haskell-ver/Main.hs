module Main where

import           Debug.Trace
import           GHC.Stack

import           Ast
import qualified Display
import qualified Edit
import           Edit (Action (..))
import qualified Highlight
import qualified FFI.Termios as Termios
import qualified Ui
import qualified PrettyDisplay

{-
  function fib(n: number): number {
    if (n <= 1) {
      return 1;
    } else {
      const prev = fib(n - 1);
      const before_that = fib(n - 2);
      return prev + before_that;
    }
  }
-}
fib =
  Func
    { dName = Just "fib"
    , dArgs =
        [ Argument {aName = Just "n", aExpr = Just "number"}
        , Argument {aName = Just "nn", aExpr = Just "number"}
        ]
    , dType = Just "number"
    , dBody =
        [ If
            { iCond =
                Just
                  (BinOps
                     (Just (Ident {cIdent = "nn"}))
                     [(Just LTE, Just (Number {cNumber = "1"}))])
            , iThen =
                [ Return
                    (Just
                       (Call
                          { cName = Just (Ident {cIdent = "fib"})
                          , cParams =
                              [ Just
                                  (BinOps
                                     (Just (Ident {cIdent = "n"}))
                                     [(Just Sub, Just (Number {cNumber = "1"}))])
                              ]
                          }))
                ]
            , iElse =
                Just
                  [ SDecl
                      (Const
                         { dName = Just "prev"
                         , dType = Just "number"
                         , dExpr =
                             Just
                               (Call
                                  { cName = Just (Ident {cIdent = "fib"})
                                  , cParams =
                                      [ Just
                                          (BinOps
                                             (Just (Ident {cIdent = "n"}))
                                             [ ( Just Sub
                                               , Just (Number {cNumber = "1"}))
                                             ])
                                      ]
                                  })
                         })
                  , SDecl
                      (Const
                         { dName = Just "before_that"
                         , dType = Just "number"
                         , dExpr =
                             Just
                               (Call
                                  { cName = Just (Ident {cIdent = "fib"})
                                  , cParams =
                                      [ Just
                                          (BinOps
                                             (Just (Ident {cIdent = "n"}))
                                             [ ( Just Sub
                                               , Just (Number {cNumber = "2"}))
                                             ])
                                      ]
                                  })
                         })
                  , Return
                      (Just
                         (BinOps
                            (Just (Ident {cIdent = "prev"}))
                            [(Just Add, Just (Ident {cIdent = "before_that"}))]))
                  ]
            }
        , Return Nothing
        ]
    }

f0 = [Func Nothing [] Nothing []]
f1 = Edit.editInner' f0 [0, 0, 0] (EditText "fib")
f2 = Edit.editInner' f1 [0, 1, 0] InsertArgument
f3 = Edit.editInner' f2 [0, 1, 0, 0, 0] (EditText "n")
f4 = Edit.editInner' f3 [0, 1, 0, 1, 0] (EditText "number")
f5 = Edit.editInner' f4 [0, 2, 0] (EditText "number")
f6 = Edit.editInner' f5 [0, 3, 0] InsertIf
f7 = Edit.editInner' f6 [0, 3, 0, 0, 0] InsertTrue
f8 = Edit.editInner' f6 [0, 3, 0, 0, 0] (InsertIdent "n")
f9 = Edit.editInner' f8 [0, 3, 0, 0, 0, 0] MakeBinOp
f10 = Edit.editInner' f9 [0, 3, 0, 0, 0, 1, 0] (InsertBinOp LTE)
f11 = Edit.editInner' f10 [0, 3, 0, 0, 0, 2, 0] (InsertNumber "1")
f12 = Edit.editInner' f11 [0, 3, 0, 1, 1] InsertReturn
f13 = Edit.editInner' f12 [0, 3, 0, 1, 0, 0, 0] (InsertIdent "fib")
f14 = Edit.editInner' f13 [0, 3, 0, 1, 0, 0, 0, 0] MakeCall
f15 = Edit.editInner' f14 [0, 3, 0, 1, 0, 0, 0, 1, 0, 0] (InsertIdent "n")
f16 = Edit.editInner' f15 [0, 3, 0, 1, 0, 0, 0, 1, 0, 0, 0] MakeBinOp
f17 = Edit.editInner' f16 [0, 3, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0] (InsertBinOp Sub)
f18 = Edit.editInner' f17 [0, 3, 0, 1, 0, 0, 0, 1, 0, 0, 2, 0] (InsertNumber "1")
f19 = Edit.editInner' f18 [0, 3, 0, 0] InsertElse
f20 = Edit.editInner' f19 [0, 3, 0, 2, 0, 0] InsertConst
f21 = Edit.editInner' f20 [0, 3, 0, 2, 0, 1] InsertConst
f22 = Edit.editInner' f21 [0, 3, 0, 2, 0, 2] InsertReturn
f23 = Edit.editInner' f22 [0, 3, 0, 2, 0, 0, 0, 0, 0] (EditText "prev")
f24 = Edit.editInner' f23 [0, 3, 0, 2, 0, 1, 0, 0, 0] (EditText "before_that")
f25 = Edit.editInner' f24 [0, 3, 0, 2, 0, 0, 0, 2, 0] (InsertIdent "fib")
f26 = Edit.editInner' f25 [0, 3, 0, 2, 0, 0, 0, 2, 0, 0] MakeCall
f27 = Edit.editInner' f26 [0, 3, 0, 2, 0, 0, 0, 2, 0, 1, 0, 0] (InsertIdent "n")
f28 = Edit.editInner' f27 [0, 3, 0, 2, 0, 1, 0, 2, 0] (InsertIdent "fib")
f29 = Edit.editInner' f28 [0, 3, 0, 2, 0, 1, 0, 2, 0, 0] MakeCall
f30 = Edit.editInner' f29 [0, 3, 0, 2, 0, 1, 0, 2, 0, 1, 0, 0] (InsertIdent "n")
f31 = Edit.editInner' f30 [0, 3, 0, 2, 0, 1, 0, 2, 0, 1, 0, 0, 0] MakeBinOp
f32 = Edit.editInner' f31 [0, 3, 0, 2, 0, 1, 0, 2, 0, 1, 0, 0, 1, 0] (InsertBinOp Sub)
f33 = Edit.editInner' f32 [0, 3, 0, 2, 0, 1, 0, 2, 0, 1, 0, 0, 2, 0] (InsertNumber "2")
f34 = Edit.editInner' f33 [0, 3, 0, 2, 0, 0, 0, 2, 0, 1, 0, 0, 0] MakeBinOp
f35 = Edit.editInner' f34 [0, 3, 0, 2, 0, 0, 0, 2, 0, 1, 0, 0, 1, 0] (InsertBinOp Sub)
f36 = Edit.editInner' f35 [0, 3, 0, 2, 0, 0, 0, 2, 0, 1, 0, 0, 2, 0] (InsertNumber "1")
f37 = Edit.editInner' f36 [0, 3, 0, 2, 0, 2, 0, 0] (InsertIdent "prev")
f38 = Edit.editInner' f37 [0, 3, 0, 2, 0, 2, 0, 0, 0] MakeBinOp
f39 = Edit.editInner' f38 [0, 3, 0, 2, 0, 2, 0, 0, 2, 0] (InsertIdent "before_that")
f40 = Edit.editInner' f39 [0, 3, 0, 2, 0, 2, 0, 0, 1, 0] (InsertBinOp Add)
f41 = Edit.editInner' f40 [0, 3, 0, 1, 0, 0, 0] Remove
f42 = Edit.editInner' f41 [0, 3, 0, 1, 0, 0, 0] (InsertNumber "1")

alignNumber :: Show a => a -> String
alignNumber n =
  let n' = show n
      len = length n'
   in replicate (3 - len) ' ' ++ n'

buildFib :: IO ()
buildFib
  = putStrLn
  . unlines
  . zipWith (\ n s -> show n ++ ": " ++ s) [0..]
  . concatMap ((++ [""]) . Display.displays)
  $ [ f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14
    , f15,f16, f17, f18, f19, f20, f21, f22, f23, f24, f25 , f26, f27
    , f28, f29, f30, f31, f32, f33, f34, f35, f36 , f37, f38, f39, f40
    , f41, f42]

testHighlight :: IO ()
testHighlight
  = putStrLn
  . unlines
  . zipWith (\n s -> alignNumber n ++ ":  " ++ s) [0 ..]
  . concatMap (++ [""])
  $ [ Highlight.highlights' [] fib
    , Highlight.highlights' [0] fib
    , Highlight.highlights' [0, 0] fib
    , Highlight.highlights' [2, 0] fib
    , Highlight.highlights' [3, 0] fib
    , Highlight.highlights' [3, 0, 0] fib
    , Highlight.highlights' [3, 1, 0] fib
    , Highlight.highlights' [3, 0, 0, 0] fib
    , Highlight.highlights' [3, 0, 0, 0, 0, 0] fib
    , Highlight.highlights' [3, 0, 0, 0, 1, 0] fib
    , Highlight.highlights' [3, 0, 0, 0, 2, 0] fib
    ]

testHighlight2 :: IO ()
testHighlight2
  = putStrLn
  . unlines
  . zipWith (\n s -> alignNumber n ++ ":  " ++ s) [0 ..]
  . concatMap (++ [""])
  $ [ Highlight.highlights' [0, 0] fib
    , Highlight.highlights' [1, 0] fib
    , Highlight.highlights' [1, 0, 0, 0] fib
    , Highlight.highlights' [1, 0, 1, 0] fib
    , Highlight.highlights' [2, 0] fib
    , Highlight.highlights' [3, 0] fib
    , Highlight.highlights' [3, 0, 0, 0] fib
    , Highlight.highlights' [3, 0, 0, 0] fib
    , Highlight.highlights' [3, 0, 0, 0, 0] fib
    , Highlight.highlights' [3, 0, 0, 0, 1, 0] fib
    , Highlight.highlights' [3, 0, 0, 0, 2, 0] fib
    , Highlight.highlights' [3, 0, 1, 0] fib
    , Highlight.highlights' [3, 0, 1, 0, 0, 0] fib
    , Highlight.highlights' [3, 0, 1, 0, 0, 0, 0] fib
    , Highlight.highlights' [3, 0, 1, 0, 0, 0, 1, 0, 0] fib
    , Highlight.highlights' [3, 0, 1, 0, 0, 0, 1, 0, 0, 0] fib
    , Highlight.highlights' [3, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0] fib
    , Highlight.highlights' [3, 0, 1, 0, 0, 0, 1, 0, 0, 2, 0] fib
    , Highlight.highlights' [3, 0, 0] fib
    , Highlight.highlights' [3, 0, 2, 0, 0, 0] fib
    , Highlight.highlights' [3, 0, 2, 0, 0, 1, 0] fib
    , Highlight.highlights' [3, 0, 2, 0, 0, 2, 0] fib
    , Highlight.highlights' [3, 0, 2, 0, 0, 0, 0, 0] fib
    , Highlight.highlights' [3, 0, 2, 0, 0, 1, 0, 0, 0] fib
    , Highlight.highlights' [3, 0, 2, 0, 0, 0, 0, 2, 0] fib
    , Highlight.highlights' [3, 0, 2, 0, 0, 2, 0] fib
    , Highlight.highlights' [3, 0, 2, 0, 0, 2, 0, 1, 0, 0] fib
    , Highlight.highlights' [3, 0, 2, 0, 0, 1, 0, 2, 0] fib
    ]

main :: IO ()
main = do
  Termios.setupTerminal
  Ui.navigate' [Ui.Ast fib] [0]
