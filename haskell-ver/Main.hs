module Main where

import           Debug.Trace

import           Brick

import           Ast
import           Display
import           Highlight
import           Edit

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
fib :: Decl
fib =
  Func
    (Just "fib")
    [Argument (Just "n") (Just "number")]
    (Just "number")
    [ If
        (Just (BinOps (Just (Ident "n")) [(Just LTE, Just (Number "1"))]))
        [Return (Just (Number "1"))]
        (Just
           [ SDecl (Const (Just "prev") Nothing Nothing)
          , SDecl (Const (Just "before_that") Nothing Nothing)
          , Return
               (Just
                  (BinOps
                     (Just (Ident "prev"))
                     [(Just Add, Just (Ident "before_that"))]))
           ])
    ]

f36' =
  [ Func
     { dName = Just "fib"
     , dArgs = [Argument {aName = Just "n", aExpr = Just "number"}]
     , dType = Just "number"
     , dBody =
          [ If
              { iCond =
                  Just
                    (BinOps
                       (Just (Ident {cIdent = "n"}))
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
                                       [ ( Just Sub
                                        , Just (Number {cNumber = "1"}))
                                       ])
                                ]
                            }))
                  ]
             , iElse =
                  Just
                    [ SDecl
                        (Const
                           { dName = Just "prev"
                          , dType = Nothing
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
                          , dType = Nothing
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
                   , Return Nothing
                    ]
              }
          ]
      }
  ]

f40' =
  Func
    { dName = Just "fib"
    , dArgs = [Argument {aName = Just "n", aExpr = Just "number"}]
    , dType = Just "number"
    , dBody =
        [ If
            { iCond =
                Just
                  (BinOps
                     (Just (Ident {cIdent = "n"}))
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
                        , dType = Nothing
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
                        , dType = Nothing
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
        ]
    }

f0 = [Func Nothing [] Nothing []]
f1 = editInner' f0 [0, 0, 0] (EditText "fib")
f2 = editInner' f1 [0, 1, 0] InsertArgument
f3 = editInner' f2 [0, 1, 0, 0, 0] (EditText "n")
f4 = editInner' f3 [0, 1, 0, 1, 0] (EditText "number")
f5 = editInner' f4 [0, 2, 0] (EditText "number")
f6 = editInner' f5 [0, 3, 0] InsertIf
f7 = editInner' f6 [0, 3, 0, 0, 0] InsertTrue
f8 = editInner' f6 [0, 3, 0, 0, 0] (InsertIdent "n")
f9 = editInner' f8 [0, 3, 0, 0, 0, 0] MakeBinOp
f10 = editInner' f9 [0, 3, 0, 0, 0, 1, 0] (InsertBinOp LTE)
f11 = editInner' f10 [0, 3, 0, 0, 0, 2, 0] (InsertNumber "1")
f12 = editInner' f11 [0, 3, 0, 1, 1] InsertReturn
f13 = editInner' f12 [0, 3, 0, 1, 0, 0, 0] (InsertIdent "fib")
f14 = editInner' f13 [0, 3, 0, 1, 0, 0, 0, 0] MakeCall
f15 = editInner' f14 [0, 3, 0, 1, 0, 0, 0, 1, 0, 0] (InsertIdent "n")
f16 = editInner' f15 [0, 3, 0, 1, 0, 0, 0, 1, 0, 0, 0] MakeBinOp
f17 = editInner' f16 [0, 3, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0] (InsertBinOp Sub)
f18 = editInner' f17 [0, 3, 0, 1, 0, 0, 0, 1, 0, 0, 2, 0] (InsertNumber "1")
f19 = editInner' f18 [0, 3, 0, 0] InsertElse
f20 = editInner' f19 [0, 3, 0, 2, 0, 0] InsertConst
f21 = editInner' f20 [0, 3, 0, 2, 0, 1] InsertConst
f22 = editInner' f21 [0, 3, 0, 2, 0, 2] InsertReturn
f23 = editInner' f22 [0, 3, 0, 2, 0, 0, 0, 0, 0] (EditText "prev")
f24 = editInner' f23 [0, 3, 0, 2, 0, 1, 0, 0, 0] (EditText "before_that")
f25 = editInner' f24 [0, 3, 0, 2, 0, 0, 0, 2, 0] (InsertIdent "fib")
f26 = editInner' f25 [0, 3, 0, 2, 0, 0, 0, 2, 0, 0] MakeCall
f27 = editInner' f26 [0, 3, 0, 2, 0, 0, 0, 2, 0, 1, 0, 0] (InsertIdent "n")
f28 = editInner' f27 [0, 3, 0, 2, 0, 1, 0, 2, 0] (InsertIdent "fib")
f29 = editInner' f28 [0, 3, 0, 2, 0, 1, 0, 2, 0, 0] MakeCall
f30 = editInner' f29 [0, 3, 0, 2, 0, 1, 0, 2, 0, 1, 0, 0] (InsertIdent "n")
f31 = editInner' f30 [0, 3, 0, 2, 0, 1, 0, 2, 0, 1, 0, 0, 0] MakeBinOp
f32 = editInner' f31 [0, 3, 0, 2, 0, 1, 0, 2, 0, 1, 0, 0, 1, 0] (InsertBinOp Sub)
f33 = editInner' f32 [0, 3, 0, 2, 0, 1, 0, 2, 0, 1, 0, 0, 2, 0] (InsertNumber "2")
f34 = editInner' f33 [0, 3, 0, 2, 0, 0, 0, 2, 0, 1, 0, 0, 0] MakeBinOp
f35 = editInner' f34 [0, 3, 0, 2, 0, 0, 0, 2, 0, 1, 0, 0, 1, 0] (InsertBinOp Sub)
f36 = editInner' f35 [0, 3, 0, 2, 0, 0, 0, 2, 0, 1, 0, 0, 2, 0] (InsertNumber "1")
f37 = editInner' f36 [0, 3, 0, 2, 0, 2, 0, 0] (InsertIdent "prev")
f38 = editInner' f37 [0, 3, 0, 2, 0, 2, 0, 0, 0] MakeBinOp
f39 = editInner' f38 [0, 3, 0, 2, 0, 2, 0, 0, 2, 0] (InsertIdent "before_that")
f40 = editInner' f39 [0, 3, 0, 2, 0, 2, 0, 0, 1, 0] (InsertBinOp Add)
f41 = editInner' f40 [0, 3, 0, 1, 0, 0, 0] Remove
f42 = editInner' f41 [0, 3, 0, 1, 0, 0, 0] (InsertNumber "1")

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

main = putStrLn . unlines . Display.displays $ f42
