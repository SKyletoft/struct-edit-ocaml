module Main where

import           Brick

import           Ast
import           Display
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

main :: IO ()
main = do
  let f0 = Func Nothing [] Nothing []
      f1 = editInner' f0 [0, 0] (EditText "fib")
      f2 = editInner' f1 [1, 0] InsertArgument
      f3 = editInner' f2 [1, 0, 0, 0] (EditText "n")
      f4 = editInner' f3 [1, 0, 1, 0] (EditText "number")
      f5 = editInner' f4 [2, 0] (EditText "number")
      f6 = editInner' f5 [3, 0] InsertIf
      f7 = editInner' f6 [3, 0, 0, 0] InsertTrue
      f8 = editInner' f6 [3, 0, 0, 0] (InsertIdent "n")
      f9 = editInner' f8 [3, 0, 0, 0, 0] MakeBinOp
      f10 = editInner' f9 [3, 0, 0, 0, 1, 0] (InsertBinOp LTE)
      f11 = editInner' f10 [3, 0, 0, 0, 2, 0] (InsertNumber "2")
   in putStrLn . unlines . concatMap ((++ [""]) . displays) $
      [fib, f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11]
