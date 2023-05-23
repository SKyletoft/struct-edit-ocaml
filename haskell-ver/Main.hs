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
    [Just (Argument (Just "n") (Just "number"))]
    (Just "number")
    [ If
        (Just (BinOps (Just (Ident "n")) [(Just LTE, Just (Number "1"))]))
        [Return (Just (Number "1"))]
        (Just
           [ SDecl (Const (Just "prev") Nothing Nothing)
           , SDecl (Const (Just "before_that") Nothing Nothing)
           , Return . Just . BinOps (Just (Ident "prev")) $
             [(Just Add, Just (Ident "before_that"))]
           ])
    ]

main = putStrLn . unlines . displays $ fib
