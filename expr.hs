module CExpr where

data CExpr = Val Float | CExpr :+ CExpr | CExpr :- CExpr | CExpr :* CExpr | CExpr :/ CExpr
            | Var String | Let String CExpr CExpr
      deriving Show

evaluate :: CExpr -> Float
evaluate (Val a) = a
evaluate (a :+ b) = evaluate a + evaluate b
evaluate (a :- b) = evaluate a - evaluate b
evaluate (a :* b) = evaluate a * evaluate b
evaluate (a :/ b) = evaluate a / evaluate b
