module CExpr where

data CExpr a = Val a | CExpr a :+ CExpr a | CExpr a :- CExpr a | CExpr a :* CExpr a | CExpr a :/ CExpr a

-- evaluate :: Num a => CExpr a -> a
evaluate (Val a) = a
evaluate (a :+ b) = evaluate a + evaluate b
evaluate (a :- b) = evaluate a - evaluate b
evaluate (a :* b) = evaluate a * evaluate b
evaluate (a :/ b) = evaluate a / evaluate b
