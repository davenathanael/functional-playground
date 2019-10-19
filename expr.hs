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
evaluate (Var v) = 0.0 -- zero value of float
evaluate (Let var exp1 exp2) = evaluate $ substitute var exp1 exp2

-- substitute any occurence of string V with the new expression 
substitute :: String -> CExpr -> CExpr -> CExpr
substitute newVar newExpr (Var var) = if newVar == var then newExpr else (Var var)
substitute _ _ (Val val) = (Val val)
substitute newVar newExpr (e1 :+ e2) = substitute newVar newExpr e1 :+ substitute newVar newExpr e2
substitute newVar newExpr (e1 :- e2) = substitute newVar newExpr e1 :- substitute newVar newExpr e2
substitute newVar newExpr (e1 :* e2) = substitute newVar newExpr e1 :* substitute newVar newExpr e2
substitute newVar newExpr (e1 :/ e2) = substitute newVar newExpr e1 :/ substitute newVar newExpr e2
substitute newVar newExpr (Let var e1 e2) = (Let var e1 (substitute newVar newExpr e2))

mapExpr :: (Float -> Float) -> CExpr -> CExpr
mapExpr f (Val a) = (Val (f a))
mapExpr _ (Var v) = (Var v)
mapExpr f (e1 :+ e2) = mapExpr f e1 :+ mapExpr f e2
mapExpr f (e1 :- e2) = mapExpr f e1 :- mapExpr f e2
mapExpr f (e1 :* e2) = mapExpr f e1 :* mapExpr f e2
mapExpr f (e1 :/ e2) = mapExpr f e1 :/ mapExpr f e2
mapExpr f (Let var e1 e2) = Let var e1 (mapExpr f e2)

foldExpr :: (Float -> CExpr -> Float) -> Float -> CExpr -> Float
foldExpr f acc (e1 :+ e2) = foldExpr f acc e1 + foldExpr f acc e2
foldExpr f acc (e1 :- e2) = foldExpr f acc e1 - foldExpr f acc e2
foldExpr f acc (e1 :* e2) = foldExpr f acc e1 * foldExpr f acc e2
foldExpr f acc (e1 :/ e2) = foldExpr f acc e1 / foldExpr f acc e2
foldExpr f acc expr = f acc expr

evalFold :: CExpr -> Float
evalFold expr = foldExpr evalHelper 0 expr

evalHelper :: Float -> CExpr -> Float
evalHelper acc (Val v) = acc+v
evalHelper acc (Var _) = acc
evalHelper acc expr@(Let _ _ _) = acc + evaluate expr

isConst :: CExpr -> Bool
isConst (Val _) = True
isConst expr = False

isVar :: CExpr -> Bool
isVar (Var _) = True
isVar expr = False

countConst :: CExpr -> Float
countConst expr = foldExpr (\acc exp -> if (isConst exp) then acc+1 else acc) 0 expr

countVar :: CExpr -> Float
countVar expr = foldExpr (\acc exp -> if (isVar exp) then acc+1 else acc) 0 expr
