rotabc :: String -> String
rotabc "" = ""
rotabc (x:str)
  | x == 'a' = 'b' : rotabc str
  | x == 'b' = 'c' : rotabc str
  | x == 'c' = 'a' : rotabc str
  | otherwise = x : rotabc str

-- last with pointfree style
last :: [a] -> a
last = head . reverse

-- last with foldl
lastWithFoldl :: [a] -> a
lastWithFoldl = foldl (\x a -> a) 0

-- last with foldr
lastWithFoldr :: [a] -> a
lastWithFoldr = foldr (\x a -> x) 0

-- compose
compose :: (t1 -> t2) -> (t -> t1) -> (t -> t2)
compose f g = \x -> f (g x)

