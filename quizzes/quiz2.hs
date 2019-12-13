rotabc :: String -> String
rotabc "" = ""
rotabc (x:str)
  | x == 'a' = 'b' : rotabc str
  | x == 'b' = 'c' : rotabc str
  | x == 'c' = 'a' : rotabc str
  | otherwise = x : rotabc str