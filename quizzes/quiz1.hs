-- 1. make a function to get the max value from 3 parameter
--    getMax 2 5 3 = 5

getMax :: (Ord a) => a -> a -> a -> a
getMax a b c
  | a > b && a > c = a
  | b > a && b > c = b
  | c > a && c > b = c

-- 2. evaluate and describe [(x, y) | x <- [1..4], y <- [2..6], x * 2 == y]

-- 3. implement quickSort

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = (quickSort (filter (<=x) xs)) : x : (quickSort (filter (>x) xs))

-- 4. use fold to calculate the sum of a list
getSum :: [a] -> a
getSum ls = foldr (+) 0 ls
-- another version:
-- getSum ls = foldr (\x a -> x+a) 0 ls

-- 5. given mystery xs ys = concat (map (\x -> map (\y -> return (x, y)) ys) xs)
--    evaluate mystery [1, 2, 3] [4, 5, 6]

-- 6. define infinite list "primes" using sieves of Erastothenes

-- 7. define function "flip" to flip parameter
