-- Exercises from chapter 9-10 of The Craft of Functional Programming

-- 1. (*) Define the length function using map and sum.
-- getLength :: (Num b) => [a] -> b 
getLength2 [] = 0 
getLength2 xs = sum [1 | _ <- xs]

-- 2. map (+1) (map (+1) xs) result in array of which the elements is
--    incremented twice (by +1 on the inner map, and the +1 in the outer map)
--
--    map f (map g xs) in general execute g for each element in xs, result in
--    a new array, and iterate the new array and execute f for each of its elements,
--    resulting in the final array


-- 3. iter n f x = f (f (... (f x)))
--    

-- 4. How would you define the sum of the squares of the natural numbers 1 to n using map and foldr?
sumOfSquares n = foldr (+) 0 (map (\x -> x*x) [1..n])

-- • How does this function below behaves?

mystery xs = foldr (++) [] (map sing xs)
  where
    sing x = [x]

-- the mystery function above takes an array, map it to the sing function,
-- resulted in an array of array, in which the inner arrays only consists of 1 element
-- [1, 2, 3] => [[1], [2], [3]]
-- and the result array is used on the foldr function, with the accumulator value is []
-- so its basically returning the array of arrays into a single array (flat map)



-- (*) Define the function flip :: (a -> b -> c) -> (b -> a -> c)

flipArgs :: (a -> b -> c) -> (b -> a -> c)
flipArgs f a b = f b a


f :: Int -> Bool
f n = n < 10