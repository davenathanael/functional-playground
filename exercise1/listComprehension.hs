-- Rewrite list comprehensions with map and filter

-- 1. [ x+1 | x <- xs ]
--    map (+1) xs

-- 2. [ x+y | x <- xs, y <-ys ]
--    concat (map (\x -> map (\y -> x+y) [10,20,30]) [1,2,3])

-- 3. [ x+2 | x <- xs, x > 3 ]
--    map (2+) (filter (>3) xs)

-- 4. [ x+3 | (x,_) <- xys ]
--    map (\(x,_) -> x+3) xys

-- 5. [ x+4 | (x,y) <- xys, x+y < 5 ]
--    map (\(x,_) -> x+4) (filter (\(x,y) -> x+y < 5) xys)

-- 6. [ x+5 | Just x <- mxs ]
--    map (+5) mxs

-----------------------------------------------------------------

-- 1. map (+3) xs
--    [ x+3 | x <- xs ]

-- 2. filter (>7) xs
--    [ x | x <- xs, x > 7 ] 

-- 3. concat (map (\x -> map (\y -> (x,y)) ys) xs)
--    [ (x,y) | x <- xs, y <- ys ]
 
-- 4. filter (>3) (map (\(x,y) -> x+y) xys)
--    [ x+y | (x,y) <- xys, x+y > 3 ]

