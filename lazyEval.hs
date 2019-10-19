import Data.List

-- 1. Uraikan langkah evaluasi dari ekspresi berikut: [ x+y | x <- [1 .. 4], y <- [2 .. 4], x > y ]
--    x = 1, 2, 3, 4
--    y = 2, 3, 4
--    untuk semua kemungkinan pasangan nilai x dan y,
--    pasangan yang x > y akan dipilih
--    dihasilkan list baru dengan nilai x+y dari x dan y yang dipilih

-- 2. Buatlah fungsi divisor yang menerima sebuah bilangan bulat n dan mengembalikan
--    list bilangan bulat positif yang membagi habis n
divisor :: Integral a => a -> [a]
divisor n = [x | x <- [1..n], n `mod` x == 0]

-- 3. Buatlah definisi quick sort menggunakan list comprehension.
-- quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = (quickSort [y | y <- xs, y <= x]) ++ [x] ++ (quickSort [y | y <- xs, y > x])

-- 4. Buatlah definisi infinite list untuk permutation.
permutate :: [a] -> [[a]]
permutate [] = [[]]
permutate list = [x:other | x <- ls, other <- perms (list\\[x])]

-- 5. Buatlah definisi untuk memberikan infinite list dari bilangan prima menerapkan algoritma Sieve of Erastothenes.
primeNums = sieve [2..]
  where sieve (x:xs) = x : sieve [y | y<-xs, y `mod` x /=0]

-- 6. Buatlah definisi infinite list dari triple pythagoras. List tersebut terdiri dari element
--    triple bilangan bulat positif yang mengikut persamaan pythagoras x^2 + y^2 = z^2
triPyth = [(x,y,z) | z <- [5..], y <- [4..z], x <- [3..y], x^2+y^2==z^2]