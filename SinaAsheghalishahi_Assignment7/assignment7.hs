{-
Name: Sina Asheghalishahi
Date Created: 04/21/2025
Program: EECS 468 Assignment 07
Description: Haskell program containing 2 sets of functions for list and numerical operations
Input: Varies by func. (see func. header comments)
Output: Varies by func. (see func. header comments)
Collaborators: NA
Other Sources: Permutations and Combinations Lecture (for formulas); chatGPT.com for Sterling NOSK formula assistance (*see below*)
-}


-- Part #1: Basic List Functions

-- replicate' func. def.
replicate' :: Integer -> a -> [a]
-- for each val from 1 to x, store y in list
replicate' x y = [y | _ <- [1..x]]


-- perfects func. def.
perfects :: Int -> [Int]
{- for all vals x from 2 to n (i.e. 1 is imperfect), if the sum of the list containing all vals y between 1 and x-1
   s.t. x is divisible by y (i.e. meaning y is a valid factor of x) is equal to x, then store x in the output list.
-}
perfects n = [x | x <- [2..n], sum [y | y <- [1..x-1], x `mod` y == 0] == x]


-- find func. def.
find :: Eq a => a -> [(a,b)] -> [b]
-- for each key-value pair in xs, if the key equals n, then store its corresponding value in the list
find n xs = [m | (n', m) <- xs, n == n']


-- positions func. def.
positions :: Eq a => a -> [a] -> [Int]
{- creates list containing contents of xs and their corresponding indices as key-value pairs,
   and then applies "find" to the list w/ given input value, which in turn outputs the list of
   all associated positions/indices of the given input value.
-}
positions x xs = find x (zip xs [0..])


-- scalarproduct func. def.
scalarproduct :: [Int] -> [Int] -> Int
{- creates list containing tuples of corresponding values of ns and ms, stores product of each tuple in the list in another list,
   and then outputs integral sum of product list -}
scalarproduct ns ms = sum [n*m | (n, m) <- (zip ns ms)]


-- Part #2: Numerical Distribution Functions

--Helper Function: Comb. and Fact. (NOTE: Used Perm. and Comb. lecture for formulas)

-- fact func. def.
fact :: Integer -> Integer
-- takes in an int and outputs the factorial
fact n = product [1..n]

-- comb func. def.
comb :: Integer -> Integer -> Integer
-- takes in n obj. and r chosen obj., and returns the # of combinations
comb n r = (fact n) `div` ((fact r)*(fact(n-r))) 


-- dodb func. def.
dodb :: Integer -> Integer -> Integer -> Integer
-- takes in n distinct objects, k distinct boxes, and m obj. per box, and outputs the # of diff. variations
-- Specifically, it multiplies factorial of m by itself k times, and then divides factorial of n by the result of such
dodb n k m = fact n `div` (product (replicate' k (fact m)))

-- Corrected Version:
dodb' :: Integer -> Integer -> Integer -> Integer
dodb' n k m
  | k * m > n  = error "Not enough objects to distribute"
  | otherwise  = product [comb (n - i * m) m | i <- [0 .. k - 1]]

--


-- iodb func. def. 
iodb :: Integer -> Integer -> Integer
-- takes in n indistinguishible objects and k distinct boxes, and returns # of diff. variations (using combinations func.)
iodb n k = comb (n + k - 1) n


-- doib func. def.
doib :: Int -> Int -> Int
-- takes in n distinct obj. and k indistinguishible boxes, and returns # of diff. variations
doib n k
    -- base cases listed out
    | n == 0 && k == 0 = 1
    | n == 0 || k == 0 = 0
    | k > n            = 0
    -- rec. case defined (i.e. call decrementing recursions)
    | otherwise        = k * doib (n - 1) k + doib (n - 1) (k - 1)

-- NOTE: chatGPT.com used to figure out Sterling Numbers of the Second Kind formula to assist in doib func. definition

-- Corrected Version:
doib' :: Integer -> Integer -> Integer
doib' n k = sum [stirling2 n i | i <- [1..k]]

-- Stirling numbers of the second kind
stirling2 :: Integer -> Integer -> Integer
stirling2 n k
  | n == k          = 1
  | k == 0 || k > n = 0
  | otherwise       = k * stirling2 (n - 1) k + stirling2 (n - 1) (k - 1)

--


-- Function to calculate the number of ways to distribute n indistinguishable
-- objects into k indistinguishable boxes (integer partitions)
ioib :: Int -> Int -> Int
ioib n 0 = if n == 0 then 1 else 0  -- If there are 0 boxes, only 1 way to partition 0 objects, otherwise no way
ioib 0 k = 1  -- Only 1 way to partition 0 objects into any number of boxes
ioib n k
  | n < k     = ioib n n  -- If we have more boxes than objects, reduce k to n
  | otherwise = ioib (n-1) k + ioib n (k-1)

-- Corrected Version:
ioib' :: Integer -> Integer -> Integer
ioib' n k
  | n == 0    = 1
  | k == 0    = 0
  | n < 0     = 0
  | otherwise = ioib' (n - k) k + ioib' n (k - 1)

--