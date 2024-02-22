module Part4 (
    isPrime,
    gcd',
    coprime,
    totient,
    primeFactors,
    primeFactorsMult,
    totientImproved,
    primesR,
    goldbach,
    goldbachList,
    goldbachList'
) where

-- 31. Determine whether a given integer number is prime.
{-
    λ> isPrime 7
    True
-}

isPrime :: Int -> Bool
isPrime n = length (divisors n) == 2

divisors :: Int -> [Int]
divisors n = foldl (\acc x -> if n `mod` x == 0 then x:acc else acc) [] [1..n]

-- 32. Determine the greatest common divisor of two positive integer numbers.
{-
    λ> [gcd 36 63, gcd (-3) (-6), gcd (-3) 6]
    [9,3,3]
-}

gcd' :: Int -> Int -> Int
gcd' m n = maximum $ intersection (divisors m) (divisors n)
    where intersection xs = foldl (\acc y -> if y `elem` xs then y:acc else acc) []

-- 33. Determine whether two positive integer numbers are coprime.
{-
    λ> coprime 35 64
    True
-}

coprime :: Int -> Int -> Bool
coprime m n = gcd' m n == 1

-- 34. Calculate Euler's totient function phi(m).
{-
Euler's so-called totient function phi(m) is defined as the number of positive integers r (1 <= r < m) that are coprime to m.
Example: m = 10: r = 1,3,7,9; thus phi(m) = 4. Note the special case: phi(1) = 1.

    λ> totient 10
    4
-}

totient :: Int -> Int
totient m = length $ foldl (\acc x -> if coprime m x then x:acc else acc) [1] [2..m]

-- 35. Determine the prime factors of a given positive integer.
{-
Construct a flat list containing the prime factors in ascending order.

    λ> primeFactors 315
    [3, 3, 5, 7]
-}

primeFactors :: Int -> [Int]
primeFactors n = primeFactors' n (reverse $ divisors n)
    where 
        primeFactors' 0 _   = []
        primeFactors' _ []  = []
        primeFactors' n' (x:xs)
            | isPrime x && n' `mod` x == 0  = x : primeFactors' (n' `div` x) (x:xs)
            | otherwise                     = primeFactors' n' xs

-- 36. Determine the prime factors and their multiplicities of a given positive integer.
{-
Construct a list containing each prime factor and its multiplicity.

    λ> primeFactorsMult 315
    [(3,2),(5,1),(7,1)]
-}

primeFactorsMult :: Int -> [(Int, Int)]
primeFactorsMult n = primeFactorsMult' (primeFactors n) 1
    where
        primeFactorsMult' [] _ = []
        primeFactorsMult' [x] n' = [(x, n')]
        primeFactorsMult' (x:xs) n'
            | x == head xs  = primeFactorsMult' xs (n'+1)
            | otherwise     = (x, n') : primeFactorsMult' xs 1

-- 37. Calculate Euler's totient function phi(m) (improved).
{-
See Problem 34 for the definition of Euler's totient function. If the list of the prime factors of a number m is known in 
the form of problem 36 then the function phi(m) can be efficiently calculated as follows: Let ((p1 m1) (p2 m2) (p3 m3) ...) 
be the list of prime factors (and their multiplicities) of a given number m. Then phi(m) can be calculated with the 
following formula:

phi(m) = (p1 - 1) * p1^(m1 - 1) * 
         (p2 - 1) * p2^(m2 - 1) * 
         (p3 - 1) * p3^(m3 - 1) * ...

    λ> totientImproved 10
    4
-}

totientImproved :: Int -> Int
totientImproved n = foldl (\acc x -> acc * (fst x - 1) * fst x ^ (snd x - 1)) 1 (primeFactorsMult n)

-- 38. Compare the two methods of calculating Euler's totient function.
{-
totient         -> O(n^2)
totientImproved -> O(n^2) -- if primeFactors function is improved then we get O(nlogn)
-}

-- 39. A list of prime numbers in a given range.
{-
    λ> primesR 10 20
    [11,13,17,19]
-}

primesR :: Int -> Int -> [Int]
primesR m n
    | m < 0 = error "Invalid lower bound."
    | n < 0 = error "Invalid upper bound."
    | m > n = error "Invalid bounds."
primesR m n = reverse $ foldl (\acc x -> if isPrime x then x:acc else acc) [] [m..n]

-- 40. Goldbach's conjecture.
{-
Goldbach's conjecture says that every positive even number greater than 2 is the sum of two prime numbers. Example: 28 = 5 + 23. 
It is one of the most famous facts in number theory that has not been proved to be correct in the general case. It has been 
numerically confirmed up to very large numbers (much larger than we can go with our Prolog system). Write a predicate to find 
the two prime numbers that sum up to a given even integer.

    λ> goldbach 28
    (5, 23)
-}

goldbach :: Int -> (Int, Int)
goldbach n
    | n <= 2 = error "Not valid number."
    | odd n  = error "Not even number."
goldbach n = goldbach' n (primesR 2 n)
    where
        goldbach' _ [] = error "Error."
        goldbach' n' (x:xs)
            | isPrime (n' - x)  = (x, n'-x)
            | otherwise         = goldbach' n' xs

-- 41. A list of even numbers and their Goldbach compositions in a given range.
{-
Given a range of integers by its lower and upper limit, print a list of all even numbers and their Goldbach composition.
In most cases, if an even number is written as the sum of two prime numbers, one of them is very small. Very rarely, the 
primes are both bigger than say 50. Try to find out how many such cases there are in the range 2..3000.

    λ> goldbachList 9 20
    [(3,7),(5,7),(3,11),(3,13),(5,13),(3,17)]

    λ> goldbachList' 4 2000 50
    [(73,919),(61,1321),(67,1789),(61,1867)]
-}

goldbachList :: Int -> Int -> [(Int, Int)]
goldbachList m n
    | m < 0 = error "Invalid lower bound."
    | n < 0 = error "Invalid upper bound."
    | m > n = error "Invalid bounds."
goldbachList m n = map goldbach $ filter even [m..n]

goldbachList' :: Int -> Int -> Int -> [(Int, Int)]
goldbachList' m n k = filter (\(x, y) -> x > k && y > k) $ goldbachList m n