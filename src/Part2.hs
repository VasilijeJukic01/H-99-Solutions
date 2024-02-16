module Part2 (
    encodeModified,
    decodeModified,
    encodeDirect,
    duplicate,
    repli,
    dropEvery,
    split,
    slice,
    rotate,
    removeAt
) where

import Part1 (encode)

-- 11. Modified run-length encoding.
{-
Modify the result of problem 10 in such a way that if an element has no duplicates it is simply copied into the result list. 
Only elements with duplicates are transferred as (N E) lists.

    λ> encodeModified "aaaabccaadeeee"
    [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e']
-}

data Encoded a = Single a | Multiple Int a

encodeModified :: (Eq a) => [a] -> [Encoded a]
encodeModified [] = error "List is empty."
encodeModified list = map (\(count, x) -> if count > 1 then Multiple count x else Single x) (encode list)

-- 12. Decode a run-length encoded list.
{-
Given a run-length code list generated as specified in problem 11. Construct its uncompressed version.

    λ> decodeModified [Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e']
    "aaaabccaadeeee"
-}

decodeModified :: (Eq a) => [Encoded a] -> [a]
decodeModified [] = []
decodeModified (x:xs) = decode x ++ decodeModified xs
    where 
        decode (Single x1)       = [x1]
        decode (Multiple n x1)   = replicate n x1;

-- 13. Run-length encoding of a list (direct solution).
{-
Implement the so-called run-length encoding data compression method directly. I.e. don't explicitly create the 
sublists containing the duplicates, as in problem 9, but only count them. As in problem P11, simplify the result 
list by replacing the singleton lists (1 X) by X.

    λ> encodeDirect "aaaabccaadeeee"
    [Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e']
-}

encodeDirect :: (Eq a) => [a] -> [Encoded a]
encodeDirect [] = error "List is empty."
encodeDirect list = map (\(count, x) -> if count > 1 then Multiple count x else Single x) (encode list)

-- 14. Duplicate the elements of a list.
{-
    λ> duplicate [1, 2, 3]
    [1,1,2,2,3,3]
-}

duplicate :: [a] -> [a]
duplicate [] = []
duplicate (x:xs) = x : x : duplicate xs

-- 15. Replicate the elements of a list a given number of times.
{-
    λ> repli "abc" 3
    "aaabbbccc"
-}

repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) k = repli' k x ++ repli xs k
    where repli' k1 x1
            | k1 > 0    = x1 : repli' (k1-1) x1
            | otherwise = []

-- 16. Drop every N'th element from a list.
{-
    λ> dropEvery "abcdefghik" 3
    "abdeghk"
-}

dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery xs n = [x | (x, i) <- zip xs [1..], i `mod` n /= 0]

-- 17. Split a list into two parts; the length of the first part is given.
{-
Do not use any predefined predicates.

    λ> split "abcdefghik" 3
    ("abc", "defghik")
-}

split :: [a] -> Int -> ([a], [a])
split xs k = (leftSide xs k, rightSide xs k)
    where
        leftSide [] _ = []
        leftSide (x1:xs1) k1
            | k1 > 0    = x1 : leftSide xs1 (k1-1)
            | otherwise = []
        rightSide [] _ = []
        rightSide (_:xs2) k2
            | k2 <= 1   = xs2
            | otherwise = rightSide xs2 (k2-1)

-- 18. Extract a slice from a list.
{-
Given two indices, i and k, the slice is the list containing the elements between the i'th and k'th element 
of the original list (both limits included). Start counting the elements with 1.

    λ> slice ['a','b','c','d','e','f','g','h','i','k'] 3 7
    "cdefg"
-}

slice :: [a] -> Int -> Int -> [a]
slice [] _ _ = []
slice (x:xs) i k
    | i > 1             = slice xs (i-1) (k-1)
    | i <= 1 && k >= 1  = x : slice xs i (k-1)
    | otherwise         = slice xs i k

-- 19. Rotate a list N places to the left.
{-
    λ> rotate ['a','b','c','d','e','f','g','h'] 3
    "defghabc"

    λ> rotate ['a','b','c','d','e','f','g','h'] (-2)
    "ghabcdef"
-}

rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate xs 0 = xs
rotate xs n 
  | n < 0       = rotate xs (n + length xs)
  | otherwise   = rotate (tail xs ++ [head xs]) (n - 1)

-- 20. Remove the K'th element from a list.
{-
    λ> removeAt 2 "abcd"
    "acd"
-}

removeAt :: [a] -> Int -> [a]
removeAt [] _ = []
removeAt (x:xs) k
    | k == 1    = removeAt xs (k-1)
    | otherwise = x : removeAt xs (k-1)
