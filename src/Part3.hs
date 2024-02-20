module Part3 (
    insertAt,
    range,
    rndSelect,
    diffSelect,
    rndPerm,
    combinations,
    group,
    lsort,
    lfsort
) where
    
import System.Random (newStdGen, randomRs, randomRIO)
import Data.List (sortOn, groupBy, nub)
import Data.Function (on)

-- 21. Insert an element at a given position into a list.
{-
    λ> insertAt 'X' "abcd" 2
    "aXbcd"
-}

insertAt :: a -> [a] -> Int -> [a]
insertAt e [] k
    | k == 1    = [e]
    | otherwise = error "Index out of bounds."
insertAt e (x:xs) k
    | k == 1    = e : x : xs
    | otherwise = x : insertAt e xs (k-1)

-- 22. Create a list containing all integers within a given range.
{-
    λ> range 4 9
    [4,5,6,7,8,9]
-}

range :: Int -> Int -> [Int]
range k limit
    | k > limit     = error "Invalid bounds."
    | k < limit     = k : range (k+1) limit
    | otherwise     = [k]

-- 23. Extract a given number of randomly selected elements from a list.
{-
    λ> rndSelect "abcdefgh" 3
    eda
-}

rndSelect :: [a] -> Int -> IO [a]
rndSelect xs k
    | k < 0     = error "Invalid number."
    | otherwise = newStdGen >>= \generator ->
        return $ take k [xs !! x | x <- randomRs (0, length xs - 1) generator]

-- 24. Lotto: Draw N different random numbers from the set 1..M.
{-
    λ> diffSelect 6 49
    [23,1,17,33,21,37]
-}

diffSelect :: Int -> Int -> IO [Int]
diffSelect n m
    | n < 0 = error "Invaluid number."
    | m < 0 = error "Invalid upper bound."
    | otherwise = newStdGen >>= \generator ->
        return $ take n $ nub $ randomRs (1, m) generator

-- 25. Generate a random permutation of the elements of a list.
{-
    λ> rndPerm "abcdef"
    "badcef"
-}

rndPerm :: [a] -> IO [a]
rndPerm [] = return []
rndPerm xs = do
    index <- randomRIO (0, length xs - 1)
    r <- rndPerm $ take index xs ++ drop (index + 1) xs
    return $ (xs !! index) : r

-- 26. Generate combinations of K distinct objects chosen from the N elements of a list.
{-
    λ> combinations 3 "abcdef"
    ["abc","abd","abe",...]
-}

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations k (x:xs) = map (x:) (combinations (k-1) xs) ++ combinations k xs

-- 27. Group the elements of a set into disjoint subsets.
{-
a) In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3 and 4 persons? Write a function 
that generates all the possibilities and returns them in a list.

    λ> group [2,3,4] ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]
    [[["aldo","beat"],["carla","david","evi"],["flip","gary","hugo","ida"]],...]
    (altogether 1260 solutions)

b) Generalize the above predicate in a way that we can specify a list of group sizes and the predicate will return 
a list of groups.

    λ> group [2,2,5] ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]
    [[["aldo","beat"],["carla","david"],["evi","flip","gary","hugo","ida"]],...]
    (altogether 756 solutions)
-}

group :: Eq a => [Int] -> [a] -> [[[a]]]
group [] _ = [[]]
group (n:ns) xs = [g:gs | g <- combinations n xs, gs <- group ns $ remove xs g]

remove :: Eq a => [a] -> [a] -> [a]
remove xs ys = [x | x <- xs, x `notElem` ys]

-- 28. Sorting a list of lists according to length of sublists.
{-
a) We suppose that a list contains elements that are lists themselves. The objective is to sort the elements of this 
list according to their length. E.g. short lists first, longer lists later, or vice versa.

    λ> lsort ["abc","de","fgh","de","ijkl","mn","o"]
    ["o","de","de","mn","abc","fgh","ijkl"]

b) Again, we suppose that a list contains elements that are lists themselves. But this time the objective is to sort the 
elements of this list according to their length frequency; i.e., in the default, where sorting is done ascendingly, lists 
with rare lengths are placed first, others with a more frequent length come later.

    λ> lfsort ["abc", "de", "fgh", "de", "ijkl", "mn", "o"]
    ["ijkl","o","abc","fgh","de","de","mn"]
-}

lsort :: [[a]] -> [[a]]
lsort [] = []
lsort (x:xs) = lsort [e1 | e1 <- xs, length e1 <= length x] ++ [x] ++ lsort [e2 | e2 <- xs, length e2 > length x]

lfsort :: [[a]] -> [[a]]
lfsort = concat . sortOn length . groupBy ((==) `on` length) . sortOn length