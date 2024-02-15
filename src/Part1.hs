module Part1 (
    last',
    secondLast,
    elementAt,
    len',
    reverse',
    isPalindrome,
    flat,
    eliminate,
    pack,
    encode
) where

-- 1. Find the last element of a list.
{-
    λ> last' [1,2,3,4]
    4
    λ> last' ['x','y','z']
    'z'
-}

last' :: [a] -> a
last' [] = error "List is empty."
last' [x] = x
last' (_:xs) = last' xs

-- 2. Find the last-but-one (or second-last) element of a list.
{-
    λ> secondLast [1,2,3,4]
    3
    λ> secondLast ['a'..'z']
    'y'
-}

secondLast :: [a] -> a
secondLast [] = error "List is empty."
secondLast [_] = error "One element list"
secondLast [x, _] = x
secondLast (_:xs) = secondLast xs

-- 3. Find the K'th element of a list.
{-
    λ> elementAt [1,2,3] 2
    2
    λ> elementAt "haskell" 5
    'e'
-}

elementAt :: [a] -> Int -> a
elementAt [] _ = error "Index out of bounds"
elementAt (x:_) 1 = x
elementAt (_:xs) k 
    | k < 1     = error "Index out of bounds"
    | otherwise = elementAt xs (k-1)

-- 4. Find the number of elements in a list.
{-
    λ> len' [123, 456, 789]
    3
    λ> len' "Hello, world!"
    13
-}

len' :: [a] -> Int
len' [] = 0
len' (_:xs) = len' xs + 1

-- 5. Reverse a list.
{-
    λ> reverse' "A man, a plan, a canal, panama!"
    "!amanap ,lanac a ,nalp a ,nam A"
    λ> reverse' [1,2,3,4]
    [4,3,2,1]
-}

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

-- 6. Find out whether a list is a palindrome.
{-
    λ> isPalindrome [1,2,3]
    False
    λ> isPalindrome [1,2,4,8,16,8,4,2,1]
    True
-}

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome (x:xs) 
    | x == last xs  = isPalindrome (init xs)
    | otherwise     = False

-- 7. Flatten a nested list structure.
{-
	λ> flat (Elem 5)
	[5]
	λ> flat (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
	[1,2,3,4,5]
	λ> flat (List [])
	[]
-}
data NestedList a = Elem a | List [NestedList a]

flat :: NestedList a -> [a]
flat (Elem x) = [x]
flat (List []) = []
flat (List (x:xs)) = flat x ++ flat (List xs)

-- 8. Eliminate consecutive duplicates of list elements.
{-
    λ> eliminate "aaaabccaadeeee"
    "abcade"
-}

eliminate :: (Eq a) => [a] -> [a]
eliminate [] = error "List is empty."
eliminate [x] = [x]
eliminate (x:xs) 
    | x /= head xs  = x : eliminate xs
    | otherwise     = eliminate xs

-- 9. Pack consecutive duplicates of list elements into sublists.
{-
    λ> pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']
    ["aaaa","b","cc","aa","d","eeee"]
-}

pack :: (Eq a) => [a] -> [[a]]
pack [] = error "List is empty."
pack [x] = [[x]]
pack (x:xs)
    | x == head xs  = (x : head sublist) : tail sublist
    | otherwise     = [x] : sublist
    where sublist = pack xs

-- 10. Run-length encoding of a list.
{-
    λ> encode "aaaabccaadeeee"
    [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
-}

encode :: (Eq a) => [a] -> [(Int, a)]
encode [] = error "List is empty."
encode list = map (\x -> (length x, head x)) (pack list)