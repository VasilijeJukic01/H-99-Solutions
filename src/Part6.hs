module Part6 (
    cbalTree,
    symmetric,
    construct,
    symCbalTrees,
    hbalTree
) where

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

-- 55. Construct completely balanced binary trees.
{-
    λ> cbalTree 4
    [
    -- permutation 1
    --     x
    --    / \
    --   x   x
    --        \
    --         x
    Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty (Branch 'x' Empty Empty)),

    -- permutation 2
    --     x
    --    / \
    --   x   x
    --      /
    --     x
    Branch 'x' (Branch 'x' Empty Empty)  (Branch 'x' (Branch 'x' Empty Empty) Empty),

    -- permutation 3
    --     x
    --    / \
    --   x   x
    --    \
    --     x
    Branch 'x' (Branch 'x' Empty (Branch 'x' Empty Empty)) (Branch 'x' Empty Empty),

    -- permutation 4
    --     x
    --    / \
    --   x   x
    --  /
    -- x
    Branch 'x' (Branch 'x' (Branch 'x' Empty Empty) Empty) (Branch 'x' Empty Empty)
    ]
-}

cbalTree :: Int -> [Tree Char]
cbalTree 0 = [Empty]
cbalTree n = [Branch 'x' left right | i <- [q .. q + r], left <- cbalTree i, right <- cbalTree (n - i - 1)]
    where (q, r) = (n - 1) `quotRem` 2

-- 56. Symmetric binary trees.
{-
Let us call a binary tree symmetric if you can draw a vertical line through the root node and then the right subtree is the mirror 
image of the left subtree. Write a predicate symmetric/1 to check whether a given binary tree is symmetric. Hint: Write a predicate 
mirror/2 first to check whether one tree is the mirror image of another. We are only interested in the structure, not in the 
contents of the nodes.

    λ> symmetric (Branch 'x' (Branch 'x' Empty Empty) Empty)
    False
    λ> symmetric (Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty))
    True
-}

mirror :: Tree a -> Tree a -> Bool
mirror Empty Empty                       = True
mirror (Branch _ l1 r1) (Branch _ l2 r2) = mirror l1 r2 && mirror r1 l2
mirror _ _                               = False

symmetric :: Tree a -> Bool
symmetric Empty             = True
symmetric (Branch _ l r)    = mirror l r

-- 57. Binary search trees.
{-
Use the predicate add/3, developed in chapter 4 of the course, to write a predicate to construct a binary search tree from a 
list of integer numbers.

    λ> construct [3, 2, 5, 7, 1]
    Branch 3 (Branch 2 (Branch 1 Empty Empty) Empty) (Branch 5 Empty (Branch 7 Empty Empty))
    λ> symmetric . construct $ [5, 3, 18, 1, 4, 12, 21]
    True
    λ> symmetric . construct $ [3, 2, 5, 7, 1]
    True
-}

construct :: [Int] -> Tree Int
construct [] = Empty
construct (x:xs) = foldl add (Branch x Empty Empty) xs

add :: Tree Int -> Int -> Tree Int
add Empty x = Branch x Empty Empty
add (Branch y left right) x
    | x < y     = Branch y (add left x) right
    | otherwise = Branch y left (add right x)

-- 58. Generate-and-test paradigm.
{-
Apply the generate-and-test paradigm to construct all symmetric, completely balanced binary trees with a given number of nodes.

    λ> symCbalTrees 5
    [Branch 'x' (Branch 'x' Empty (Branch 'x' Empty Empty)) (Branch 'x' (Branch 'x' Empty Empty) Empty),Branch 'x' (Branch 'x' 
        (Branch 'x' Empty Empty) Empty) (Branch 'x' Empty (Branch 'x' Empty Empty))]
-}

symCbalTrees :: Int -> [Tree Char]
symCbalTrees x = filter symmetric $ cbalTree x

-- 59. Construct height-balanced binary trees.
{-
In a height-balanced binary tree, the following property holds for every node: The height of its left subtree and the height of 
its right subtree are almost equal, which means their difference is not greater than one. Construct a list of all height-balanced 
binary trees with the given element and the given maximum height.

    λ> take 4 $ hbalTree 'x' 3
    [Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty (Branch 'x' Empty Empty)),
    Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' (Branch 'x' Empty Empty) Empty),
    Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty)),
    Branch 'x' (Branch 'x' Empty (Branch 'x' Empty Empty)) (Branch 'x' Empty Empty)]
-}

hbalTree :: a -> Int -> [Tree a]
hbalTree _ 0 = [Empty]
hbalTree x 1 = [Branch x Empty Empty]
hbalTree x h = [Branch x l r | (hl, hr) <- [(h-2, h-1), (h-1, h-1), (h-1, h-2)], 
    l <- hbalTree x hl, 
    r <- hbalTree x hr]