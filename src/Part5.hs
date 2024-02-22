module Part5 (
    table,
    tablen,
    gray,
    huffman
) where

import Data.List (sortOn)

-- 46. & 47. Truth tables for logical expressions.
{-
46. Define predicates and/2, or/2, nand/2, nor/2, xor/2, impl/2 and equ/2 (for logical equivalence) which succeed or fail 
according to the result of their respective operations; e.g. and(A,B) will succeed, if and only if both A and B succeed.
A logical expression in two variables can then be written as in the following example: and(or(A,B),nand(A,B)).
Now, write a predicate table/3 which prints the truth table of a given logical expression in two variables.

    λ> table (\a b -> (and' a (or' a b)))
    True True True
    True False True
    False True False
    False False False

47. Continue Problem 46 by defining and/2, or/2, etc as being operators. This allows to write the logical expression in 
the more natural way, as in the example: A and (A or not B)

table (\a b -> a `and'` (a `or'` not b))
-}

and' :: Bool -> Bool -> Bool
and' True True  = True
and' _ _        = False

or' :: Bool -> Bool -> Bool
or' False False = False
or' _ _         = True

not' :: Bool -> Bool
not' True = False
not' False = True;

nand' :: Bool -> Bool -> Bool
nand' a b = not' $ and' a b

nor' :: Bool -> Bool -> Bool
nor' a b = not' $ or' a b

xor' :: Bool -> Bool -> Bool
xor' True False = True
xor' False True = True
xor' _ _        = False

impl' :: Bool -> Bool -> Bool
impl' a b = not' $ or' a b

equ' :: Bool -> Bool -> Bool
equ' True True      = True
equ' False False    = True
equ' _ _            = False

-- table (\a b -> (and' a (or' a b)))
table :: (Bool -> Bool -> Bool) -> IO ()
table f = putStrLn $ concatMap (++ "\n" )
          [show a ++ " " ++ show b ++ " " ++ show (f a b) | a <- [True, False], b <- [True, False]]

-- 48. Truth tables for logical expressions.
{-
Generalize Problem 47 in such a way that the logical expression may contain any number of logical variables. 
Define table/2 in a way that table(List,Expr) prints the truth table for the expression Expr, which contains 
the logical variables enumerated in List.

    λ> tablen 3 (\[a,b,c] -> a `and'` (b `or'` c) `equ'` a `and'` b `or'` a `and'` c)
    True  True  True  True
    True  True  False True
    True  False True  True
    True  False False True
    False True  True  True
    False True  False True
    False False True  True
    False False False True
-}

tablen :: Int -> ([Bool] -> Bool) -> IO ()
tablen n f = putStrLn $ concatMap (++ "\n" )
    [show arg ++ " " ++ " " ++ show (f arg) | arg <- sequence $ replicate n [True, False]]

-- 49. Gray codes.
{-
An n-bit Gray code is a sequence of n-bit strings constructed according to certain rules. For example,
n = 1: C(1) = ['0','1'].
n = 2: C(2) = ['00','01','11','10'].
n = 3: C(3) = ['000','001','011','010',´110´,´111´,´101´,´100´].

Can you apply the method of "result caching" in order to make the predicate more efficient, when it 
is to be used repeatedly?

    λ> gray 3
    ["000","001","011","010","110","111","101","100"]
-}

gray :: Int -> [String]
gray 0 = [""]
gray n = map ('0':) xs ++ map ('1':) (reverse xs)
    where xs = gray (n-1)

-- 50. Huffman codes.
{-
We suppose a set of symbols with their frequencies, given as a list of fr(S,F) terms. 
Example: [fr(a,45),fr(b,13),fr(c,12),fr(d,16),fr(e,9),fr(f,5)]. Our objective is to construct a list hc(S,C) 
terms, where C is the Huffman code word for the symbol S. In our example, the result could be 
Hs = [hc(a,'0'), hc(b,'101'), hc(c,'100'), hc(d,'111'), hc(e,'1101'), hc(f,'1100')] [hc(a,'01'),...etc.]. 
The task shall be performed by the predicate huffman/2 defined as follows:
    % huffman(Fs,Hs) :- Hs is the Huffman code table for the frequency table Fs

    λ> huffman [('a',45),('b',13),('c',12),('d',16),('e',9),('f',5)]
    [('a',"0"),('b',"101"),('c',"100"),('d',"111"),('e',"1101"),('f',"1100")]
-}

data HuffmanTree a = Node (HuffmanTree a) (HuffmanTree a) Int | Leaf a Int deriving Show

huffman :: [(a, Int)] -> [(a, String)]
huffman freq = generateCodes $ buildTree freq

buildTree :: [(a, Int)] -> HuffmanTree a
buildTree freqList = buildHuffmanTree $ map (\(s, f) -> Leaf s f) $ sortOn snd freqList
  where
    buildHuffmanTree []         = error "Error."
    buildHuffmanTree [node]     = node
    buildHuffmanTree (x:y:xs)   = buildHuffmanTree $ insert (Node x y (freq x + freq y)) xs

    insert node [] = [node]
    insert node (t:ts)
      | freq node <= freq t  = node : t : ts
      | otherwise               = t : insert node ts

    freq (Leaf _ f)     = f
    freq (Node _ _ f)   = f

generateCodes :: HuffmanTree a -> [(a, String)]
generateCodes (Leaf s _)            = [(s, "")]
generateCodes (Node left right _)   = combine '0' left ++ combine '1' right
  where
    combine c = map (\(s, code) -> (s, c : code)) . generateCodes

