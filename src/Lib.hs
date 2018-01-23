module Lib where

-- -----------------------------------------------------------------------------
-- LISTS
-- -----------------------------------------------------------------------------

l1 :: [Int]
l1 = []

-- cons operator
-- (:) :: a -> [a] -> [a]
l2 :: [Int]
l2 = 1 : [2, 3]

-- append operator
-- (++) :: [a] -> [a] -> [a]
-- can only append lists of the same type
l3 :: [Int]
l3 = [1,2] ++ [3,4,5]

l4_v :: Int
l4_v = head [1, 2, 3, 4]

hd      :: [a] -> a  
hd []    = error "empty list is given to hd"
hd (h:_) = h

hd2  :: [a] -> a
hd2 l = case l of
          []  -> error "empty list is given to hd2"
          h:_ -> h

-- -----------------------------------------------------------------------------
-- TUPLES
-- -----------------------------------------------------------------------------

t1 :: (Int, String)
t1  = (3,   "abcd")

t2 :: (Int, String, (Float, Float))
t2  = (3,   "abcd", (0.25,   0.125))

l3' = []
t3' = ([], [])  

first       :: (a, b) -> a
first (a, _) = a

-- -----------------------------------------------------------------------------
-- VARIABLES
-- -----------------------------------------------------------------------------

v1 :: Int
v1 = let x = 1
     in  x + 1

v2 :: Int
v2 = let x = 1
     in  let y = 2
         in  x + y

v2' :: Int
v2' =  x + y
  where
    x = 1
    y = 2

-- Binding by pattern-matching
v3 :: Float
v3 = let (_, _, (x, y)) = t2
     in x + y

v4 :: Int
v4 = let h:_ = [1, 2, 3]
     in  h + 1

-- -----------------------------------------------------------------------------
-- FUNCTIONS
-- -----------------------------------------------------------------------------

f1 :: Int -> Int
f1 x = x + 1

f2 []    = 0
f2 (h:_) = h

-- You can use C-like arguments, which is basically a tuple
f3 :: (Int, Int) -> Bool
f3 (x,y) = x < y

-- Or, you can write "curried" functions  
f4 :: Int -> Int -> Bool
f4 x y  = x < y

f4' :: Int -> Int -> Bool
f4' = \x y -> x < y

f4'' :: Int -> Int -> Bool
f4'' = \x -> \y -> x < y  

-- f4, f4' and f4'' are the same functions
-- which makes it easier the see that the the of f4 is
-- 'a -> ('a -> bool)
-- and hence f4 actually returns a function !

-- By the way, the following two expressions are the same
-- e1 e2 e3 e4 = ((e1 e2) e3) e4

-- Meanwhile the following type signatures are the same
-- a -> b -> c -> d = a -> (b -> (c -> d))


-- -----------------------------------------------------------------------------
-- HIGHER ORDER FUNCTIONS
-- -----------------------------------------------------------------------------

filter' :: (a -> Bool) -> [a] -> [a]
filter' pred l = undefined

partition' :: (a -> Bool) -> [a] -> ([a], [a])
partition' pred l = undefined

quicksort :: [a] -> [a]
quicksort l = undefined

-- -----------------------------------------------------------------------------
-- DATA STRUCTURES
-- -----------------------------------------------------------------------------

data MyDataType = X Int
                | Y String
                deriving (Show, Eq)

-- peano arithmetic
data Peano = Zero
           | Succ Peano
           deriving (Show, Eq)

padd :: Peano -> Peano -> Peano
padd = undefined

p1 :: Peano
p1 = padd (Succ (Succ (Succ Zero))) (Succ (Succ Zero))

pmul :: Peano -> Peano -> Peano
pmul = undefined

p2 :: Peano
p2 = pmul (Succ (Succ (Succ Zero))) (Succ (Succ Zero))

data HashMap k v = Nil
                 | Entry k v (HashMap k v)
                 deriving (Show, Eq)
  
put :: (HashMap k v) -> k -> v -> (HashMap k v)
put m k v = undefined

find :: k -> (HashMap k v) -> Maybe v
find k m = undefined

-- -----------------------------------------------------------------------------
-- MAIN
-- -----------------------------------------------------------------------------

runMain :: IO ()
runMain = putStrLn "main function is unimplemented ..."
