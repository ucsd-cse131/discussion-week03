import Test.Tasty
import Test.Tasty.HUnit

import Data.List hiding (find)
import Data.Ord
import Lib

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests = testGroup "Unit tests"
  [ testCase "filter-1" $
      filter' pred l @?= [3, 2, 1]

  , testCase "partition-1" $
      partition' pred l @?= ([3, 2, 1], [8, 6, 7, 9])

  , testCase "qs-1" $
      quicksort l @?= [1, 2, 3, 6, 7, 8, 9]

  , testCase "padd-1" $
      p1 @?= Succ (Succ (Succ (Succ (Succ Zero))))
    
  , testCase "pmul-1" $
      p2 @?= Succ (Succ (Succ (Succ (Succ (Succ Zero)))))

  , testCase "hashmap-1" $
      find "bar" m @?= Just 2
  ]
  where
    l      = [8, 3, 2, 6, 7, 9, 1]
    pred x = x <= 4
    m :: HashMap String Int
    m = put (put Nil "foo" 1) "bar" 2
