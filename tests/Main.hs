module Main where
  
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck


import Data.SparseVector
import qualified Data.Vector as V
import Data.Vector (Vector)
import Instances

main = defaultMain tests

tests =
  [ testGroup "Conversion functions" 
      [ testProperty "toVectors . fromVectors == id" prop_toVectorsFromVectors
      , testProperty "fromVectors . toVectors == id" prop_fromVectorsToVectors
      , testProperty "toLists . fromLists == id" prop_toListsFromLists
      , testProperty "fromLists . toLists == idsort2" prop_fromListsToLists
      ]
  , testGroup "Indexing functions"
      [ testProperty "Indexing on a single segment is sane" prop_indexSingleSegment
      ]
  ]


prop_toVectorsFromVectors xs = toVectors (fromVectors xs) == xs
  where types = (xs :: [(Index, Vector Int)])

prop_fromVectorsToVectors xs = fromVectors (toVectors xs) == xs
  where types = (xs :: SparseVector Int)


prop_toListsFromLists xs = toLists (fromLists xs) == xs
  where types = (xs :: [(Index, [Int])])

prop_fromListsToLists xs = fromLists (toLists xs) == xs
  where types = (xs :: SparseVector Int)


prop_indexSingleSegment xs j i = not (null xs) && i >= 0 ==> Just (xs !! i) == fromList j xs ! (i - j)
  where types = (xs :: [Int])

{-
prop_sort2 xs =
        (not (null xs)) ==>
        (head (sort xs) == minimum xs)
  where types = (xs :: [Int])

prop_sort3 xs = (not (null xs)) ==>
        last (sort xs) == maximum xs
  where types = (xs :: [Int])

prop_sort4 xs ys =
        (not (null xs)) ==>
        (not (null ys)) ==>
        (head (sort (xs ++ ys)) == min (minimum xs) (minimum ys))
  where types = (xs :: [Int], ys :: [Int])

prop_sort5 xs ys =
        (not (null xs)) ==>
        (not (null ys)) ==>
        (head (sort (xs ++ ys)) == max (maximum xs) (maximum ys))
  where types = (xs :: [Int], ys :: [Int])

prop_sort6 xs ys =
        (not (null xs)) ==>
        (not (null ys)) ==>
        (last (sort (xs ++ ys)) == max (maximum xs) (maximum ys))
  where types = (xs :: [Int], ys :: [Int])

test_sort7 = sort [8, 7, 2, 5, 4, 9, 6, 1, 0, 3] @?= [0..9]
-}