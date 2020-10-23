{-# LANGUAGE ScopedTypeVariables #-}

module OrderedTreeTest (testOrderedTree) where

import Data.List (sort)
import OrderedTree (OrderedTree, fromList, insert, toList)
import Test.Tasty
import Test.Tasty.QuickCheck

-- UTILS

newtype AnyOrderedTree a = AnyOrderedTree (OrderedTree a) deriving (Show)

instance (Ord a, Arbitrary a) => Arbitrary (AnyOrderedTree a) where
  arbitrary = (AnyOrderedTree . fromList) <$> listOf arbitrary

isOrdered :: Ord a => OrderedTree a -> Bool
isOrdered t =
  let traversal = toList t
   in traversal == sort traversal

-- TESTS

testOrderedTree :: TestTree
testOrderedTree =
  testGroup
    "Ordered Tree"
    [ testOrderedGeneration,
      testOrderedInsert,
      testOrderedMerge
    ]

testOrderedGeneration :: TestTree
testOrderedGeneration =
  testProperty "generates correctly" $
    (propOrderedGeneration :: OrderedList Int -> Bool)
  where
    propOrderedGeneration :: (Ord a) => OrderedList a -> Bool
    propOrderedGeneration = isOrdered . fromList . getOrdered

testOrderedInsert :: TestTree
testOrderedInsert =
  testProperty "preserves ordering on inserts" $
    (propOrderedInsert :: Int -> AnyOrderedTree Int -> Bool)
  where
    propOrderedInsert :: (Ord a) => a -> AnyOrderedTree a -> Bool
    propOrderedInsert el (AnyOrderedTree t) = isOrdered (insert el t)

testOrderedMerge :: TestTree
testOrderedMerge =
  testProperty "preserves ordering on merges" $
    (propOrderedMerge :: AnyOrderedTree Int -> AnyOrderedTree Int -> Bool)
  where
    propOrderedMerge :: (Ord a) => AnyOrderedTree a -> AnyOrderedTree a -> Bool
    propOrderedMerge (AnyOrderedTree t1) (AnyOrderedTree t2) = isOrdered (t1 <> t2)
