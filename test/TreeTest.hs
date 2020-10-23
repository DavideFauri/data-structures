{-# LANGUAGE GADTs #-}

module TreeTest (testSimpleTree) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Tree

-- UTILS

newtype AnyTree a = AnyTree {toTree :: Tree a} deriving (Show)

instance (Arbitrary a) => Arbitrary (AnyTree a) where
  arbitrary = AnyTree <$> sized makeTree
    where
      makeTree :: (Arbitrary a) => Int -> Gen (Tree a)
      makeTree 0 = pure Leaf
      makeTree n = do
        m <- choose (0, n)
        l <- makeTree m
        c <- arbitrary
        r <- makeTree (n - m)
        pure $ Node l c r

data ExampleTree a = (Show a, Eq a) =>
  ExampleTree
  { name :: String,
    tree :: Tree a,
    toStr :: String,
    toList :: [a]
  }

tests :: ExampleTree a -> TestTree
tests ex =
  testGroup
    (name ex)
    [ testCase "is pretty printed" $ testShow ex,
      testCase "is traversed inorder" $ testTraversal ex
    ]

testShow :: ExampleTree a -> Assertion
testShow ExampleTree {tree = t, toStr = s} = show t @?= s

testTraversal :: ExampleTree a -> Assertion
testTraversal ExampleTree {tree = t, toList = l} = inorder t @?= l

-- TESTS

testSimpleTree :: TestTree
testSimpleTree =
  testGroup
    "Tree"
    [ tests emptyTree,
      tests smallIntTree
    ]

emptyTree :: ExampleTree Int
emptyTree =
  ExampleTree
    { name = "Empty tree",
      tree = Leaf,
      toStr = "",
      toList = []
    }

smallIntTree :: ExampleTree Int
smallIntTree =
  ExampleTree
    { name = "Small tree of Int",
      tree = Node (Node Leaf 1 Leaf) 2 (Node (Node Leaf 4 Leaf) 3 (Node Leaf 5 Leaf)),
      toStr = "2\n  1\n  3\n    4\n    5\n",
      toList = [1, 2, 4, 3, 5]
    }
