{-# LANGUAGE GADTs #-}

module TreeTest (testSimpleTree) where

import Test.Tasty
import Test.Tasty.HUnit
import Tree

-- UTILS

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
