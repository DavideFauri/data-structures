{-# LANGUAGE GADTs #-}

module TreeTest (testSimpleTree) where

import Data.Foldable (toList)
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
    inorder :: [a],
    preorder :: [a],
    postorder :: [a],
    levelorder :: [a]
  }

-- TESTS

tests :: ExampleTree a -> TestTree
tests ex =
  testGroup
    (name ex)
    [ testCase "is pretty printed" $ testShow ex,
      testCase "is traversed inorder" $ testInOrder ex,
      testCase "is traversed preorder" $ testPreOrder ex,
      testCase "is traversed postorder" $ testPostOrder ex,
      testCase "is traversed levelorder" $ testLevelOrder ex
    ]

testShow :: ExampleTree a -> Assertion
testShow ExampleTree {tree = t, toStr = s} = show t @?= s

testInOrder :: ExampleTree a -> Assertion
testInOrder ExampleTree {tree = t, inorder = l} = toList (InOrder t) @?= l

testPreOrder :: ExampleTree a -> Assertion
testPreOrder ExampleTree {tree = t, preorder = l} = toList (PreOrder t) @?= l

testPostOrder :: ExampleTree a -> Assertion
testPostOrder ExampleTree {tree = t, postorder = l} = toList (PostOrder t) @?= l

testLevelOrder :: ExampleTree a -> Assertion
testLevelOrder ExampleTree {tree = t, levelorder = l} = toList (LevelOrder t) @?= l

-- UNIT CASES

testSimpleTree :: TestTree
testSimpleTree =
  testGroup
    "Tree"
    [ tests emptyTree,
      tests singletonTree,
      tests smallIntTree
    ]

emptyTree :: ExampleTree Int
emptyTree =
  ExampleTree
    { name = "Empty tree",
      tree = Leaf,
      toStr = "",
      inorder = [],
      preorder = [],
      postorder = [],
      levelorder = []
    }

singletonTree :: ExampleTree String
singletonTree =
  ExampleTree
    { name = "Singleton tree",
      tree = Node Leaf "example" Leaf,
      toStr = "\"example\"\n",
      inorder = ["example"],
      preorder = ["example"],
      postorder = ["example"],
      levelorder = ["example"]
    }

smallIntTree :: ExampleTree Int
smallIntTree =
  ExampleTree
    { name = "Small tree of Int",
      tree = Node (Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)) 1 (Node Leaf 3 Leaf),
      toStr = "1\n  2\n    4\n    5\n  3\n",
      inorder = [4, 2, 5, 1, 3],
      preorder = [1, 2, 4, 5, 3],
      postorder = [4, 5, 2, 3, 1],
      levelorder = [1, 2, 3, 4, 5]
    }
