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
        x <- arbitrary
        r <- makeTree (n - m)
        pure $ Node x l r

data ExampleTree a --(Show a, Eq a) =>
  = ExampleTree
  { name :: String,
    tree :: Tree a,
    toStr :: String,
    inorder :: [a],
    preorder :: [a],
    postorder :: [a],
    levelorder :: [a]
  }

-- TESTS

testSimpleTree :: TestTree
testSimpleTree =
  testGroup "Tree" $
    [ testGroup "Unit cases" $
        [ testUnitCase (emptyTree :: ExampleTree Double),
          testUnitCase singletonTree,
          testUnitCase smallIntTree,
          testUnitCase smallStringTree
        ]
    ]

testUnitCase :: (Show a, Eq a) => ExampleTree a -> TestTree
testUnitCase ex =
  testGroup
    (name ex)
    [ testCase "is pretty printed" $ testShow ex,
      testCase "is traversed inorder" $ testInOrder ex,
      testCase "is traversed preorder" $ testPreOrder ex,
      testCase "is traversed postorder" $ testPostOrder ex,
      testCase "is traversed levelorder" $ testLevelOrder ex
    ]

testShow :: Show a => ExampleTree a -> Assertion
testShow ExampleTree {tree = t, toStr = s} = show t @?= s

testInOrder :: (Show a, Eq a) => ExampleTree a -> Assertion
testInOrder ExampleTree {tree = t, inorder = l} = toList (InOrder t) @?= l

testPreOrder :: (Show a, Eq a) => ExampleTree a -> Assertion
testPreOrder ExampleTree {tree = t, preorder = l} = toList (PreOrder t) @?= l

testPostOrder :: (Show a, Eq a) => ExampleTree a -> Assertion
testPostOrder ExampleTree {tree = t, postorder = l} = toList (PostOrder t) @?= l

testLevelOrder :: (Show a, Eq a) => ExampleTree a -> Assertion
testLevelOrder ExampleTree {tree = t, levelorder = l} = toList (LevelOrder t) @?= l

-- UNIT CASES

emptyTree :: ExampleTree a
emptyTree =
  ExampleTree
    { name = "Empty",
      tree = Leaf,
      toStr = "",
      inorder = [],
      preorder = [],
      postorder = [],
      levelorder = []
    }

singletonTree :: ExampleTree Int
singletonTree =
  ExampleTree
    { name = "Singleton",
      tree = Node 42 Leaf Leaf,
      toStr = "42\n",
      inorder = [42],
      preorder = [42],
      postorder = [42],
      levelorder = [42]
    }

smallIntTree :: ExampleTree Int
smallIntTree =
  ExampleTree
    { name = "Small Int",
      tree = Node 1 (Node 2 (Node 4 Leaf Leaf) (Node 5 Leaf Leaf)) (Node 3 Leaf Leaf),
      toStr = "1\n  2\n    4\n    5\n  3\n",
      inorder = [4, 2, 5, 1, 3],
      preorder = [1, 2, 4, 5, 3],
      postorder = [4, 5, 2, 3, 1],
      levelorder = [1, 2, 3, 4, 5]
    }
