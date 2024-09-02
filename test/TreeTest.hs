{-# LANGUAGE GADTs #-}

module TreeTest (testSimpleTree) where

import Data.Foldable (toList)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Tree
import Types (Iso (..), IsoOrd (..))

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
  testGroup "Tree"
    [ testGroup "Unit cases"
        [ testUnitCase (emptyTree :: ExampleTree Double),
          testUnitCase singletonTree,
          testUnitCase smallIntTree,
          testUnitCase smallStringTree
        ],
      testGroup "Instances"
        [ testGroup "Iso"
            [ testIsomorphic (emptyTree :: ExampleTree Double) (emptyTree :: ExampleTree Char),
              testIsomorphic smallFuncTree smallIntTree,
              testNotIsomorphic (emptyTree :: ExampleTree Int) singletonTree,
              testNotIsomorphic smallIntTree (emptyTree :: ExampleTree Double),
              testNotIsomorphic smallIntTree singletonTree,
              testNotIsomorphic smallIntTree superIntTree,
              testNotIsomorphic smallIntTree notSuperIntTree
            ],
          testGroup "IsoOrd"
            [ testSubmorphic (emptyTree :: ExampleTree Int) singletonTree,
              testSubmorphic singletonTree smallIntTree,
              testSubmorphic smallIntTree superIntTree,
              testSupermorphic superIntTree smallIntTree,
              testFailMorphic superIntTree notSuperIntTree
            ]
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

failMorphicMsg :: ExampleTree a -> ExampleTree b -> String
failMorphicMsg t1 t2 =
  "Mismatching structure.\n"
    <> name t1
    <> " has structure:\n"
    <> showStructure (tree t1)
    <> "\nbut\n"
    <> name t2
    <> " has structure:\n"
    <> showStructure (tree t2)

testIsomorphic :: ExampleTree a -> ExampleTree b -> TestTree
testIsomorphic t1 t2 =
  testCase (name t1 <> " is isomorphic to " <> name t2) $
    assertBool (failMorphicMsg t1 t2) $
      tree t1 ~== tree t2

testNotIsomorphic :: ExampleTree a -> ExampleTree b -> TestTree
testNotIsomorphic t1 t2 =
  testCase (name t1 <> " is not isomorphic to " <> name t2) $
    assertBool (failMorphicMsg t1 t2) $
      tree t1 ~/= tree t2

testSubmorphic :: ExampleTree a -> ExampleTree b -> TestTree
testSubmorphic tSub tSuper =
  testCase (name tSub <> " is submorphic to " <> name tSuper) $
    assertEqual
      (failMorphicMsg tSub tSuper)
      (Just LT)
      (Types.compare (tree tSub) (tree tSuper))

testSupermorphic :: ExampleTree a -> ExampleTree b -> TestTree
testSupermorphic tSuper tSub =
  testCase (name tSuper <> " is supermorphic to " <> name tSub) $
    assertEqual
      (failMorphicMsg tSuper tSub)
      (Just GT)
      (Types.compare (tree tSuper) (tree tSub))

testFailMorphic :: ExampleTree a -> ExampleTree b -> TestTree
testFailMorphic t1 t2 =
  testCase (name t1 <> " can't be morphologically compared to " <> name t2) $
    assertEqual
      (failMorphicMsg t1 t2)
      Nothing
      (Types.compare (tree t1) (tree t2))

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

smallStringTree :: ExampleTree String
smallStringTree =
  ExampleTree
    { name = "Small String",
      tree = Node "This" (Node "Sentence" (Node "Is" Leaf Leaf) (Node "Wrong" Leaf Leaf)) Leaf,
      toStr = "\"This\"\n  \"Sentence\"\n    \"Is\"\n    \"Wrong\"\n",
      inorder = ["Is", "Sentence", "Wrong", "This"],
      preorder = ["This", "Sentence", "Is", "Wrong"],
      postorder = ["Is", "Wrong", "Sentence", "This"],
      levelorder = ["This", "Sentence", "Is", "Wrong"]
    }

superIntTree :: ExampleTree Int
superIntTree =
  emptyTree
    { name = "Medium Int (super of small Int)",
      tree = Node 1 (Node 2 (Node 4 Leaf Leaf) (Node 5 (Node 6 Leaf Leaf) Leaf)) (Node 3 Leaf (Node 7 Leaf Leaf))
    }

notSuperIntTree :: ExampleTree Int
notSuperIntTree =
  emptyTree
    { name = "Medium Int (not super of small Int)",
      tree = Node 1 (Node 2 (Node 4 Leaf Leaf) Leaf) (Node 3 Leaf (Node 7 (Node 8 Leaf Leaf) Leaf))
    }

smallFuncTree :: ExampleTree (Int -> Int)
smallFuncTree =
  emptyTree
    { name = "Small functions",
      tree = Node (* 1) (Node (* 10) (Node (* 100) Leaf Leaf) (Node (* 1000) Leaf Leaf)) (Node (* 1000) Leaf Leaf)
    }
