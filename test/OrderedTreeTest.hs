module OrderedTreeTest (testOrderedTree) where

import OrderedTree
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

testOrderedTree :: TestTree
testOrderedTree = testGroup "Ordered Tree" $
  [testCase "TODO" $ True @?= True]
