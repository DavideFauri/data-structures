module TreeTest (testTree) where

import Tree
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

testTree :: TestTree
testTree = testGroup "Tree" $
  [testCase "TODO" $ True @?= True]
