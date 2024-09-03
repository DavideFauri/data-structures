module InfiniteTreeTest (testInfiniteTree) where

import InfiniteTree
import Test.Tasty
import Test.Tasty.HUnit

testInfiniteTree :: TestTree
testInfiniteTree =
  testGroup
    "Infinite Tree"
    [ testCut0,
      testCut1,
      testCut2,
      testCut3
    ]

testCut0 :: TestTree
testCut0 = testCase "Cutting to depth 0" $ show (cut 0 infiniteTupleTree) @?= ""

testCut1 :: TestTree
testCut1 = testCase "Cutting to depth 1" $ show (cut 1 infiniteTupleTree) @?= "-\n"

testCut2 :: TestTree
testCut2 = testCase "Cutting to depth 2" $ show (cut 2 infiniteTupleTree) @?= "-\n  -L\n  -R\n"

testCut3 :: TestTree
testCut3 = testCase "Cutting to depth 3" $ show (cut 3 infiniteTupleTree) @?= "-\n  -L\n    -LL\n    -LR\n  -R\n    -LR\n    -RR\n"
