import Test.Tasty

import TreeTest (testTree)
import OrderedTreeTest (testOrderedTree)

main :: IO ()
main = defaultMain $ allTests

allTests :: TestTree
allTests = testGroup "" $ [ testTree
                          , testOrderedTree
                          ]
