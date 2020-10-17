import OrderedTreeTest (testOrderedTree)
import Test.Tasty
import TreeSearchTest (testTreeSearch)
import TreeTest (testSimpleTree)

main :: IO ()
main = defaultMain $ allTests

allTests :: TestTree
allTests =
  testGroup "" $
    [ testSimpleTree,
      testOrderedTree,
      testTreeSearch
    ]
