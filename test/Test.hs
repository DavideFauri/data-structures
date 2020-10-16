import Test.Tasty

import TreeTest (testTree)

main :: IO ()
main = defaultMain $ allTests

allTests :: TestTree
allTests = testGroup "Testing..." $ [ testTree
                                    ]
