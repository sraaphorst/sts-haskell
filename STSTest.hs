module STSTest where
  
import STSHillclimbing
import Test.HUnit
--import Test.HUnit.Tools (assertRaises)

testSTS7 :: Test
testSTS7 = TestCase (do sts <- makeSTS 7
                        assertEqual "size of STS(7) is 7" 7 (length sts))

testSTS9 :: Test
testSTS9 = TestCase (do sts <- makeSTS 9
                        assertEqual "size of STS(9) is 12" 12 (length sts))
                        
--testSTS11 :: Test
--testSTS11 = TestCase (assertRaises Error("STS only exist for n = 1,3 (mod 6)") (makeSTS 11))

tests = TestList [TestLabel "testSTS7" testSTS7,
                  TestLabel "testSTS9" testSTS9]
                  --TestLabel "testSTS11" testSTS11]

main :: IO Counts
main = runTestTT tests
