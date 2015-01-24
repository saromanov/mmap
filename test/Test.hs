import Test.Tasty
import Test.Tasty.HUnit


unitTests = testGroup "Test fromList"
  [ testCase "Example of load with fromList 1" $
      getMap (fromList [(7,4), (5,2), (5,3), (8,12), (7,3)]) `compare` M.fromList [(5,[2,3]),(7,[3,4]),(8,[12])] @?= EQ

  -- the following test does not hold
  , testCase "Example of load with fromList 2" $
      getMap (fromList [(1,2), (5,8), (9,1)]) `compare` M.fromList [(1,[2]),(5,[8]),(9,[1])] @?= EQ
  ]

unitTestsKeys = testGroup "test keys"
 [ testCase "Example of return keys 1" $
		 keys (fromList [(1,2), (5,8), (9,1)]) `compare` [1,5,9] @?= EQ
 ]