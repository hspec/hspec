import Test.Hspec
import Test.Hspec.Contrib.HUnit (fromHUnitTest)
import Test.HUnit

main :: IO ()
main = hspec $ do
  describe "some ordinary spec items" $ do
    it "returns the first element of a list" $ do
      head [23 ..] `shouldBe` (23 :: Int)

  describe "some legacy HUnit tests" $ do
    fromHUnitTest testSuite


-- | A HUnit test suite
testSuite :: Test
testSuite = TestList [
    TestLabel "test_read_is_inverse_to_show" test_read_is_inverse_to_show
  , TestLabel "test_23_is_equal_to_42" test_23_is_equal_to_42
  ]

test_read_is_inverse_to_show :: Test
test_read_is_inverse_to_show = TestCase $ do
  (read . show) (23 :: Int) @?= (23 :: Int)

-- a failing test case
test_23_is_equal_to_42 :: Test
test_23_is_equal_to_42 = TestCase $ do
  23 @?= (42 :: Int)
