{-# LANGUAGE OverloadedStrings #-}
module RunSpec (main, spec) where

import           Test.Hspec.ShouldBe

import           Run

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "findSpecs" $ do
    context "when specs are not nested" $ do
      it "finds a single spec" $ do
        findSpecs "test-data/single-spec/Spec.hs" `shouldReturn` [SpecNode "Foo" True []]

      it "finds several specs" $ do
        findSpecs "test-data/several-specs/Spec.hs" `shouldReturn` [SpecNode "Bar" True [], SpecNode "Baz" True [], SpecNode "Foo" True []]

    context "when specs are nested" $ do
      it "finds a single spec" $ do
        findSpecs "test-data/single-spec-nested/Spec.hs" `shouldReturn` [SpecNode "Foo" False [SpecNode "Bar" True []]]

      it "properly groups nested specs" $ do
        findSpecs "test-data/nested-spec/Spec.hs" `shouldReturn` [SpecNode "Foo" True [SpecNode "Bar" True [SpecNode "Baz" True []]]]

    context "given a nested spec, without specs at the intermediate nodes" $ do
      it "finds a single spec" $ do
        findSpecs "test-data/no-intermediate-specs/Spec.hs" `shouldReturn` [SpecNode "Foo" False [SpecNode "Bar" False [SpecNode "Baz" True []]]]

    context "given a nested specs, with specs at the intermediate nodes" $ do
      context "with two top-level specs, where one spec name is a prefix of the other" $ do
        it "properly sorts specs" $ do
          findSpecs "test-data/prefix-name/Spec.hs" `shouldReturn`
            [SpecNode "Foo" True [SpecNode "Baz" True []], SpecNode "FooBar" True [SpecNode "Baz" True []]]

    context "when there are no specs" $ do
      it "returns an empty list" $ do
        findSpecs "test-data/empty-dir/Spec.hs" `shouldReturn` []

  describe "formatSpec" $ do
    it "generates code for a spec" $
      formatSpec (SpecNode "Foo" True []) "" `shouldBe` "describe \"Foo\" FooSpec.spec"

    it "generates code for a nested spec" $
      formatSpec (SpecNode "Foo" True [SpecNode "Bar" True [SpecNode "Baz" True []]]) "" `shouldBe`
        "describe \"Foo\" FooSpec.spec >> describe \"Foo.Bar\" Foo.BarSpec.spec >> describe \"Foo.Bar.Baz\" Foo.Bar.BazSpec.spec"

  describe "formatSpecs" $ do
    it "generates code for a list of specs" $ do
      formatSpecs [SpecNode "Bar" True [], SpecNode "Baz" True [], SpecNode "Foo" True []] "" `shouldBe`
        "describe \"Bar\" BarSpec.spec >> describe \"Baz\" BazSpec.spec >> describe \"Foo\" FooSpec.spec"

    it "generates code for an empty list" $ do
      formatSpecs [] "" `shouldBe`
        "return ()"

  describe "formatSpecNested" $ do
    it "generates code for a spec" $
      formatSpecNested (SpecNode "Foo" True []) "" `shouldBe` "describe \"Foo\" (FooSpec.spec)"

    it "generates code for a nested spec" $
      formatSpecNested (SpecNode "Foo" True [SpecNode "Bar" True [SpecNode "Baz" True []]]) "" `shouldBe`
        "describe \"Foo\" (FooSpec.spec >> describe \"Bar\" (Foo.BarSpec.spec >> describe \"Baz\" (Foo.Bar.BazSpec.spec)))"

  describe "formatSpecsNested" $ do
    it "generates code for a list of specs" $ do
      formatSpecsNested [SpecNode "Bar" True [], SpecNode "Baz" True [], SpecNode "Foo" True []] "" `shouldBe`
        "describe \"Bar\" (BarSpec.spec) >> describe \"Baz\" (BazSpec.spec) >> describe \"Foo\" (FooSpec.spec)"

    it "generates code for an empty list" $ do
      formatSpecsNested [] "" `shouldBe`
        "return ()"

  describe "importList" $ do
    it "generates imports for a spec" $ do
      importList [SpecNode "Foo" True []] "" `shouldBe` "import qualified FooSpec\n"

    it "generates imports for a nested spec" $
      importList [SpecNode "Foo" True [SpecNode "Bar" True [SpecNode "Baz" True []]]] "" `shouldBe`
        "import qualified FooSpec\nimport qualified Foo.BarSpec\nimport qualified Foo.Bar.BazSpec\n"

    it "generates imports for a list of specs" $ do
      importList [SpecNode "Bar" True [], SpecNode "Baz" True [], SpecNode "Foo" True []] "" `shouldBe`
        "import qualified BarSpec\nimport qualified BazSpec\nimport qualified FooSpec\n"

    it "generates imports for a nested spec that has no specs at the intermediate nodes" $ do
      importList [SpecNode "Foo" False [SpecNode "Bar" False [SpecNode "Baz" True []]]] "" `shouldBe`
        "import qualified Foo.Bar.BazSpec\n"
