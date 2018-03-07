module Test.Brainscrambler.ParserSpec
    ( spec
    ) where

import           Universum

import           Test.Hspec            (Spec, describe, it, shouldBe)

import           Brainscrambler.Parser (parse)

spec :: Spec
spec = do
    describe "Parser" $ do
        it "should return the same" $ do
            let source = "+-.[[<#>^]].,23.,5<"
            let Just result = parse source
            show result `shouldBe` source
