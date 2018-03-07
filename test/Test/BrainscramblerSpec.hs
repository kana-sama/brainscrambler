module Test.BrainscramblerSpec
    ( spec
    ) where

import           Universum

import           Test.Hspec     (Spec, describe, it, shouldBe)

import           Brainscrambler (eval)

spec :: Spec
spec = do
    describe "Brainscrambler" $ do
        it "should work" $ do
            eval "*."               `shouldBe` "0"
            eval "*++."             `shouldBe` "2"
            eval "*+-."             `shouldBe` "0"
            eval "*+*."             `shouldBe` "0"
            eval "*+++-+."          `shouldBe` "3"
            eval "####*."           `shouldBe` "0"
            eval "*.#*+.#*++.#."    `shouldBe` "0120"
            eval "*++>#."           `shouldBe` "2"
            eval "*+++<##."         `shouldBe` "3"
            eval "***++-+>#>#>#."   `shouldBe` "2"
            eval ",5[.-]"           `shouldBe` "54321"
            eval ",9[.-]"           `shouldBe` "987654321"
            eval ",10>*#[-##.+#]"   `shouldBe` "0123456789"
            eval ",4>*++#[-##.++#]" `shouldBe` "2468"
            eval ",9[.--]"          `shouldBe` "97531"
