module Test.Brainscrambler.InterpreterSpec
    ( spec
    ) where

import           Universum

import           Test.Hspec                 (Spec, describe, it, shouldBe)

import           Brainscrambler.AST         (cycleEnd, cycleStart, decrement,
                                             increment, input, moveHeadToLeft,
                                             moveHeadToRight, output, pushZero,
                                             rotate)
import           Brainscrambler.Interpreter (runBrainscrambler)

spec :: Spec
spec = do
    describe "Interpretator" $ do
        describe "Addition Subtraction Tests" $ do
            it "Current number on stack A should be outputted and be 0" $ do
                outputShouldBe "0" $ do
                    pushZero
                    output
            it "Increment should increment current value" $ do
                outputShouldBe "2" $ do
                    pushZero
                    increment
                    increment
                    output
            it "Decrement should decrement current value" $ do
                outputShouldBe "0" $ do
                    pushZero
                    increment
                    decrement
                    output
            it "New value after push should be zero" $ do
                outputShouldBe "0" $ do
                    pushZero
                    increment
                    pushZero
                    output
            it "3 - 1 + 1 = 3" $ do
                outputShouldBe "3" $ do
                    pushZero
                    increment
                    increment
                    increment
                    decrement
                    increment
                    output
        describe "Rotation Tests" $ do
            it "4xrotate" $ do
                outputShouldBe "0" $ do
                    rotate
                    rotate
                    rotate
                    rotate
                    pushZero
                    output
            it "*.#*+.#*++.#." $ do
                outputShouldBe "0120" $ do
                    pushZero
                    output
                    rotate
                    pushZero
                    increment
                    output
                    rotate
                    pushZero
                    increment
                    increment
                    output
                    rotate
                    output
            it "*++>#." $ do
                outputShouldBe "2" $ do
                    pushZero
                    increment
                    increment
                    moveHeadToRight
                    rotate
                    output
            it "*+++<##." $ do
                outputShouldBe "3" $ do
                    pushZero
                    increment
                    increment
                    increment
                    moveHeadToLeft
                    rotate
                    rotate
                    output
            it "***++-+>#>#>#." $ do
                outputShouldBe "2" $ do
                    pushZero
                    pushZero
                    pushZero
                    increment
                    increment
                    decrement
                    increment
                    moveHeadToLeft
                    rotate
                    moveHeadToLeft
                    rotate
                    moveHeadToLeft
                    rotate
                    output
        describe "Loop Tests" $ do
            it ",5[.-]" $ do
                outputShouldBe "54321" $ do
                    input 5
                    cycleStart
                    output
                    decrement
                    cycleEnd
            it ",9[.-]" $ do
                outputShouldBe "987654321" $ do
                    input 9
                    cycleStart
                    output
                    decrement
                    cycleEnd
            it ",10>*#[-##.+#]" $ do
                outputShouldBe "0123456789" $ do
                    input 10
                    moveHeadToRight
                    pushZero
                    rotate
                    cycleStart
                    decrement
                    rotate
                    rotate
                    output
                    increment
                    rotate
                    cycleEnd
            it ",4>*++#[-##.++#]" $ do
                outputShouldBe "2468" $ do
                    input 4
                    moveHeadToRight
                    pushZero
                    increment
                    increment
                    rotate
                    cycleStart
                    decrement
                    rotate
                    rotate
                    output
                    increment
                    increment
                    rotate
                    cycleEnd
            it ",9[.--]" $ do
                outputShouldBe "97531" $ do
                    input 9
                    cycleStart
                    output
                    decrement
                    decrement
                    cycleEnd
  where
    outputShouldBe x m = runBrainscrambler m `shouldBe` x
