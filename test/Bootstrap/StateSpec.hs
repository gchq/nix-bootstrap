-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.StateSpec (spec) where

import Bootstrap.State
  ( InputLine (InputLine),
    TextInputState (TextInputState),
    backspaceInputLine,
    handleBackspacePress,
    handleCharEntry,
    inputLineLength,
    insertIntoInputLine,
    updateCursorPos,
  )
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)

spec :: Spec
spec = do
  describe "InputLine" do
    describe "insertIntoInputLine" do
      it "inserts into the inputLine at the expected position" $
        insertIntoInputLine (InputLine "hit") 2 'n' `shouldBe` InputLine "hint"
    describe "backspaceInputLine" do
      it "deletes from the inputLine at the expected position" $
        backspaceInputLine (InputLine "hint") 3 `shouldBe` InputLine "hit"
    describe "inputLineLength" do
      it "gives the correct length" $
        inputLineLength (InputLine "hint") `shouldBe` 4
  describe "TextInputState" do
    describe "updateCursorPos" do
      it "correctly applies the update function" $
        updateCursorPos (TextInputState 3 (InputLine "abcdefg")) (+ 3) `shouldBe` TextInputState 6 (InputLine "abcdefg")
      it "cannot move left of zero" $
        updateCursorPos (TextInputState 3 (InputLine "abcdefg")) (\v -> v - 5) `shouldBe` TextInputState 0 (InputLine "abcdefg")
      it "cannot move right of the end of the string" $
        updateCursorPos (TextInputState 3 (InputLine "abcdefg")) (+ 20) `shouldBe` TextInputState 7 (InputLine "abcdefg")
    describe "handleCharEntry" do
      it "inserts a character and updates the cursor position" $
        handleCharEntry (TextInputState 3 (InputLine "abcdefg")) '!' `shouldBe` TextInputState 4 (InputLine "abc!defg")
    describe "handleBackspacePress" do
      it "deletes a character and updates the cursor position" $
        handleBackspacePress (TextInputState 3 (InputLine "abcdefg")) `shouldBe` TextInputState 2 (InputLine "abdefg")
      it "does not update the state when the cursor position is 0" $
        handleBackspacePress (TextInputState 0 (InputLine "abcdefg")) `shouldBe` TextInputState 0 (InputLine "abcdefg")
