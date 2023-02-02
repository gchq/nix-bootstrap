-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.State
  ( CursorPos,
    InputLine (..),
    blankInputLine,
    TextInputState (..),
    initialTextInputState,
    updateCursorPos,
    handleCharEntry,
    handleBackspacePress,
    ChoiceInputState (..),
    initialChoiceInputState,
    MultipleChoiceInputState (..),
    initialMultipleChoiceInputState,
    -- internal; exported for testing
    insertIntoInputLine,
    backspaceInputLine,
    inputLineLength,
  )
where

import qualified Data.Set as Set
import qualified Data.Text as T

type CursorPos = Int

newtype InputLine = InputLine {unInputLine :: Text} deriving stock (Eq, Show)

blankInputLine :: InputLine
blankInputLine = InputLine ""

insertIntoInputLine :: InputLine -> CursorPos -> Char -> InputLine
insertIntoInputLine (InputLine t) pos c = InputLine $ T.take pos t <> one c <> T.drop pos t

backspaceInputLine :: InputLine -> CursorPos -> InputLine
backspaceInputLine (InputLine t) pos = InputLine $ T.take (pos - 1) t <> T.drop pos t

inputLineLength :: InputLine -> CursorPos
inputLineLength = T.length . unInputLine

data TextInputState = TextInputState
  { cursorPos :: CursorPos,
    currentLine :: InputLine
  }
  deriving stock (Eq, Show)

initialTextInputState :: TextInputState
initialTextInputState = TextInputState 0 blankInputLine

updateCursorPos :: TextInputState -> (CursorPos -> CursorPos) -> TextInputState
updateCursorPos s f = s {cursorPos = clamp 0 (inputLineLength $ currentLine s) (f $ cursorPos s)}

handleCharEntry :: TextInputState -> Char -> TextInputState
handleCharEntry s@TextInputState {cursorPos, currentLine} c =
  s
    { cursorPos = cursorPos + 1,
      currentLine = insertIntoInputLine currentLine cursorPos c
    }

handleBackspacePress :: TextInputState -> TextInputState
handleBackspacePress s@TextInputState {cursorPos, currentLine} =
  if cursorPos == 0
    then s
    else
      s
        { cursorPos = cursorPos - 1,
          currentLine = backspaceInputLine currentLine cursorPos
        }

newtype ChoiceInputState a = ChoiceInputState {chosenItem :: a}

initialChoiceInputState :: Bounded a => ChoiceInputState a
initialChoiceInputState = ChoiceInputState minBound

data MultipleChoiceInputState a = MultipleChoiceInputState {chosenItems :: Set a, cursorItem :: a}

initialMultipleChoiceInputState :: Bounded a => MultipleChoiceInputState a
initialMultipleChoiceInputState = MultipleChoiceInputState Set.empty minBound
