{-# LANGUAGE ScopedTypeVariables #-}

-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Terminal
  ( promptChoice,
    promptMultipleChoice,
    promptMultipleChoiceNonEmpty,
    promptNonemptyText,
    promptYesNo,
    promptYesNoWithCustomPrompt,
    promptYesNoWithDefault,
    promptYesNoWithCustomPromptAndDefault,
    putErrorLn,
    withAttribute,
    withAttributes,
  )
where

import Bootstrap.Monad (MonadBootstrap)
import Bootstrap.State
  ( ChoiceInputState (ChoiceInputState, chosenItem),
    CursorPos,
    InputLine (unInputLine),
    MultipleChoiceInputState (chosenItems, cursorItem),
    TextInputState (currentLine, cursorPos),
    handleBackspacePress,
    handleCharEntry,
    initialMultipleChoiceInputState,
    initialTextInputState,
    updateCursorPos,
  )
import qualified Data.Set as Set
import qualified Data.Text as T
import Relude.Extra.Enum (next, prev)
import System.Terminal
  ( Direction (Downwards, Leftwards, Rightwards, Upwards),
    Event (KeyEvent),
    Interrupt (Interrupt),
    Key (ArrowKey, BackspaceKey, CharKey, EnterKey, SpaceKey),
    MonadColorPrinter (blue, foreground, green, magenta, red, yellow),
    MonadFormattingPrinter (bold),
    MonadMarkupPrinter (Attribute, resetAttributes, setAttribute),
    MonadPrinter (flush, putChar, putLn, putText, putTextLn),
    MonadScreen
      ( deleteChars,
        deleteLines,
        insertChars,
        moveCursorBackward,
        moveCursorUp,
        setCursorColumn
      ),
    awaitEvent,
  )

promptChoice ::
  forall a m.
  (Eq a, MonadBootstrap m) =>
  Text ->
  NonEmpty a ->
  (a -> Text) ->
  m a
promptChoice promptText options optionToText = go (ChoiceInputState $ head options)
  where
    go :: ChoiceInputState a -> m a
    go state = do
      withAttribute (foreground blue) $ putTextLn promptText
      forM_ options printOption
      awaitEvent >>= \case
        Left Interrupt -> exitFailure
        Right (KeyEvent key _) ->
          case key of
            EnterKey ->
              getConfirmation "selected" (chosenItem state) optionToText $
                promptChoice promptText options optionToText
            ArrowKey direction -> case direction of
              Upwards -> goAgain state {chosenItem = prevOption (chosenItem state)}
              Downwards -> goAgain state {chosenItem = nextOption (chosenItem state)}
              _ -> goAgain state
            _ -> goAgain state
        _ -> goAgain state
      where
        printOption :: a -> m ()
        printOption option =
          if chosenItem state == option
            then withAttributes [bold, foreground yellow] . putTextLn $ "> " <> optionToText option
            else putTextLn $ "  " <> optionToText option
        prevOption :: a -> a
        prevOption a = case drop 1 . dropWhile (/= a) . reverse $ toList options of
          [] -> a
          (x : _) -> x
        nextOption :: a -> a
        nextOption a = case drop 1 . dropWhile (/= a) $ toList options of
          [] -> a
          (x : _) -> x
        goAgain :: ChoiceInputState a -> m a
        goAgain nextState = scrubLines (length options + 1) *> go nextState

promptMultipleChoice ::
  forall a m.
  (Bounded a, Enum a, Ord a, MonadBootstrap m) =>
  Text ->
  [a] ->
  (a -> Text) ->
  m [a]
promptMultipleChoice promptText options optionToText = go initialMultipleChoiceInputState
  where
    go :: MultipleChoiceInputState a -> m [a]
    go state = do
      withAttribute (foreground blue) $ putTextLn $ promptText <> " (SPACE to select, ENTER to confirm)"
      forM_ options $ printMultipleChoiceOption state optionToText
      printSummary
      awaitEvent
        >>= handleMultipleChoiceInput
          state
          (getConfirmationMultiple (toList $ chosenItems state) optionToText $ go state)
          goAgain
      where
        printSummary :: m ()
        printSummary = withAttributes [foreground green] do
          putText "  "
          putText . show . length $ chosenItems state
          putText "/"
          putText . show $ length options
          putTextLn " selected"
        goAgain :: MultipleChoiceInputState a -> m [a]
        goAgain nextState = scrubLines (length options + 2) *> go nextState

promptMultipleChoiceNonEmpty ::
  forall a m.
  (Bounded a, Enum a, Ord a, MonadBootstrap m) =>
  Text ->
  NonEmpty a ->
  (a -> Text) ->
  m (NonEmpty a)
promptMultipleChoiceNonEmpty promptText options optionToText = go initialMultipleChoiceInputState
  where
    go :: MultipleChoiceInputState a -> m (NonEmpty a)
    go state = do
      withAttribute (foreground blue) $ putTextLn $ promptText <> " (SPACE to select, ENTER to confirm)"
      forM_ options $ printMultipleChoiceOption state optionToText
      printSummary
      awaitEvent
        >>= handleMultipleChoiceInput
          state
          ( case nonEmpty . toList $ chosenItems state of
              Just items -> getConfirmationMultiple items optionToText $ go state
              Nothing -> goAgain state
          )
          goAgain
      where
        printSummary :: m ()
        printSummary =
          let valid = not . null $ chosenItems state
           in withAttributes [foreground (if valid then green else red)] do
                putText "  "
                putText . show . length $ chosenItems state
                putText "/"
                putText . show $ length options
                putText " selected"
                unless valid $ putText " - select at least 1"
                putLn
        goAgain :: MultipleChoiceInputState a -> m (NonEmpty a)
        goAgain nextState = scrubLines (length options + 2) *> go nextState

-- | Prints a selectable option based on whether it is selected and under the cursoe
printMultipleChoiceOption :: (MonadBootstrap m, Ord a) => MultipleChoiceInputState a -> (a -> Text) -> a -> m ()
printMultipleChoiceOption state optionToText option = do
  let selected = option `Set.member` chosenItems state
      underCursor = cursorItem state == option
  withAttributes [bold, foreground yellow] . putText $ if underCursor then "> " else "  "
  if selected
    then withAttributes [foreground magenta] $ putText "[X] "
    else putText "[ ] "
  withAttributes [foreground magenta | selected] $ putTextLn $ optionToText option

handleMultipleChoiceInput ::
  (MonadIO m, Bounded a, Enum a, Ord a) =>
  MultipleChoiceInputState a ->
  m (t a) ->
  (MultipleChoiceInputState a -> m (t a)) ->
  Either Interrupt Event ->
  m (t a)
handleMultipleChoiceInput state onConfirmation goAgain = \case
  Left Interrupt -> exitFailure
  Right (KeyEvent key _) ->
    case key of
      EnterKey -> onConfirmation
      SpaceKey ->
        if cursorItem state `Set.member` chosenItems state
          then goAgain state {chosenItems = Set.delete (cursorItem state) (chosenItems state)}
          else goAgain state {chosenItems = Set.insert (cursorItem state) (chosenItems state)}
      ArrowKey direction -> case direction of
        Upwards -> goAgain state {cursorItem = prev (cursorItem state)}
        Downwards -> goAgain state {cursorItem = next (cursorItem state)}
        _ -> goAgain state
      _ -> goAgain state
  _ -> goAgain state

-- | Removes the given number of lines from the terminal to allow rewriting
scrubLines :: MonadScreen m => Int -> m ()
scrubLines n = moveCursorUp n *> deleteLines n

promptNonemptyText :: MonadBootstrap m => Maybe Text -> Text -> m Text
promptNonemptyText mDefault promptText = do
  let fullPromptText = case mDefault of
        Just def -> promptText <> " (" <> def <> "):"
        Nothing -> promptText
  withAttribute (foreground blue) $ putText fullPromptText
  response <- T.strip <$> getFreeText (T.length fullPromptText) initialTextInputState
  if T.null response
    then case mDefault of
      Just def -> pure def
      Nothing -> reprompt "Please enter a value." $ promptNonemptyText mDefault promptText
    else getConfirmation "entered" response id $ promptNonemptyText mDefault promptText

promptYesNo :: MonadBootstrap m => Text -> m Bool
promptYesNo = promptYesNoWithDefault Nothing

promptYesNoWithCustomPrompt :: MonadBootstrap m => m Int -> m Bool
promptYesNoWithCustomPrompt = promptYesNoWithCustomPromptAndDefault Nothing

promptYesNoWithDefault :: MonadBootstrap m => Maybe Bool -> Text -> m Bool
promptYesNoWithDefault mDefault promptText = promptYesNoWithCustomPromptAndDefault mDefault do
  withAttribute (foreground blue) $ putText promptText
  pure $ T.length promptText

-- | Prompts for a yes/no reponse unless the default is provided, in which case
-- that answer will be automatically "input".
--
-- The prompt should return its length in characters.
promptYesNoWithCustomPromptAndDefault :: MonadBootstrap m => Maybe Bool -> m Int -> m Bool
promptYesNoWithCustomPromptAndDefault mDefault doPrompt = do
  let promptSuffix = " [yes|no]: "
  preSuffixPromptLength <- doPrompt
  withAttribute (foreground blue) $ putText promptSuffix
  case mDefault of
    Just def -> do
      putTextLn $ if def then "y" else "n"
      pure def
    Nothing ->
      getFreeText (T.length promptSuffix + preSuffixPromptLength) initialTextInputState
        >>= \case
          "y" -> pure True
          "yes" -> pure True
          "n" -> pure False
          "no" -> pure False
          _ -> reprompt "Please answer yes or no." $ promptYesNoWithCustomPrompt doPrompt
          . T.toLower

reprompt :: MonadBootstrap m => Text -> m a -> m a
reprompt msg action = putErrorLn msg >> action

getConfirmation :: MonadBootstrap m => Text -> a -> (a -> Text) -> m a -> m a
getConfirmation verb response responseToText retryAction = do
  putText $ "You " <> verb <> " \""
  withAttributes [bold, foreground magenta] . putText $ responseToText response
  putTextLn "\"."
  confirmed <- promptYesNo "Is that correct?"
  if confirmed
    then pure response
    else retryAction

getConfirmationMultiple :: (MonadBootstrap m, Traversable t) => t a -> (a -> Text) -> m (t a) -> m (t a)
getConfirmationMultiple choices choiceToText retryAction = do
  putTextLn "You selected the following options:"
  forM_ choices \c -> do
    putText "  - "
    withAttributes [bold, foreground magenta] . putTextLn $ choiceToText c
  confirmed <- promptYesNo "Is that correct?"
  if confirmed
    then pure choices
    else retryAction

getFreeText :: MonadBootstrap m => CursorPos -> TextInputState -> m Text
getFreeText minCursorPos state = do
  setCursorColumn $ minCursorPos + cursorPos state
  flush
  awaitEvent >>= \case
    Left Interrupt -> exitFailure
    Right (KeyEvent key modifiers) ->
      case key of
        CharKey c
          | modifiers == mempty -> do
            insertChars 1
            putChar c
            getFreeText minCursorPos $ handleCharEntry state c
          | otherwise -> getFreeText minCursorPos state
        BackspaceKey ->
          if cursorPos state == 0
            then getFreeText minCursorPos state
            else do
              moveCursorBackward 1
              deleteChars 1
              getFreeText minCursorPos $ handleBackspacePress state
        EnterKey -> do
          putLn
          pure . unInputLine $ currentLine state
        ArrowKey direction -> case direction of
          Leftwards -> getFreeText minCursorPos $ updateCursorPos state \p -> p - 1
          Rightwards -> getFreeText minCursorPos $ updateCursorPos state (+ 1)
          _ -> getFreeText minCursorPos state
        _ -> getFreeText minCursorPos state
    _ -> getFreeText minCursorPos state

putErrorLn :: MonadBootstrap m => Text -> m ()
putErrorLn msg = withAttribute (foreground red) (putTextLn msg)

withAttribute :: MonadBootstrap m => Attribute m -> m () -> m ()
withAttribute = withAttributes . one

withAttributes :: MonadBootstrap m => [Attribute m] -> m () -> m ()
withAttributes attrs action = do
  traverse_ setAttribute attrs
  action
  resetAttributes
  flush
