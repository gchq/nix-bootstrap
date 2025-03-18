{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Error
  ( InProgressDuration (..),
    runWithProgressMsg,
    CanDieOnError (dieOnError, dieOnError', dieOnErrorWithPrefix),
  )
where

import Bootstrap.Monad (MonadBootstrap)
import Bootstrap.Terminal (putErrorLn, withAttributes)
import System.Terminal
  ( MonadColorPrinter (cyan, foreground, green, red),
    MonadFormattingPrinter (bold, italic),
    MonadPrinter (putText, putTextLn),
  )

data InProgressDuration = Quick | LongRunning

runWithProgressMsg ::
  forall e m a.
  (MonadBootstrap m) =>
  InProgressDuration ->
  Text ->
  ExceptT e m a ->
  m (Either e a)
runWithProgressMsg duration msg action = do
  withAttributes [italic, foreground cyan] . putText $ msg <> msgEnder
  res <- runExceptT action
  case res of
    Left _ ->
      withAttributes [bold, foreground red] $ putTextLn "ERROR"
    Right _ ->
      withAttributes [bold, foreground green] $ putTextLn "DONE"
  pure res
  where
    msgEnder :: Text
    msgEnder = case duration of
      Quick -> "... "
      LongRunning -> " (this may take a while)... "

class CanDieOnError m where
  dieOnError :: (e -> Text) -> ExceptT e m a -> m a

  -- | Convenience function to print exceptions unmodified when dying
  dieOnError' :: (Exception e) => ExceptT e m a -> m a
  dieOnError' = dieOnError (toText . displayException)

  -- | Convenience function to print exceptions unmodified but with a prefix when dying.
  --
  -- Adds a colon and space to the end of the prefix.
  dieOnErrorWithPrefix :: (Exception e) => Text -> ExceptT e m a -> m a
  dieOnErrorWithPrefix prefix = dieOnError (((prefix <> ": ") <>) . toText . displayException)

instance (MonadBootstrap m) => CanDieOnError m where
  dieOnError displayError action =
    runExceptT action >>= \case
      Left e -> putErrorLn (displayError e) >> exitFailure
      Right a -> pure a
