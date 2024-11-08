-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Cli
  ( Command (..),
    ErrorMessage,
    RunConfig (..),
    allowDirtyFlagName,
    fromScratchFlagName,
    parseCommand,
    showHelp,
  )
where

import Bootstrap.Data.DevContainer (DevContainerConfig (DevContainerConfig))
import Bootstrap.Monad (MonadBootstrap)
import Bootstrap.Terminal (putErrorLn, withAttribute, withAttributes)
import Data.Char (isSpace)
import qualified Data.Text as T
import System.Console.GetOpt as GetOpt
  ( ArgDescr (NoArg),
    ArgOrder (Permute),
    OptDescr (Option),
    getOpt,
    usageInfo,
  )
import System.Terminal
  ( MonadColorPrinter (blue, foreground),
    MonadFormattingPrinter (bold),
    MonadPrinter (putLn),
    putText,
    putTextLn,
    yellow,
  )

data Command
  = CommandHelp [ErrorMessage]
  | CommandRun RunConfig
  | CommandVersion

newtype ErrorMessage = ErrorMessage {unErrorMessage :: Text}

data RunConfig = RunConfig
  { rcAllowDirty :: Bool,
    rcFromScratch :: Bool,
    rcNonInteractive :: Bool,
    rcWithDevContainer :: Maybe DevContainerConfig
  }

parseCommand :: MonadIO m => m Command
parseCommand = do
  args <- getArgs
  let (options, _, errors) = getOpt Permute cliOptions args
  if not $ null errors
    then pure . CommandHelp $ ErrorMessage . T.strip . toText <$> errors
    else
      if Help `elem` options
        then pure $ CommandHelp []
        else
          if Version `elem` options
            then pure CommandVersion
            else do
              let rcAllowDirty = AllowDirty `elem` options
                  rcFromScratch = FromScratch `elem` options
                  rcNonInteractive = NonInteractive `elem` options
                  rcWithDevContainer =
                    if WithDevContainer `elem` options
                      then Just (DevContainerConfig True)
                      else Nothing
              pure $ CommandRun RunConfig {..}

data Flag
  = AllowDirty
  | FromScratch
  | Help
  | NonInteractive
  | Version
  | WithDevContainer
  deriving stock (Eq)

allowDirtyFlagName :: IsString s => s
allowDirtyFlagName = "allow-dirty"

fromScratchFlagName :: IsString s => s
fromScratchFlagName = "from-scratch"

cliOptions :: [OptDescr Flag]
cliOptions =
  [ GetOpt.Option [] [allowDirtyFlagName] (NoArg AllowDirty) "Allow nix-bootstrap to run even if the current state of the git repo is dirty.",
    GetOpt.Option [] [fromScratchFlagName] (NoArg FromScratch) $
      "Ignore any previous nix-bootstrap state files and re-prompt for every "
        <> "configuration question. Note that this doesn't delete files which are not overwritten in the new configuration.",
    GetOpt.Option [] ["help"] (NoArg Help) "Print the program information and usage.",
    GetOpt.Option [] ["non-interactive"] (NoArg NonInteractive) "When working from an existing state file, don't require any user input.",
    GetOpt.Option [] ["version"] (NoArg Version) "Print the program version.",
    GetOpt.Option [] ["with-devcontainer"] (NoArg WithDevContainer) "Make sure nix-bootstrap sets up a VSCode DevContainer."
  ]

showHelp :: MonadBootstrap m => [ErrorMessage] -> m ()
showHelp errors = do
  mapM_ (putErrorLn . ("Error: " <>) . unErrorMessage) errors
  withAttributes [bold, foreground blue] $ putTextLn "Usage: nix-bootstrap [OPTION]..."
  let usageInfoLines = lines . toText $ usageInfo "Generate infrastructure for common types of project.\n" cliOptions
  case uncons usageInfoLines of
    Nothing -> putLn
    Just (firstLine, restOfLines) -> do
      putTextLn firstLine
      putLn
      forM_ restOfLines \line -> do
        case words line of
          [] -> pass
          (w : ws) -> do
            putText $ T.takeWhile isSpace line
            withAttribute bold $ putText w
            let postFlagSpacing =
                  T.takeWhile isSpace
                    . T.dropWhile (not . isSpace)
                    $ T.dropWhile isSpace line
            putText postFlagSpacing
            forM_ ws \word -> do
              if word == "(EXPERIMENTAL)"
                then withAttributes [bold, foreground yellow] $ putText word
                else putText word
              putText " "
            putLn
