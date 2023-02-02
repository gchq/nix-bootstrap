{-# LANGUAGE ScopedTypeVariables #-}

-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap (nixBootstrap) where

import Bootstrap.Cli
  ( Command (CommandHelp, CommandRun, CommandVersion),
    RunConfig (RunConfig, rcAllowDirty, rcFromScratch, rcNonInteractive, rcUseFlakes, rcWithDevContainer),
    allowDirtyFlagName,
    fromScratchFlagName,
    parseCommand,
    showHelp,
    useFlakesFlagName,
  )
import Bootstrap.Data.Bootstrappable
  ( Bootstrappable (bootstrapName),
  )
import Bootstrap.Data.Bootstrappable.BootstrapState
  ( BootstrapState
      ( stateContinuousIntegrationConfig,
        stateDevContainerConfig,
        statePreCommitHooksConfig,
        stateProjectName,
        stateProjectType,
        stateUseFlakes,
        stateVersion
      ),
    BootstrapVersion (unBootstrapVersion),
    bootstrapStateCodec,
    bootstrapStateFileName,
    bootstrapStateFor,
  )
import Bootstrap.Data.Bootstrappable.DefaultNix (defaultNixFor)
import Bootstrap.Data.Bootstrappable.DevContainer
  ( devContainerDockerComposeFor,
    devContainerDockerfileFor,
    devContainerJsonFor,
  )
import Bootstrap.Data.Bootstrappable.Envrc (Envrc (Envrc))
import Bootstrap.Data.Bootstrappable.FlakeNix (flakeNixFor)
import Bootstrap.Data.Bootstrappable.GitPodYml (GitPodYml (GitPodYml))
import Bootstrap.Data.Bootstrappable.Gitignore (gitignoreFor)
import Bootstrap.Data.Bootstrappable.GitlabCIConfig (gitlabCIConfigFor)
import Bootstrap.Data.Bootstrappable.Go.Modfile (goModfileFor)
import Bootstrap.Data.Bootstrappable.NixPreCommitHookConfig (nixPreCommitHookConfigFor)
import Bootstrap.Data.Bootstrappable.NixShell (nixShellFor)
import Bootstrap.Data.Bootstrappable.Python.Requirements (Requirements (Requirements))
import Bootstrap.Data.Bootstrappable.Readme
  ( Readme
      ( Readme,
        readmeHasDevContainer,
        readmeMBuildPlan,
        readmeProjectName,
        readmeProjectType,
        readmeUseFlakes
      ),
  )
import Bootstrap.Data.Bootstrappable.VSCodeSettings (vsCodeSettingsFor)
import Bootstrap.Data.BuildPlan
  ( BuildPlan (BuildPlan),
    BuildPlanFile,
    bootstrap,
    confirmBuildPlan,
    toBuildPlanFile,
    toBuildPlanFiles,
  )
import Bootstrap.Data.ContinuousIntegration
  ( ContinuousIntegrationConfig (ContinuousIntegrationConfig),
  )
import Bootstrap.Data.DevContainer (DevContainerConfig (DevContainerConfig, unDevContainerConfig))
import Bootstrap.Data.HList (HList (HNil), (~:))
import Bootstrap.Data.PreCommitHook (PreCommitHooksConfig (PreCommitHooksConfig, unPreCommitHooksConfig))
import Bootstrap.Data.ProjectName (ProjectName, mkProjectName)
import Bootstrap.Data.ProjectType
  ( ArtefactId (ArtefactId),
    InstallLombok (InstallLombok),
    InstallMinishift (InstallMinishift),
    ProjectSuperType (PSTGo, PSTJava, PSTMinimal, PSTNode, PSTPython),
    ProjectType (Go, Java, Minimal, Node, Python),
    PythonVersion (Python39),
    SetUpGoBuild (SetUpGoBuild),
    SetUpJavaBuild (NoJavaBuild, SetUpJavaBuild),
    nodePackageManagerName,
    projectSuperTypeName,
  )
import Bootstrap.Data.Version (MajorVersion (MajorVersion), displayMajorVersion, parseMajorVersion, toMajorVersion)
import Bootstrap.Error (CanDieOnError (dieOnError', dieOnErrorWithPrefix))
import Bootstrap.GitPod (resetPermissionsInGitPod)
import Bootstrap.Monad (MonadBootstrap)
import Bootstrap.Niv (initialiseNiv)
import Bootstrap.Nix.Evaluate (NixBinaryPaths, getNixBinaryPaths, getNixConfig, getNixVersion)
import Bootstrap.Nix.Flake (generateIntermediateFlake)
import Bootstrap.Terminal
  ( promptChoice,
    promptNonemptyText,
    promptYesNo,
    promptYesNoWithCustomPrompt,
    promptYesNoWithDefault,
    putErrorLn,
    withAttribute,
    withAttributes,
  )
import Bootstrap.Terminal.Icon (icon)
import Bootstrap.Unix (git)
import Data.Char (isSpace)
import qualified Data.Text as T
import Data.Version (showVersion)
import Paths_nix_bootstrap (version)
import Relude.Extra.Map (alter, toPairs)
import qualified Relude.Extra.Map as M
import qualified Relude.Unsafe as Unsafe
import System.Directory (doesFileExist, doesPathExist, getCurrentDirectory)
import System.FilePath (takeFileName)
import System.Terminal
  ( MonadColorPrinter (blue, foreground, green, yellow),
    MonadFormattingPrinter (bold, underlined),
    MonadPrinter (putLn, putText, putTextLn),
    runTerminalT,
    withTerminal,
  )
import Toml (decodeFileEither, prettyTomlDecodeErrors)

nixBootstrap :: IO ()
nixBootstrap = withTerminal $ runTerminalT do
  parseCommand >>= \case
    CommandHelp errs -> showHelp errs
    CommandRun initialRunConfig ->
      do
        mBootstrapState <- getBootstrapState
        showWelcomeMessage
        resetPermissionsInGitPod
        useFlakes <- case mBootstrapState of
          Just bootstrapState ->
            if rcFromScratch initialRunConfig
              then pure $ rcUseFlakes initialRunConfig
              else
                if rcUseFlakes initialRunConfig && not (stateUseFlakes bootstrapState)
                  then do
                    putErrorLn $
                      "This project has been not configured to use nix flakes; re-run with --"
                        <> fromScratchFlagName
                        <> " and --"
                        <> useFlakesFlagName
                        <> " to re-configure the project to use nix flakes"
                    exitFailure
                  else pure $ stateUseFlakes bootstrapState
          Nothing -> pure $ rcUseFlakes initialRunConfig
        let nonInteractive = case mBootstrapState of
              Just _ -> rcNonInteractive initialRunConfig
              Nothing -> False
            runConfig =
              initialRunConfig
                { rcNonInteractive = nonInteractive,
                  rcUseFlakes = useFlakes
                }
        nixBinaryPaths <- performInitialChecks runConfig
        buildPlan <-
          if rcFromScratch runConfig
            then promptBuildConfig nixBinaryPaths runConfig
            else case mBootstrapState of
              Just bootstrapState -> do
                withAttributes [bold, foreground yellow] . putTextLn $
                  "Using configuration from state file; re-run with --"
                    <> fromScratchFlagName
                    <> " to ignore the old configuration and bootstrap from scratch."
                let mbpPreCommitHooksConfig = statePreCommitHooksConfig bootstrapState
                if useFlakes
                  then generateIntermediateFlake nixBinaryPaths runConfig (stateProjectName bootstrapState)
                  else initialiseNiv runConfig mbpPreCommitHooksConfig
                makeBuildPlan
                  MakeBuildPlanArgs
                    { mbpNixBinaryPaths = nixBinaryPaths,
                      mbpProjectName = stateProjectName bootstrapState,
                      mbpProjectType = stateProjectType bootstrapState,
                      mbpPreCommitHooksConfig,
                      mbpContinuousIntegrationConfig = stateContinuousIntegrationConfig bootstrapState,
                      mbpDevContainerConfig = stateDevContainerConfig bootstrapState,
                      mbpRunConfig = runConfig
                    }
              Nothing -> promptBuildConfig nixBinaryPaths runConfig
        confirmBuildPlan buildPlan >>= \case
          Just confirmedBuildPlan -> do
            bootstrap confirmedBuildPlan
            resetPermissionsInGitPod
            showCompletionMessage
            trackAllFilesInGit
          Nothing -> do
            putTextLn "Okay, exiting."
            exitFailure
    CommandVersion -> do
      putTextLn $ "nix-bootstrap version " <> toText (showVersion version)

performInitialChecks :: forall m. MonadBootstrap m => RunConfig -> m NixBinaryPaths
performInitialChecks rc@RunConfig {rcUseFlakes} = do
  currentDirectoryName <- toText . takeFileName <$> liftIO getCurrentDirectory
  when (currentDirectoryName == "nix-bootstrap" || currentDirectoryName == "nix-bootstrap-hs") $
    putErrorLn "In nix-bootstrap directory; exiting. (Test in another directory.)" >> exitFailure
  runExceptT getNixBinaryPaths >>= \case
    Left e -> do
      putErrorLn . toText $ displayException e
      putErrorLn "Could not get the nix binary path. If it's not installed, please install it by running the following command:"
      putLn
      putText "  "
      withAttribute underlined $ putTextLn "sh <(curl -L https://nixos.org/nix/install) --daemon"
      exitFailure
    Right nixBinaryPaths -> do
      inGitRepo <- liftIO $ doesPathExist ".git"
      if inGitRepo
        then checkWorkingStateIsClean rc
        else offerToInitialiseGitRepo
      when rcUseFlakes $ checkNixFlakesAreConfigured nixBinaryPaths
      pure nixBinaryPaths
  where
    checkWorkingStateIsClean :: RunConfig -> m ()
    checkWorkingStateIsClean RunConfig {rcAllowDirty} = do
      statusOutput <-
        fmap toText
          . dieOnErrorWithPrefix "Could not check current git working tree state"
          . ExceptT
          $ git ["status", "-s"]
      unless (T.null (T.strip statusOutput)) do
        withAttributes [bold, foreground yellow] $ putTextLn "Git working tree is not clean."
        unless rcAllowDirty do
          putErrorLn $ "Refusing to proceed without the --" <> allowDirtyFlagName <> " flag enabled."
          exitFailure
    checkNixFlakesAreConfigured :: NixBinaryPaths -> m ()
    checkNixFlakesAreConfigured nixBinaryPaths = checkNixVersion *> checkExperimentalFeatures
      where
        checkNixVersion :: m ()
        checkNixVersion = do
          putTextLn "Checking nix flakes are configured"
          nixVersion <- dieOnError' $ getNixVersion nixBinaryPaths
          let requiredNixVersion = MajorVersion 2 4
          when (nixVersion < requiredNixVersion) do
            putErrorLn $
              "Your nix version ("
                <> displayMajorVersion nixVersion
                <> ") doesn't support flakes. Please upgrade to nix >= "
                <> displayMajorVersion requiredNixVersion
                <> " or remove the --"
                <> useFlakesFlagName
                <> " flag."
            exitFailure
        checkExperimentalFeatures :: m ()
        checkExperimentalFeatures = do
          nixConfig <- dieOnError' $ getNixConfig nixBinaryPaths
          case M.lookup "experimental-features" nixConfig of
            Just experimentalFeaturesConfig -> do
              let enabledFeatures = T.strip <$> T.split isSpace experimentalFeaturesConfig
              unless
                ("flakes" `elem` enabledFeatures && "nix-command" `elem` enabledFeatures)
                showFlakeConfigError
            _ -> showFlakeConfigError
        showFlakeConfigError :: m a
        showFlakeConfigError = do
          putErrorLn "Nix flakes are not properly configured on this system."
          putText "Visit "
          withAttribute underlined $ putText "https://nixos.wiki/wiki/flakes#Installing_flakes"
          putTextLn " to find out how to configure them."
          exitFailure

offerToInitialiseGitRepo :: forall m. MonadBootstrap m => m ()
offerToInitialiseGitRepo = do
  putErrorLn "No `.git` found relative to the current path."
  promptYesNo "Would you like to intialise git in the current directory?" >>= \case
    True ->
      void
        . dieOnErrorWithPrefix "Something went wrong initialising the git repository; exiting"
        . ExceptT
        $ git ["init"]
    False -> do
      putTextLn "Okay, exiting."
      exitFailure

showWelcomeMessage :: MonadBootstrap m => m ()
showWelcomeMessage = do
  withAttributes [bold, foreground blue] $ putTextLn `mapM_` icon
  putLn
  putTextLn "This program will take you through the process of setting up a new project with nix-based infrastructure."
  putTextLn "It is expected to be run in an empty git repository."

promptProjectName :: MonadBootstrap m => m ProjectName
promptProjectName =
  promptNonemptyText "Please enter a project name: "
    >>= ( \case
            Just projectName -> pure projectName
            Nothing -> do
              putErrorLn "Invalid project name. Project names must begin with a letter and contain only alphanumerical characters, spaces, dashes (-), and underscores(_)."
              promptProjectName
        )
      . mkProjectName

getBootstrapState :: forall m. MonadBootstrap m => m (Maybe BootstrapState)
getBootstrapState = do
  stateFileExists <- liftIO $ doesFileExist bootstrapStateFileName
  if stateFileExists
    then do
      decodedState <- decodeFileEither bootstrapStateCodec bootstrapStateFileName
      case decodedState of
        Left e -> do
          putErrorLn $ "Error reading state file: " <> prettyTomlDecodeErrors e
          abort
        Right bootstrapState -> do
          case parseMajorVersion . unBootstrapVersion $ stateVersion bootstrapState of
            Nothing -> do
              putErrorLn "Failed to parse the state file's version number"
              abort
            Just stateMajorVersion -> do
              case toMajorVersion version of
                Just bootstrapVersion -> do
                  if bootstrapVersion == stateMajorVersion
                    then pure $ Just bootstrapState
                    else do
                      withAttributes [bold, foreground yellow] $ putTextLn "WARNING: Cannot use existing state file as it has a different major version number"
                      promptYesNo "Would you like to upgrade?" >>= \case
                        True -> pure Nothing
                        False -> exitFailure
                Nothing -> do
                  putErrorLn "Failed to parse state file's version number"
                  abort
    else pure Nothing
  where
    abort :: m a
    abort = do
      putErrorLn "To bootstrap from scratch, delete .nix-bootstrap.toml and run nix-bootstrap again."
      exitFailure

promptBuildConfig :: MonadBootstrap m => NixBinaryPaths -> RunConfig -> m BuildPlan
promptBuildConfig mbpNixBinaryPaths mbpRunConfig@RunConfig {rcUseFlakes, rcWithDevContainer} = do
  when rcUseFlakes do
    withAttributes [bold, foreground yellow] do
      putTextLn "Nix Flakes are an experimental feature of nix and their API is subject to breaking changes without warning."
      putTextLn "Flakes bootstrapped with nix-bootstrap may stop working following a nix version upgrade or when users on your team have different nix versions."
    promptYesNo "Please confirm you understand and accept these risks"
      >>= flip unless do
        putTextLn "Okay, exiting."
        exitFailure
  mbpProjectName <- promptProjectName
  mbpDevContainerConfig <-
    DevContainerConfig
      <$> promptYesNoWithDefault
        (unDevContainerConfig <$> rcWithDevContainer)
        "Would you like to set up a VSCode DevContainer?"
  mbpProjectType <- promptProjectType mbpDevContainerConfig
  mbpPreCommitHooksConfig <- PreCommitHooksConfig <$> promptYesNo "Would you like to set up pre-commit hooks?"
  mbpContinuousIntegrationConfig <-
    ContinuousIntegrationConfig
      <$> promptYesNo "Would you like to set up CI with GitLab CI?"
  if rcUseFlakes
    then generateIntermediateFlake mbpNixBinaryPaths mbpRunConfig mbpProjectName
    else initialiseNiv mbpRunConfig mbpPreCommitHooksConfig
  makeBuildPlan MakeBuildPlanArgs {..}

promptProjectType :: forall m. MonadBootstrap m => DevContainerConfig -> m ProjectType
promptProjectType devContainerConfig = do
  superType <- promptChoice "Select a project type:" universe projectSuperTypeName
  case superType of
    PSTMinimal -> pure Minimal
    PSTNode -> do
      packageManager <- promptChoice "Select a package manager:" universe nodePackageManagerName
      pure $ Node packageManager
    PSTGo -> do
      setUpGoBuild <- SetUpGoBuild <$> askIfReproducibleBuildRequired
      pure $ Go setUpGoBuild
    PSTJava -> do
      installMinishift <- InstallMinishift <$> promptYesNo "Would you like to install Minishift?"
      installLombok <-
        InstallLombok
          <$> promptYesNoWithDefault
            (if unDevContainerConfig devContainerConfig then Nothing else Just False)
            "Would you like to install the Lombok VSCode extension?"
      setUpJavaBuild <-
        askIfReproducibleBuildRequired >>= \case
          False -> pure NoJavaBuild
          True ->
            SetUpJavaBuild . ArtefactId
              <$> promptNonemptyText "Enter your Maven ArtefactId (e.g. the 'demo' in 'com.example.demo'): "
      pure $ Java installMinishift installLombok setUpJavaBuild
    PSTPython -> pure $ Python Python39
  where
    askIfReproducibleBuildRequired :: m Bool
    askIfReproducibleBuildRequired = promptYesNoWithCustomPrompt do
      let (part1, part2, part3) =
            ( "Would you like to set up a reproducible build for this project ",
              "(EXPERIMENTAL)",
              "?"
            )
      withAttribute (foreground blue) $ putText part1
      withAttributes [bold, foreground yellow] $ putText part2
      withAttribute (foreground blue) $ putText part3
      pure $ sum $ T.length <$> [part1, part2, part3]

data MakeBuildPlanArgs = MakeBuildPlanArgs
  { mbpNixBinaryPaths :: NixBinaryPaths,
    mbpProjectName :: ProjectName,
    mbpProjectType :: ProjectType,
    mbpPreCommitHooksConfig :: PreCommitHooksConfig,
    mbpContinuousIntegrationConfig :: ContinuousIntegrationConfig,
    mbpDevContainerConfig :: DevContainerConfig,
    mbpRunConfig :: RunConfig
  }

makeBuildPlan :: forall m. MonadBootstrap m => MakeBuildPlanArgs -> m BuildPlan
makeBuildPlan MakeBuildPlanArgs {..} = do
  initialBuildPlanMap <- mkInitialBuildPlanMap
  readme <- readmeWithBuildPlan . BuildPlan $ toPairs initialBuildPlanMap
  pure . BuildPlan . toPairs $ alter (const $ pure readme) (bootstrapName initialReadme) initialBuildPlanMap
  where
    initialReadme :: Readme
    initialReadme =
      Readme
        { readmeProjectName = mbpProjectName,
          readmeProjectType = mbpProjectType,
          readmeHasDevContainer = mbpDevContainerConfig,
          readmeMBuildPlan = Nothing,
          readmeUseFlakes = rcUseFlakes mbpRunConfig
        }
    mkInitialBuildPlanMap :: m (Map FilePath BuildPlanFile)
    mkInitialBuildPlanMap = do
      let nixPreCommitHookConfig =
            if unPreCommitHooksConfig mbpPreCommitHooksConfig
              then Just $ nixPreCommitHookConfigFor mbpRunConfig mbpProjectType
              else Nothing
      goModfile <-
        case mbpProjectType of
          Go (SetUpGoBuild True) ->
            Just <$> goModfileFor mbpNixBinaryPaths mbpRunConfig mbpProjectName
          _ -> pure Nothing
      pythonRequirementsFile <- case mbpProjectType of
        Python _ -> do
          requirementsExist <- liftIO $ doesFileExist (bootstrapName Requirements)
          if requirementsExist then pure Nothing else pure $ Just Requirements
        _ -> pure Nothing
      fromList
        <$> toBuildPlanFiles
          ( bootstrapStateFor mbpProjectName mbpProjectType mbpPreCommitHooksConfig mbpContinuousIntegrationConfig mbpDevContainerConfig (rcUseFlakes mbpRunConfig)
              ~: Envrc mbpPreCommitHooksConfig (rcUseFlakes mbpRunConfig)
              ~: gitignoreFor mbpRunConfig mbpProjectType mbpPreCommitHooksConfig
              ~: initialReadme
              ~: flakeNixFor mbpRunConfig mbpProjectName mbpProjectType mbpPreCommitHooksConfig nixPreCommitHookConfig
              ~: ( if rcUseFlakes mbpRunConfig
                     then Nothing
                     else Just $ nixShellFor mbpRunConfig mbpProjectType mbpPreCommitHooksConfig nixPreCommitHookConfig
                 )
              ~: ( if rcUseFlakes mbpRunConfig
                     then Nothing
                     else defaultNixFor mbpProjectName mbpProjectType
                 )
              ~: nixPreCommitHookConfig
              ~: gitlabCIConfigFor mbpContinuousIntegrationConfig mbpRunConfig mbpProjectType mbpPreCommitHooksConfig
              ~: devContainerDockerComposeFor mbpDevContainerConfig mbpProjectName
              ~: devContainerDockerfileFor mbpDevContainerConfig
              ~: devContainerJsonFor mbpDevContainerConfig mbpProjectName mbpProjectType
              ~: vsCodeSettingsFor mbpDevContainerConfig
              ~: goModfile
              ~: pythonRequirementsFile
              ~: GitPodYml
              ~: HNil
          )
    readmeWithBuildPlan :: BuildPlan -> m BuildPlanFile
    readmeWithBuildPlan initialPlan =
      snd . Unsafe.fromJust
        <$> toBuildPlanFile initialReadme {readmeMBuildPlan = Just initialPlan}

showCompletionMessage :: MonadBootstrap m => m ()
showCompletionMessage = do
  putLn
  putTextLn "All steps have finished successfully."
  putTextLn "Once complete, you will be advised to run direnv allow if you're using direnv."
  withAttributes [bold, foreground green] $ putTextLn "nix-bootstrap complete!"

trackAllFilesInGit :: MonadBootstrap m => m ()
trackAllFilesInGit = void $ git ["add", "-N", "."]
