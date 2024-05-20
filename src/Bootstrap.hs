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
import Bootstrap.Data.Bootstrappable.BuildNix (buildNixFor)
import Bootstrap.Data.Bootstrappable.DefaultNix (SrcDir (SrcDirCurrent), defaultNixFor)
import Bootstrap.Data.Bootstrappable.DevContainer
  ( devContainerDockerComposeFor,
    devContainerDockerfileFor,
    devContainerJsonFor,
  )
import Bootstrap.Data.Bootstrappable.Elm.ElmJson (elmJsonFor)
import Bootstrap.Data.Bootstrappable.Elm.IndexHtml (elmIndexHtmlFor)
import Bootstrap.Data.Bootstrappable.Elm.IndexJs (elmIndexJsFor)
import Bootstrap.Data.Bootstrappable.Elm.MainElm (mainElmFor)
import Bootstrap.Data.Bootstrappable.Elm.PackageJson (elmPackageJsonFor)
import Bootstrap.Data.Bootstrappable.Elm.Review.Config
  ( elmReviewConfigFor,
  )
import Bootstrap.Data.Bootstrappable.Elm.Review.ElmJson
  ( elmReviewElmJsonFor,
  )
import Bootstrap.Data.Bootstrappable.Envrc (Envrc (Envrc))
import Bootstrap.Data.Bootstrappable.FlakeNix (flakeNixFor)
import Bootstrap.Data.Bootstrappable.GitPodYml (gitPodYmlFor)
import Bootstrap.Data.Bootstrappable.Gitignore (gitignoreFor)
import Bootstrap.Data.Bootstrappable.GitlabCIConfig (gitlabCIConfigFor)
import Bootstrap.Data.Bootstrappable.Go.Modfile (goModfileFor)
import Bootstrap.Data.Bootstrappable.Haskell.LibHs (libHsFor)
import Bootstrap.Data.Bootstrappable.Haskell.MainHs (mainHsFor)
import Bootstrap.Data.Bootstrappable.Haskell.PackageYaml (packageYamlFor)
import Bootstrap.Data.Bootstrappable.Haskell.PreludeHs (preludeHsFor)
import Bootstrap.Data.Bootstrappable.HaskellPackagesNix (haskellPackagesNixFor)
import Bootstrap.Data.Bootstrappable.NixPreCommitHookConfig (nixPreCommitHookConfigFor)
import Bootstrap.Data.Bootstrappable.NixShell (nixShellFor)
import Bootstrap.Data.Bootstrappable.NixShellCompat (nixShellCompatFor)
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
import Bootstrap.Data.Bootstrappable.Rust.CargoLock (cargoLockFor)
import Bootstrap.Data.Bootstrappable.Rust.CargoToml (cargoTomlFor)
import Bootstrap.Data.Bootstrappable.Rust.MainRs (mainRsFor)
import Bootstrap.Data.Bootstrappable.VSCodeExtensions (vsCodeExtensionsFileFor)
import Bootstrap.Data.Bootstrappable.VSCodeSettings (vsCodeSettingsFor)
import Bootstrap.Data.BuildPlan
  ( BuildPlan (BuildPlan),
    BuildPlanFile,
    bootstrap,
    confirmBuildPlan,
    toBuildPlanFile,
    toBuildPlanFiles,
  )
import Bootstrap.Data.Config
  ( LoadConfigResult
      ( LoadConfigResultError,
        LoadConfigResultFound,
        LoadConfigResultNotFound
      ),
    configFor,
    configProjectName,
    configProjectType,
    configSetUpContinuousIntegration,
    configSetUpPreCommitHooks,
    configSetUpVSCodeDevContainer,
    configUseNixFlakes,
    loadConfig,
  )
import Bootstrap.Data.ContinuousIntegration
  ( ContinuousIntegrationConfig (ContinuousIntegrationConfig),
  )
import Bootstrap.Data.DevContainer (DevContainerConfig (DevContainerConfig, unDevContainerConfig))
import Bootstrap.Data.GHCVersion (printGHCVersion)
import Bootstrap.Data.HList (HList (HNil), (~:))
import Bootstrap.Data.PreCommitHook (PreCommitHooksConfig (PreCommitHooksConfig, unPreCommitHooksConfig))
import Bootstrap.Data.ProjectName (ProjectName, mkProjectName)
import Bootstrap.Data.ProjectType
  ( ArtefactId (ArtefactId),
    ElmMode (ElmModeBare, ElmModeNode),
    ElmModeSimple (ElmModeSimpleBare, ElmModeSimpleNode),
    ElmOptions (ElmOptions, elmOptionElmMode, elmOptionProvideElmReview),
    HaskellOptions (HaskellOptions),
    InstallLombok (InstallLombok),
    InstallMinishift (InstallMinishift),
    JavaOptions (JavaOptions),
    ProjectSuperType
      ( PSTElm,
        PSTGo,
        PSTHaskell,
        PSTJava,
        PSTMinimal,
        PSTNode,
        PSTPython,
        PSTRust
      ),
    ProjectType (Elm, Go, Haskell, Java, Minimal, Node, Python, Rust),
    PythonVersion (Python39),
    SetUpGoBuild (SetUpGoBuild),
    SetUpJavaBuild (NoJavaBuild, SetUpJavaBuild),
    jdkName,
    nodePackageManagerName,
    projectSuperTypeName,
    promptHaskellProjectType,
  )
import Bootstrap.Data.Version (MajorVersion (MajorVersion), displayMajorVersion)
import Bootstrap.Error (CanDieOnError (dieOnError', dieOnErrorWithPrefix))
import Bootstrap.GitPod (resetPermissionsInGitPod)
import Bootstrap.Monad (MonadBootstrap)
import Bootstrap.Niv (initialiseNiv)
import Bootstrap.Nix.Evaluate (NixBinaryPaths, getAvailableGHCVersions, getNixBinaryPaths, getNixConfig, getNixVersion)
import Bootstrap.Nix.Flake (generateIntermediateFlake)
import Bootstrap.Terminal
  ( askIfReproducibleBuildRequired,
    promptChoice,
    promptNonemptyText,
    promptYesNo,
    promptYesNoWithDefault,
    putErrorLn,
    withAttribute,
    withAttributes,
  )
import Bootstrap.Terminal.Icon (icon)
import Bootstrap.Unix (git)
import Control.Lens ((^.))
import Control.Monad.Catch (catchAll)
import Data.Char (isSpace)
import qualified Data.Text as T
import Data.Version (showVersion)
import Paths_nix_bootstrap (version)
import Relude.Extra.Map (alter, toPairs)
import qualified Relude.Extra.Map as M
import qualified Relude.Unsafe as Unsafe
import System.Directory (doesFileExist, doesPathExist, getCurrentDirectory, removeFile)
import System.FilePath (takeFileName)
import System.Terminal
  ( MonadColorPrinter (blue, foreground, green, yellow),
    MonadFormattingPrinter (bold, underlined),
    MonadPrinter (putLn, putText, putTextLn),
    runTerminalT,
    withTerminal,
  )

-- | The name of the current directory (excluding the rest of its path)
newtype CurrentDirectoryName = CurrentDirectoryName Text

nixBootstrap :: IO ()
nixBootstrap = withTerminal $ runTerminalT do
  parseCommand >>= \case
    CommandHelp errs -> showHelp errs
    CommandRun initialRunConfig ->
      do
        mConfig <-
          loadConfig >>= \case
            LoadConfigResultFound c -> pure $ Just c
            LoadConfigResultNotFound -> pure Nothing
            LoadConfigResultError e -> do
              putErrorLn . toText $ displayException e
              putErrorLn "Invalid config file found; please delete the file and try again."
              exitFailure
        showWelcomeMessage
        resetPermissionsInGitPod
        useFlakes <- case mConfig of
          Just cfg ->
            if rcFromScratch initialRunConfig
              then pure $ rcUseFlakes initialRunConfig
              else
                if rcUseFlakes initialRunConfig && not (cfg ^. configUseNixFlakes)
                  then do
                    putErrorLn $
                      "This project has been not configured to use nix flakes; re-run with --"
                        <> fromScratchFlagName
                        <> " and --"
                        <> useFlakesFlagName
                        <> " to re-configure the project to use nix flakes"
                    exitFailure
                  else pure $ cfg ^. configUseNixFlakes
          Nothing -> pure $ rcUseFlakes initialRunConfig
        let nonInteractive = maybe False (const $ rcNonInteractive initialRunConfig) mConfig
            runConfig =
              initialRunConfig
                { rcNonInteractive = nonInteractive,
                  rcUseFlakes = useFlakes
                }
        (currentDirectoryName, nixBinaryPaths) <- performInitialChecks runConfig
        buildPlan <-
          if rcFromScratch runConfig
            then do
              promptFlakesWarning runConfig
              projectName <- promptProjectName currentDirectoryName
              preCommitHooksConfig <- promptPreCommitHooksConfig
              if useFlakes
                then generateIntermediateFlake nixBinaryPaths runConfig projectName
                else initialiseNiv runConfig preCommitHooksConfig
              promptBuildConfig nixBinaryPaths runConfig projectName preCommitHooksConfig
            else case mConfig of
              Just cfg -> do
                withAttributes [bold, foreground yellow] . putTextLn $
                  "Using configuration from state file; re-run with --"
                    <> fromScratchFlagName
                    <> " to ignore the old configuration and bootstrap from scratch."
                makeBuildPlan
                  MakeBuildPlanArgs
                    { mbpNixBinaryPaths = nixBinaryPaths,
                      mbpProjectName = cfg ^. configProjectName,
                      mbpProjectType = cfg ^. configProjectType,
                      mbpPreCommitHooksConfig = cfg ^. configSetUpPreCommitHooks,
                      mbpContinuousIntegrationConfig = cfg ^. configSetUpContinuousIntegration,
                      mbpDevContainerConfig = cfg ^. configSetUpVSCodeDevContainer,
                      mbpRunConfig = runConfig
                    }
              Nothing -> do
                promptFlakesWarning runConfig
                projectName <- promptProjectName currentDirectoryName
                preCommitHooksConfig <- promptPreCommitHooksConfig
                if useFlakes
                  then generateIntermediateFlake nixBinaryPaths runConfig projectName
                  else initialiseNiv runConfig preCommitHooksConfig
                promptBuildConfig nixBinaryPaths runConfig projectName preCommitHooksConfig
        confirmBuildPlan buildPlan >>= \case
          Just confirmedBuildPlan -> do
            bootstrap confirmedBuildPlan
            liftIO (removeFile ".nix-bootstrap.toml" `catchAll` const pass)
            resetPermissionsInGitPod
            showCompletionMessage
            trackAllFilesInGit
          Nothing -> do
            putTextLn "Okay, exiting."
            exitFailure
    CommandVersion -> do
      putTextLn $ "nix-bootstrap version " <> toText (showVersion version)

performInitialChecks :: forall m. MonadBootstrap m => RunConfig -> m (CurrentDirectoryName, NixBinaryPaths)
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
      pure (CurrentDirectoryName currentDirectoryName, nixBinaryPaths)
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

-- | Warns the user that flakes are an experimental feature if they are using them
promptFlakesWarning :: MonadBootstrap m => RunConfig -> m ()
promptFlakesWarning RunConfig {rcUseFlakes} = when rcUseFlakes do
  withAttributes [bold, foreground yellow] do
    putTextLn "Nix Flakes are an experimental feature of nix and their API is subject to breaking changes without warning."
    putTextLn "Flakes bootstrapped with nix-bootstrap may stop working following a nix version upgrade or when users on your team have different nix versions."
  promptYesNo "Please confirm you understand and accept these risks"
    >>= flip unless do
      putTextLn "Okay, exiting."
      exitFailure

-- | Asks the user to enter a project name
promptProjectName :: MonadBootstrap m => CurrentDirectoryName -> m ProjectName
promptProjectName cdn@(CurrentDirectoryName currentDirectoryName) =
  promptNonemptyText (Just currentDirectoryName) "Please enter a project name"
    >>= ( \case
            Just projectName -> pure projectName
            Nothing -> do
              putErrorLn "Invalid project name. Project names must begin with a letter and contain only alphanumerical characters, spaces, dashes (-), and underscores(_)."
              promptProjectName cdn
        )
      . mkProjectName

-- | Asks the user to decide whether they'd like to have pre-commit hooks set up
promptPreCommitHooksConfig :: MonadBootstrap m => m PreCommitHooksConfig
promptPreCommitHooksConfig =
  PreCommitHooksConfig
    <$> promptYesNo "Would you like pre-commit hooks to be included in what nix-bootstrap sets up?"

promptBuildConfig ::
  MonadBootstrap m =>
  NixBinaryPaths ->
  RunConfig ->
  ProjectName ->
  PreCommitHooksConfig ->
  m BuildPlan
promptBuildConfig
  mbpNixBinaryPaths
  mbpRunConfig@RunConfig {rcWithDevContainer}
  mbpProjectName
  mbpPreCommitHooksConfig = do
    mbpDevContainerConfig <-
      DevContainerConfig
        <$> promptYesNoWithDefault
          (unDevContainerConfig <$> rcWithDevContainer)
          "Would you like to set up a VSCode DevContainer?"
    mbpProjectType <- promptProjectType mbpNixBinaryPaths mbpRunConfig mbpDevContainerConfig
    mbpContinuousIntegrationConfig <-
      ContinuousIntegrationConfig
        <$> promptYesNo "Would you like to set up CI with GitLab CI?"
    makeBuildPlan MakeBuildPlanArgs {..}

promptProjectType :: forall m. MonadBootstrap m => NixBinaryPaths -> RunConfig -> DevContainerConfig -> m ProjectType
promptProjectType nixBinaryPaths runConfig devContainerConfig = do
  superType <- promptChoice "Select a project type:" universeNonEmpty projectSuperTypeName
  case superType of
    PSTMinimal -> pure Minimal
    PSTElm -> do
      elmModeSimple <- promptChoice "How would you like to use Elm?" universeNonEmpty \case
        ElmModeSimpleBare -> "On its own"
        ElmModeSimpleNode -> "As part of a Node application"
      elmOptionElmMode <- case elmModeSimple of
        ElmModeSimpleBare -> pure ElmModeBare
        ElmModeSimpleNode ->
          ElmModeNode
            <$> promptChoice "Select a node package manager:" universeNonEmpty nodePackageManagerName
      elmOptionProvideElmReview <-
        promptYesNo
          "Would you like to set up elm-review with a default configuration for code quality?"
      pure $ Elm ElmOptions {..}
    PSTHaskell -> do
      availableGHCVersions <- getAvailableGHCVersions nixBinaryPaths runConfig
      case nonEmpty $ toList availableGHCVersions of
        Just availableGHCVersions' -> do
          haskellProjectType <- promptHaskellProjectType
          ghcVersion <- promptChoice "Select a version of the Glasgow Haskell Compiler:" availableGHCVersions' printGHCVersion
          pure . Haskell $ HaskellOptions ghcVersion haskellProjectType
        Nothing -> putErrorLn "Could not find any versions of GHC in nixpkgs" *> exitFailure
    PSTNode -> do
      packageManager <- promptChoice "Select a package manager:" universeNonEmpty nodePackageManagerName
      pure $ Node packageManager
    PSTGo -> do
      setUpGoBuild <- SetUpGoBuild <$> askIfReproducibleBuildRequired
      pure $ Go setUpGoBuild
    PSTJava -> do
      jdk <- promptChoice "Select a Java Development Kit:" universeNonEmpty jdkName
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
              <$> promptNonemptyText Nothing "Enter your Maven ArtefactId (e.g. the 'demo' in 'com.example.demo'): "
      pure . Java $ JavaOptions installMinishift installLombok setUpJavaBuild jdk
    PSTPython -> pure $ Python Python39
    PSTRust -> pure Rust

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
          buildNix = buildNixFor mbpRunConfig mbpProjectName mbpProjectType
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
          ( configFor mbpProjectName mbpProjectType mbpPreCommitHooksConfig mbpContinuousIntegrationConfig mbpDevContainerConfig (rcUseFlakes mbpRunConfig)
              ~: Envrc mbpPreCommitHooksConfig (rcUseFlakes mbpRunConfig)
              ~: gitignoreFor mbpRunConfig mbpProjectType mbpPreCommitHooksConfig
              ~: initialReadme
              ~: buildNix
              ~: flakeNixFor mbpRunConfig mbpProjectName mbpProjectType mbpPreCommitHooksConfig nixPreCommitHookConfig buildNix
              ~: ( if rcUseFlakes mbpRunConfig
                     then Nothing
                     else Just $ nixShellFor mbpRunConfig mbpProjectType mbpPreCommitHooksConfig nixPreCommitHookConfig
                 )
              ~: ( if rcUseFlakes mbpRunConfig
                     then Nothing
                     else defaultNixFor SrcDirCurrent mbpProjectName mbpProjectType
                 )
              ~: nixShellCompatFor mbpRunConfig
              ~: nixPreCommitHookConfig
              ~: gitlabCIConfigFor mbpContinuousIntegrationConfig mbpRunConfig mbpProjectType nixPreCommitHookConfig
              ~: devContainerDockerComposeFor mbpDevContainerConfig mbpProjectName
              ~: devContainerDockerfileFor mbpDevContainerConfig
              ~: devContainerJsonFor mbpDevContainerConfig mbpProjectName mbpProjectType
              ~: vsCodeExtensionsFileFor mbpProjectType
              ~: vsCodeSettingsFor mbpDevContainerConfig
              ~: goModfile
              ~: pythonRequirementsFile
              ~: mainElmFor mbpProjectType
              ~: elmJsonFor mbpProjectType
              ~: elmReviewElmJsonFor mbpProjectType
              ~: elmReviewConfigFor mbpProjectType
              ~: elmPackageJsonFor mbpProjectType
              ~: elmIndexHtmlFor mbpProjectName mbpProjectType
              ~: elmIndexJsFor mbpProjectType
              ~: packageYamlFor mbpNixBinaryPaths mbpRunConfig mbpProjectName mbpProjectType
              ~: preludeHsFor mbpProjectType
              ~: libHsFor mbpProjectType
              ~: mainHsFor mbpProjectType
              ~: haskellPackagesNixFor mbpProjectType
              ~: cargoLockFor mbpProjectType mbpProjectName
              ~: cargoTomlFor mbpProjectType mbpProjectName
              ~: mainRsFor mbpProjectType
              ~: gitPodYmlFor mbpProjectType
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
