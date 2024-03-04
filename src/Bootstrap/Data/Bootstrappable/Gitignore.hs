-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.Bootstrappable.Gitignore (gitignoreFor) where

import Bootstrap.Cli (RunConfig (RunConfig, rcUseFlakes))
import Bootstrap.Data.Bootstrappable (Bootstrappable (bootstrapContent, bootstrapName, bootstrapReason))
import Bootstrap.Data.PreCommitHook (PreCommitHooksConfig, unPreCommitHooksConfig)
import Bootstrap.Data.ProjectType
  ( ElmMode (ElmModeBare, ElmModeNode),
    ElmOptions (ElmOptions, elmOptionElmMode),
    HaskellOptions (HaskellOptions),
    NodePackageManager (NPM, PNPm, Yarn),
    ProjectType (Elm, Go, Haskell, Java, Minimal, Node, Python, Rust),
  )

newtype Gitignore = Gitignore [GitignoreGroup]

instance Bootstrappable Gitignore where
  bootstrapName = const ".gitignore"
  bootstrapReason = const "This tells git what not to track."
  bootstrapContent (Gitignore groups) = pure . Right . unlines . intercalate [""] $ gitignoreGroupLines <$> groups

data GitignoreGroup = GitignoreGroup Text [GitignoreLine] deriving stock (Eq)

instance Ord GitignoreGroup where
  compare (GitignoreGroup comment1 _) (GitignoreGroup comment2 _) = compare comment1 comment2

gitignoreGroupLines :: GitignoreGroup -> [Text]
gitignoreGroupLines (GitignoreGroup comment ls) = ("# " <> comment) : (unGitignoreLine <$> ls)

ggNix :: GitignoreGroup
ggNix =
  GitignoreGroup "Nix Artefacts" $
    GitignoreLine
      <$> [ "result",
            "result-*"
          ]

ggDirenv :: GitignoreGroup
ggDirenv =
  GitignoreGroup "Direnv Config" . one $ GitignoreLine "/.direnv"

ggOSSpecific :: GitignoreGroup
ggOSSpecific =
  GitignoreGroup "OS-Specific" $
    GitignoreLine
      <$> [ ".DS_Store",
            "*~"
          ]

ggPreCommitConfig :: GitignoreGroup
ggPreCommitConfig = GitignoreGroup "Pre-Commit Hooks" [GitignoreLine "/.pre-commit-config.yaml"]

ggElm :: GitignoreGroup
ggElm = GitignoreGroup "Elm" [GitignoreLine "elm-stuff"]

ggElmGeneratedIndexHtml :: GitignoreGroup
ggElmGeneratedIndexHtml = GitignoreGroup "Elm-Generated index.html" [GitignoreLine "index.html"]

ggHaskell :: GitignoreGroup
ggHaskell =
  GitignoreGroup
    "Haskell"
    [ GitignoreLine "dist",
      GitignoreLine "dist-*",
      GitignoreLine "cabal-dev",
      GitignoreLine "*.o",
      GitignoreLine "*.hi",
      GitignoreLine "*.hie",
      GitignoreLine "*.chi",
      GitignoreLine "*.chs.h",
      GitignoreLine "*.dyn_o",
      GitignoreLine "*.dyn_hi",
      GitignoreLine ".hpc",
      GitignoreLine ".hsenv",
      GitignoreLine ".cabal-sandbox",
      GitignoreLine "cabal.sandbox.config",
      GitignoreLine "*.prof",
      GitignoreLine "*.aux",
      GitignoreLine "*.hp",
      GitignoreLine "*.eventlog",
      GitignoreLine ".stack-work/",
      GitignoreLine "cabal.project.local",
      GitignoreLine "cabal.project.local~",
      GitignoreLine ".HTF/",
      GitignoreLine ".ghc.environment.*"
    ]

ggParcel :: GitignoreGroup
ggParcel =
  GitignoreGroup
    "Parcel"
    [ GitignoreLine "/.parcel-cache",
      GitignoreLine "/dist"
    ]

ggNode :: GitignoreGroup
ggNode = GitignoreGroup "Node" [GitignoreLine "/node_modules"]

ggYarnErrorLog :: GitignoreGroup
ggYarnErrorLog = GitignoreGroup "Yarn error log" [GitignoreLine "yarn-error.log"]

ggGoBinariesPlugins :: GitignoreGroup
ggGoBinariesPlugins =
  GitignoreGroup "Binaries for programs and plugins" $
    GitignoreLine
      <$> [ "*.exe",
            "*.exe~",
            "*.dll",
            "*.so",
            "*.dylib"
          ]

ggGoTestBinary :: GitignoreGroup
ggGoTestBinary =
  GitignoreGroup
    "Test binary, built with `go test -c`"
    [GitignoreLine "*.test"]

ggGoCoverage :: GitignoreGroup
ggGoCoverage =
  GitignoreGroup
    "Output of go coverage tool"
    [GitignoreLine "*.out"]

ggGoWorkspace :: GitignoreGroup
ggGoWorkspace =
  GitignoreGroup
    "Go workspace file"
    [GitignoreLine "go.work"]

ggJavaCompiledClass :: GitignoreGroup
ggJavaCompiledClass =
  GitignoreGroup
    "Compiled class file"
    [GitignoreLine "*.class"]

ggJavaLogFile :: GitignoreGroup
ggJavaLogFile =
  GitignoreGroup
    "Log File"
    [GitignoreLine "*.log"]

ggJavaIDEFiles :: GitignoreGroup
ggJavaIDEFiles =
  GitignoreGroup "IDE files" $
    GitignoreLine
      <$> [ "*.ctxt",
            ".settings/",
            ".project",
            ".classpath",
            ".idea/",
            "*.iml"
          ]

ggJavaMobileTools :: GitignoreGroup
ggJavaMobileTools =
  GitignoreGroup
    "Mobile Tools for Java (J2ME)"
    [GitignoreLine ".mtj.tmp/"]

ggJavaPackageFiles :: GitignoreGroup
ggJavaPackageFiles =
  GitignoreGroup "Package Files" $
    GitignoreLine
      <$> [ "*.jar",
            "*.war",
            "*.nar",
            "*.ear",
            "*.zip",
            "*.tar.gz",
            "*.rar"
          ]

ggJavaMavenFiles :: GitignoreGroup
ggJavaMavenFiles =
  GitignoreGroup
    "Maven Files"
    [GitignoreLine "target/"]

ggJavaVMLogs :: GitignoreGroup
ggJavaVMLogs =
  GitignoreGroup "Virtual machine crash logs" $
    GitignoreLine
      <$> [ "hs_err_pid*",
            "replay_pid*"
          ]

ggPythonCompiled :: GitignoreGroup
ggPythonCompiled =
  GitignoreGroup "Byte-compiled / optimised / DLL files" $
    GitignoreLine
      <$> [ "__pycache__/",
            "*.py[cod]",
            "*$py.class"
          ]

ggPythonDistribution :: GitignoreGroup
ggPythonDistribution =
  GitignoreGroup "Distribution / packaging" $
    GitignoreLine
      <$> [ ".Python",
            "build/",
            "develop-eggs/",
            "dist/",
            "downloads/",
            "eggs/",
            ".eggs/",
            "lib/",
            "lib64/",
            "parts/",
            "sdist/",
            "var/",
            "wheels/",
            "share/python-wheels/",
            "*.egg-info/",
            ".installed.cfg",
            "*.egg",
            "MANIFEST"
          ]

ggPythonPyInstaller :: GitignoreGroup
ggPythonPyInstaller =
  GitignoreGroup "PyInstaller" $
    GitignoreLine
      <$> [ "*.manifest",
            "*.spec"
          ]

ggPythonInstaller :: GitignoreGroup
ggPythonInstaller =
  GitignoreGroup "Installer logs" $
    GitignoreLine
      <$> [ "pip-log.txt",
            "pip-delete-this-directory.txt"
          ]

ggPythonUnitTest :: GitignoreGroup
ggPythonUnitTest =
  GitignoreGroup "Unit test" $
    GitignoreLine
      <$> [ "htmlcov/",
            ".tox/",
            ".nox/",
            ".coverage",
            ".coverage.*",
            ".cache",
            "nosetests.xml",
            "coverage.xml",
            "*.cover",
            "*.py,cover",
            ".hypothesis/",
            ".pytest_cache/",
            "cover/"
          ]

ggPythonTranslations :: GitignoreGroup
ggPythonTranslations =
  GitignoreGroup "Translations" $
    GitignoreLine
      <$> [ "*.mo",
            "*.pot"
          ]

ggPythonDjango :: GitignoreGroup
ggPythonDjango =
  GitignoreGroup "Django stuff:" $
    GitignoreLine
      <$> [ "*.log",
            "local_settings.py",
            "db.sqlite3",
            "db.sqlite3-journal"
          ]

ggPythonFlask :: GitignoreGroup
ggPythonFlask =
  GitignoreGroup "Flask stuff:" $
    GitignoreLine
      <$> [ "instance/",
            ".webassets-cache"
          ]

ggPythonScrapy :: GitignoreGroup
ggPythonScrapy =
  GitignoreGroup
    "Scrapy Stuff:"
    [GitignoreLine ".scrapy"]

ggPythonSphinx :: GitignoreGroup
ggPythonSphinx =
  GitignoreGroup
    "Sphinx documentation"
    [GitignoreLine "docs/_build/"]

ggPythonPyBuilder :: GitignoreGroup
ggPythonPyBuilder =
  GitignoreGroup "PyBuilder" $
    GitignoreLine
      <$> [ ".pybuilder/",
            "target/"
          ]

ggPythonJupyter :: GitignoreGroup
ggPythonJupyter =
  GitignoreGroup
    "Jupyter Notebook"
    [GitignoreLine ".ipynb_checkpoints"]

ggPythonIPython :: GitignoreGroup
ggPythonIPython =
  GitignoreGroup "IPython" $
    GitignoreLine
      <$> [ "profile_default/",
            "ipython_config.py"
          ]

ggPythonPyenv :: GitignoreGroup
ggPythonPyenv =
  GitignoreGroup
    "python-version"
    [GitignoreLine ".python-version"]

ggPythonLockFiles :: GitignoreGroup
ggPythonLockFiles =
  GitignoreGroup "Package manager lock files" $
    GitignoreLine
      <$> [ "Pipfile.lock",
            "poetry.lock",
            ".pdm.toml"
          ]

ggPythonPep582 :: GitignoreGroup
ggPythonPep582 =
  GitignoreGroup
    "PEP 582"
    [GitignoreLine "__pypackages__/"]

ggPythonCelery :: GitignoreGroup
ggPythonCelery =
  GitignoreGroup "Celery stuff" $
    GitignoreLine
      <$> [ "celerybeat-schedule",
            "celerybeat.pid"
          ]

ggPythonSageMath :: GitignoreGroup
ggPythonSageMath =
  GitignoreGroup
    "SageMath parsed files"
    [GitignoreLine "*.sage.py"]

ggPythonEnvironments :: GitignoreGroup
ggPythonEnvironments =
  GitignoreGroup "Environments" $
    GitignoreLine
      <$> [ ".env",
            ".venv",
            "env/",
            "venv/",
            "ENV/",
            "env.bak/",
            "venv.bak/"
          ]

ggPythonSpyder :: GitignoreGroup
ggPythonSpyder =
  GitignoreGroup "Spyder project settings" $
    GitignoreLine
      <$> [ ".spyderproject",
            ".spyproject"
          ]

ggPythonRope :: GitignoreGroup
ggPythonRope =
  GitignoreGroup
    "Rope project settings"
    [GitignoreLine ".ropeproject"]

ggPythonMkdocs :: GitignoreGroup
ggPythonMkdocs =
  GitignoreGroup
    "mkdocs documentation"
    [GitignoreLine "/site"]

ggPythonMypy :: GitignoreGroup
ggPythonMypy =
  GitignoreGroup "mypy" $
    GitignoreLine
      <$> [ ".mypy_cache/",
            ".dmypy.json",
            "dmypy.json"
          ]

ggPythonPyre :: GitignoreGroup
ggPythonPyre =
  GitignoreGroup
    "Pyre type checker"
    [GitignoreLine ".pyre/"]

ggPythonPytype :: GitignoreGroup
ggPythonPytype =
  GitignoreGroup
    "pytype static type analyzer"
    [GitignoreLine ".pytype/"]

ggPythonCython :: GitignoreGroup
ggPythonCython =
  GitignoreGroup
    "Cython debug symbols"
    [GitignoreLine "cython_debug/"]

ggPythonPycharm :: GitignoreGroup
ggPythonPycharm =
  GitignoreGroup
    "PyCharm"
    [GitignoreLine ".idea/"]

ggRust :: GitignoreGroup
ggRust =
  GitignoreGroup "Rust" $
    GitignoreLine
      <$> [ "debug/",
            "target/",
            "**/*.rs.bk",
            "*.pdb"
          ]

newtype GitignoreLine = GitignoreLine {unGitignoreLine :: Text} deriving stock (Eq)

gitignoreFor :: RunConfig -> ProjectType -> PreCommitHooksConfig -> Gitignore
gitignoreFor RunConfig {rcUseFlakes} projectType preCommitHooksConfig =
  Gitignore . sort $ baseGroups <> flakeGroups <> preCommitHooksGroups <> projectTypeGroups
  where
    baseGroups :: [GitignoreGroup]
    baseGroups = [ggNix, ggOSSpecific]
    flakeGroups :: [GitignoreGroup]
    flakeGroups = [ggDirenv | rcUseFlakes]
    preCommitHooksGroups :: [GitignoreGroup]
    preCommitHooksGroups = [ggPreCommitConfig | unPreCommitHooksConfig preCommitHooksConfig]
    projectTypeGroups :: [GitignoreGroup]
    projectTypeGroups = case projectType of
      Minimal -> []
      Elm ElmOptions {..} ->
        ggElm : case elmOptionElmMode of
          ElmModeBare -> [ggElmGeneratedIndexHtml]
          ElmModeNode packageManager -> ggParcel : nodeGitignoreGroups packageManager
      Haskell (HaskellOptions _ _) -> [ggHaskell]
      Node packageManager -> nodeGitignoreGroups packageManager
      Go _ -> [ggGoBinariesPlugins, ggGoTestBinary, ggGoCoverage, ggGoWorkspace]
      Java {} -> [ggJavaCompiledClass, ggJavaLogFile, ggJavaIDEFiles, ggJavaMobileTools, ggJavaPackageFiles, ggJavaVMLogs, ggJavaMavenFiles]
      Python _ ->
        [ ggPythonCompiled,
          ggPythonDistribution,
          ggPythonPyInstaller,
          ggPythonInstaller,
          ggPythonUnitTest,
          ggPythonTranslations,
          ggPythonDjango,
          ggPythonFlask,
          ggPythonScrapy,
          ggPythonSphinx,
          ggPythonPyBuilder,
          ggPythonJupyter,
          ggPythonIPython,
          ggPythonPyenv,
          ggPythonLockFiles,
          ggPythonPep582,
          ggPythonCelery,
          ggPythonSageMath,
          ggPythonEnvironments,
          ggPythonSpyder,
          ggPythonRope,
          ggPythonMkdocs,
          ggPythonMypy,
          ggPythonPyre,
          ggPythonPytype,
          ggPythonCython,
          ggPythonPycharm
        ]
      Rust -> [ggRust]
    nodeGitignoreGroups :: NodePackageManager -> [GitignoreGroup]
    nodeGitignoreGroups =
      (ggNode :) . \case
        NPM -> []
        PNPm -> []
        Yarn -> [ggYarnErrorLog]
