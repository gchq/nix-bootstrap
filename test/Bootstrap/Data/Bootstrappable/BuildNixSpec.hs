{-# LANGUAGE QuasiQuotes #-}

-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.Bootstrappable.BuildNixSpec (spec) where

import Bootstrap.Data.Bootstrappable (Bootstrappable (bootstrapContent))
import Bootstrap.Data.Bootstrappable.BuildNix (buildNixFor)
import Bootstrap.Data.GHCVersion (GHCVersion (GHCVersion))
import Bootstrap.Data.ProjectName (ProjectName, mkProjectName)
import Bootstrap.Data.ProjectType
  ( ArtefactId (ArtefactId),
    HaskellOptions (HaskellOptions),
    HaskellProjectType (HaskellProjectTypeBasic),
    InstallLombok (InstallLombok),
    InstallMinishift (InstallMinishift),
    JavaOptions (JavaOptions),
    JdkPackage (OpenJDK),
    ProjectType (Go, Haskell, Java, Rust),
    SetUpGoBuild (SetUpGoBuild),
    SetUpHaskellBuild (SetUpHaskellBuild),
    SetUpJavaBuild (SetUpJavaBuild),
  )
import qualified Relude.Unsafe as Unsafe
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)
import Text.RawString.QQ (r)

spec :: Spec
spec = describe "build.nix rendering" do
  it "renders correctly for a Go project" do
    case buildNixFor projectName (Go $ SetUpGoBuild True) of
      Just buildNix ->
        bootstrapContent buildNix
          >>= ( `shouldBe`
                  Right
                    [r|{nixpkgs}:
nixpkgs.buildGoModule {
  pname = "test-project";
  version = "0.1.0";
  src = ./.;
  vendorSha256 = null;
  # Swap out the line above for the one below once you start adding dependencies.
  # After your dependencies change, builds will fail until you update the hash below.
  # When the build fails, it will tell you what the expected hash is.
  # vendorSha256 = "sha256-00000000000000000000000000000000000000000000";
}
|]
              )
      Nothing -> fail "Gave nothing for a project which should've had a build.nix generated."

  it "renders correctly for a Java project" do
    case buildNixFor
      projectName
      ( Java $
          JavaOptions
            (InstallMinishift True)
            (InstallLombok True)
            (SetUpJavaBuild $ ArtefactId "testId")
            OpenJDK
      ) of
      Just buildNix ->
        bootstrapContent buildNix
          >>= ( `shouldBe`
                  Right
                    [r|{nixpkgs}: let
  projectName = "test-project";
  artefactId = "testId";
  buildInputs = [nixpkgs.maven];
  repository = nixpkgs.stdenv.mkDerivation {
    inherit buildInputs;
    name = "${projectName}-repository";
    src = ./.;
    buildPhase = "mvn package -Dmaven.repo.local=$out";
    # keep only *.{pom,jar,sha1,nbm} and delete all ephemeral files with lastModified timestamps inside
    installPhase = ''
      find $out -type f \
          -name \*.lastUpdated -or \
          -name resolver-status.properties -or \
          -name _remote.repositories \
          -delete
    '';
    dontFixup = true;
    outputHashAlgo = "sha256";
    outputHashMode = "recursive";
    # This hash will need updating when changing your dependencies; the correct
    # hash will be displayed when the build fails if so.
    outputHash = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";
  };
  builtJar = nixpkgs.stdenv.mkDerivation rec {
    inherit buildInputs;
    pname = projectName;
    version = "0.0.1-SNAPSHOT";
    src = ./.;
    buildPhase = ''
      echo "Using repository ${repository}"
      mvn --offline -Dmaven.repo.local=${repository} package;
    '';
    installPhase = ''
      install -Dm644 target/${artefactId}-${version}.jar $out/${projectName}
    '';
  };
  fromImage = nixpkgs.dockerTools.pullImage {
    imageName = "eclipse-temurin";
    imageDigest = "sha256:4cc7dfdfb7837f35c3820bcfbc5f666521364e2198960322848ab7d3e2ca3e88";
    finalImageName = "eclipse-temurin";
    finalImageTag = "17.0.4.1_1-jre";
    sha256 = "sha256-OIib6PQJc1xX+Xu2xtaFEo/jZtxypkg8Y9RFMcJf39w=";
  };
in
  nixpkgs.dockerTools.buildLayeredImage {
    inherit fromImage;
    name = projectName;
    enableFakechroot = true;
    fakeRootCommands = ''
      cp ${builtJar}/${projectName} /${projectName}
      chown root:root /${projectName}
      chmod 774 /${projectName}
    '';
    config = {
      Entrypoint = [
        "/bin/sh"
        "-c"
        "java $JAVA_OPTS -jar /${projectName}"
      ];
    };
  }
|]
              )
      Nothing -> fail "Gave nothing for a project which should've had a build.nix generated."
  it "renders correctly for a Rust project" do
    case buildNixFor projectName Rust of
      Just buildNix ->
        bootstrapContent buildNix
          >>= ( `shouldBe`
                  Right
                    [r|{nixpkgs}: let
  src = ../.;
  cargoToml = builtins.fromTOML (builtins.readFile (src + "/Cargo.toml"));
in
  nixpkgs.rustPlatform.buildRustPackage {
    inherit src;
    inherit (cargoToml.package) name version;
    cargoLock.lockFile = src + "/Cargo.lock";
  }
|]
              )
      Nothing -> fail "Gave nothing for a project which should've had a build.nix generated."
  it "renders correctly for a Haskell project" do
    case buildNixFor
      projectName
      (Haskell $ HaskellOptions (GHCVersion 9 4 8) (HaskellProjectTypeBasic $ SetUpHaskellBuild True)) of
      Just buildNix ->
        bootstrapContent buildNix
          >>= ( `shouldBe`
                  Right
                    [r|{
  haskellPackages,
  nixpkgs,
}: let
  # This is the core build of your program, but its closure includes all build inputs
  unstripped = haskellPackages.callCabal2nix "test-project" ../. {};
in
  nixpkgs.stdenv.mkDerivation {
    # This derivation strips out the build inputs from `unstripped` above to leave just your program
    inherit (unstripped) name src version;
    buildInputs = with nixpkgs; [
      # This is an assumed set of system libraries needed; you can add to this as necessary.
      # You can find out what your package needs by reading the output of `ldd` on your built binary.
      glibc
      gmp
      libffi
      # ncurses and zlib are not needed for the bootstrapped program, but are needed by lots of common Haskell libraries
      ncurses
      zlib
    ];
    installPhase = ''
      mkdir -p $out/bin
      cp ${(nixpkgs.haskell.lib.enableSeparateBinOutput unstripped).bin}/bin/app $out/bin/test-project'';
  }
|]
              )
      Nothing -> fail "Gave nothing for a project which should've had a build.nix generated."

projectName :: ProjectName
projectName = Unsafe.fromJust (mkProjectName "test-project")
