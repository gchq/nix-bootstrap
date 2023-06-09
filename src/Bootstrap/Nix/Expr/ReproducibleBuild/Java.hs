{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Nix.Expr.ReproducibleBuild.Java (reproducibleJavaBuild) where

import Bootstrap.Data.ProjectName (ProjectName (unProjectName))
import Bootstrap.Data.ProjectType (ArtefactId (unArtefactId))
import Bootstrap.Nix.Expr
  ( Expr (ELetIn, ELit),
    Literal (LString),
    nix,
    nixbinding,
    nixproperty,
    (|=),
  )

reproducibleJavaBuild :: ProjectName -> ArtefactId -> Expr
reproducibleJavaBuild projectName artefactId =
  ELetIn
    ( ([nixproperty|projectName|] |= ELit (LString $ unProjectName projectName))
        :| [ [nixproperty|artefactId|] |= ELit (LString $ unArtefactId artefactId),
             [nixbinding|
        repository = nixpkgs.stdenv.mkDerivation {
            name = "${projectName}-repository";
            buildInputs = [nixpkgs.maven];
            src = ./.;
            buildPhase = "mvn package -Dmaven.repo.local=$out";
            # keep only *.{pom,jar,sha1,nbm} and delete all ephemeral files with lastModified timestamps inside
            installPhase = ''
            find $out -type f \\
                -name \\*.lastUpdated -or \\
                -name resolver-status.properties -or \\
                -name _remote.repositories \\
                -delete
            '';
            dontFixup = true;
            outputHashAlgo = "sha256";
            outputHashMode = "recursive";
            # This hash will need updating when changing your dependencies; the correct
            # hash will be displayed when the build fails if so.
            outputHash = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";
        };|],
             [nixbinding|
        builtJar = nixpkgs.stdenv.mkDerivation rec {
            pname = projectName;
            version = "0.0.1-SNAPSHOT";
            src = ./.;
            buildInputs = [nixpkgs.maven];
            buildPhase = ''
            echo "Using repository ${repository}"
            mvn --offline -Dmaven.repo.local=${repository} package;
            '';
            installPhase = ''
            install -Dm644 target/${artefactId}-${version}.jar $out/${projectName}
            '';
        };|],
             [nixbinding|
        fromImage = nixpkgs.dockerTools.pullImage {
            imageName = "eclipse-temurin";
            imageDigest = "sha256:4cc7dfdfb7837f35c3820bcfbc5f666521364e2198960322848ab7d3e2ca3e88";
            finalImageName = "eclipse-temurin";
            finalImageTag = "17.0.4.1_1-jre";
            sha256 = "sha256-OIib6PQJc1xX+Xu2xtaFEo/jZtxypkg8Y9RFMcJf39w=";
        };|]
           ]
    )
    [nix|
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
            Entrypoint = ["/bin/sh" "-c" "java $JAVA_OPTS -jar /${projectName}"];
        };
    }|]
