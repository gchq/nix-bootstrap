{-# LANGUAGE QuasiQuotes #-}

-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.Bootstrappable.Elm.Review.ConfigSpec (spec) where

import Bootstrap.Data.Bootstrappable (Bootstrappable (bootstrapContent))
import Bootstrap.Data.Bootstrappable.Elm.Review.Config (elmReviewConfigFor)
import Bootstrap.Data.ProjectType
  ( ElmMode (ElmModeBare),
    ElmOptions (ElmOptions),
    ProjectType (Elm),
  )
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)
import Text.RawString.QQ (r)

spec :: Spec
spec = describe "ReviewConfig.elm rendering" do
  it "renders the ReviewConfig.elm correctly" do
    bootstrapContent (elmReviewConfigFor . Elm $ ElmOptions ElmModeBare True)
      >>= ( `shouldBe`
              Right
                [r|module ReviewConfig exposing (config)

import NoBooleanCase
import NoDuplicatePorts
import NoExposingEverything
import NoImportingEverything
import NoMissingSubscriptionsCall
import NoMissingTypeAnnotation
import NoMissingTypeConstructor
import NoMissingTypeExpose
import NoPrematureLetComputation
import NoRecursiveUpdate
import NoSimpleLetBody
import NoUnused.CustomTypeConstructorArgs
import NoUnused.CustomTypeConstructors
import NoUnused.Dependencies
import NoUnused.Exports
import NoUnused.Modules
import NoUnused.Parameters
import NoUnused.Patterns
import NoUnused.Variables
import NoUnusedPorts
import NoUselessSubscriptions
import Review.Rule exposing (Rule)
import Simplify


config : List Rule
config =
    [ NoBooleanCase.rule
    , NoDuplicatePorts.rule
    , NoExposingEverything.rule
    , NoImportingEverything.rule []
    , NoMissingSubscriptionsCall.rule
    , NoMissingTypeAnnotation.rule
    , NoMissingTypeConstructor.rule
    , NoMissingTypeExpose.rule
    , NoPrematureLetComputation.rule
    , NoRecursiveUpdate.rule
    , NoSimpleLetBody.rule
    , NoUnusedPorts.rule
    , NoUnused.CustomTypeConstructors.rule []
    , NoUnused.CustomTypeConstructorArgs.rule
    , NoUnused.Dependencies.rule
    , NoUnused.Exports.rule
    , NoUnused.Modules.rule
    , NoUnused.Parameters.rule
    , NoUnused.Patterns.rule
    , NoUnused.Variables.rule
    , NoUselessSubscriptions.rule
    , Simplify.rule Simplify.defaults
    ]
|]
          )
