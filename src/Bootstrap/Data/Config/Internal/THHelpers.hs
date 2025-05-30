{-# LANGUAGE TemplateHaskellQuotes #-}

-- | Copyright : (c) Crown Copyright GCHQ
--   Description : Common TH functions for use in ...Config.Internal
module Bootstrap.Data.Config.Internal.THHelpers (isoForName) where

import Control.Lens (iso)
import Language.Haskell.TH
  ( ExpQ,
    Name,
    Quote (newName),
    appE,
    conE,
    conP,
    lamE,
    varE,
    varP,
  )

-- | Creates an iso like
--   iso (\(VPT9 x) -> x) VPT9
--   if VPT9 is the constructor name
isoForName :: Name -> ExpQ
isoForName constructorName = do
  x <- newName "x"
  [|iso|] `appE` lamE [conP constructorName [varP x]] (varE x) `appE` conE constructorName
