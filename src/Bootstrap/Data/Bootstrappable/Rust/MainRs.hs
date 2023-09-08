{-# LANGUAGE QuasiQuotes #-}

-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.Bootstrappable.Rust.MainRs
  ( MainRs,
    mainRsFor,
  )
where

import Bootstrap.Data.Bootstrappable
  ( Bootstrappable (bootstrapContent, bootstrapName, bootstrapReason),
  )
import Bootstrap.Data.ProjectType
  ( ProjectType (Rust),
  )
import Text.RawString.QQ (r)

data MainRs = MainRs

instance Bootstrappable MainRs where
  bootstrapName = const "src/main.rs"
  bootstrapReason = const "Your rust application's entrypoint"
  bootstrapContent MainRs =
    pure $
      Right
        [r|fn main() {
    println!("Hello, world!");
}
|]

mainRsFor :: ProjectType -> Maybe MainRs
mainRsFor = \case
  Rust -> Just MainRs
  _ -> Nothing
