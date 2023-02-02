-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.Bootstrappable.Python.Requirements (Requirements (Requirements)) where

import Bootstrap.Data.Bootstrappable (Bootstrappable (bootstrapContent, bootstrapName, bootstrapReason))

data Requirements = Requirements

instance Bootstrappable Requirements where
  bootstrapName = const "requirements.txt"
  bootstrapReason = const "A file to contain python modules to be installed"
  bootstrapContent = const . pure $ Right "# Add your python dependencies to this file:"
