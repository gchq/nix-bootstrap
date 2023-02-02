-- | Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.Version (MajorVersion (MajorVersion), displayMajorVersion, parseMajorVersion, toMajorVersion) where

import qualified Data.Version as V
import Text.ParserCombinators.ReadP (readP_to_S)

data MajorVersion = MajorVersion
  { flagshipVersion :: Int,
    majorVersion :: Int
  }
  deriving stock (Eq, Show)

instance Ord MajorVersion where
  a <= b =
    flagshipVersion a < flagshipVersion b
      || (flagshipVersion a == flagshipVersion b && majorVersion a <= majorVersion b)

displayMajorVersion :: (IsString s, Semigroup s) => MajorVersion -> s
displayMajorVersion MajorVersion {..} = show flagshipVersion <> "." <> show majorVersion

parseMajorVersion :: String -> Maybe MajorVersion
parseMajorVersion = toMajorVersion <=< (fmap fst . viaNonEmpty last . readP_to_S V.parseVersion)

toMajorVersion :: V.Version -> Maybe MajorVersion
toMajorVersion version = do
  case V.versionBranch version of
    (flagship : major : _) -> Just $ MajorVersion flagship major
    _ -> Nothing
