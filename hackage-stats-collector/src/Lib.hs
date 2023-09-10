{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Lib
  ( someFunc,
  )
where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZip
import Conduit
import qualified Data.ByteString.Lazy as LB
import qualified Data.Conduit.Combinators as C
import Data.Csv as Csv
import Data.Csv.Conduit
import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Distribution.PackageDescription
import Distribution.PackageDescription.Parsec as Cabal
import Distribution.Pretty (prettyShow)
import Distribution.Utils.ShortText as Cabal
import Distribution.Verbosity as Cabal
import GHC.Generics (Generic)
import Path
import Path.IO
import System.Environment
import System.Exit

someFunc :: IO ()
someFunc = do
  args <- getArgs
  case args of
    [] -> die "Usage: hackage-stats-collector index.tar.gz"
    (tarFile : _) -> do
      tdir <- resolveDir' "/tmp/extracted"
      -- withSystemTempDir "hackage-stats-collector" $ \tdir -> do
      extractedDir <- resolveDir tdir "extracted"
      exists <- doesDirExist extractedDir
      if exists
        then pure ()
        else Tar.unpack (fromAbsDir extractedDir) . Tar.read . GZip.decompress =<< LB.readFile tarFile

      runConduit $
        yield extractedDir
          .| C.concatMapM (fmap fst . listDir)
          .| C.concatMapM (fmap fst . listDir)
          .| C.concatMapM (fmap snd . listDir)
          -- .| C.concatMapM (fmap (take 1) . fmap snd . listDir) -- Only the first cabal file
          .| C.filter ((".cabal" `isSuffixOf`) . fromAbsFile)
          .| C.mapM readPackageInfo
          .| toCsv Csv.defaultEncodeOptions
          .| C.stdout

data PackageInfo = PackageInfo
  { packageInfoName :: String,
    packageInfoVersion :: String,
    packageInfoMaintainer :: Text,
    packageInfoTime :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance ToRecord PackageInfo

instance ToField UTCTime where
  toField = toField . formatTime defaultTimeLocale "%F %T"

readPackageInfo :: Path Abs File -> IO PackageInfo
readPackageInfo cabalFile = do
  gpd <- Cabal.readGenericPackageDescription Cabal.silent (fromAbsFile cabalFile)
  let pd = packageDescription gpd
  let pid = package pd
  let packageInfoName = unPackageName $ pkgName pid
  let packageInfoVersion = prettyShow $ pkgVersion pid
  let packageInfoMaintainer = T.strip $ T.pack (fromShortText (maintainer pd))
  packageInfoTime <- getModificationTime cabalFile
  pure PackageInfo {..}
