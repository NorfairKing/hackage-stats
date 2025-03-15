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
import Control.Monad
import qualified Data.ByteString.Lazy as LB
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.ConcurrentMap as C
import Data.Csv as Csv
import Data.Csv.Conduit
import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Distribution.PackageDescription
import Distribution.Pretty (prettyShow)
import Distribution.Simple.PackageDescription as Cabal
import Distribution.Utils.ShortText as Cabal (fromShortText)
import Distribution.Verbosity as Cabal
import GHC.Generics (Generic)
import Path
import Path.IO
import System.Environment
import System.Exit
import System.IO
import Text.Printf
import Text.Regex.TDFA

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

      runResourceT $
        runConduit $
          yield extractedDir
            .| C.mapM (fmap fst . listDir)
            .| progressConduit
            .| C.concatMapM (fmap fst . listDir)
            .| C.concatMapM (fmap snd . listDir)
            -- .| C.concatMapM (fmap (take 1) . fmap snd . listDir) -- Only the first cabal file
            .| C.filter ((".cabal" `isSuffixOf`) . fromAbsFile)
            .| C.concurrentMapM_numCaps 64 (liftIO . readPackageInfo)
            .| toCsv Csv.defaultEncodeOptions
            .| C.stdout

progressConduit :: MonadIO m => ConduitT [o] o m ()
progressConduit = awaitForever $ \vals -> do
  let total = length vals
  forM_ (zip [1 ..] vals) $ \(num, val) -> do
    yield val
    liftIO $ hPutStrLn stderr $ printf "%d/%d" (num :: Int) total

data PackageInfo = PackageInfo
  { packageInfoName :: String,
    packageInfoVersion :: String,
    packageInfoMaintainer :: Text,
    packageInfoYear :: Integer
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
  let maintainerText = T.toCaseFold $ T.strip $ T.pack (fromShortText (maintainer pd))
  let packageInfoMaintainer = maybe maintainerText stripAfterPlusBeforeAt $ maintainerText =~~ emailRegex
  packageInfoTime <- getModificationTime cabalFile
  let (packageInfoYear, _, _) = toGregorian $ utctDay packageInfoTime
  pure PackageInfo {..}

emailRegex :: String
emailRegex = concat [localPart, "@", domainPart, "\\.", tldRegex]
  where
    localPart = "[a-zA-Z0-9_.+-]+"
    domainPart = "[-a-zA-Z0-9@:%._\\+~#=]{1,64}"
    tldRegex = "[a-z]{1,63}"

stripAfterPlusBeforeAt :: Text -> Text
stripAfterPlusBeforeAt t = case T.splitOn "@" t of
  [b, a] -> T.takeWhile (/= '+') b <> "@" <> a
  _ -> t
