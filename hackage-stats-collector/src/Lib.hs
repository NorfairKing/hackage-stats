{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( someFunc,
  )
where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZip
import Conduit
import Control.Monad
import qualified Data.ByteString as SB
import qualified Data.ByteString.Char8 as SB8
import qualified Data.ByteString.Lazy as LB
import qualified Data.Conduit.Combinators as C
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Data.Ord
import qualified Data.Text as T
import Distribution.Compiler as Cabal
import Distribution.License as Cabal
import Distribution.ModuleName as Cabal
import Distribution.PackageDescription
import Distribution.PackageDescription.Parsec as Cabal
import Distribution.Types.CondTree as Cabal
import Distribution.Types.Dependency as Cabal
import Distribution.Types.ExeDependency as Cabal
import Distribution.Types.ExecutableScope as Cabal
import Distribution.Types.ForeignLib as Cabal
import Distribution.Types.ForeignLibOption as Cabal
import Distribution.Types.ForeignLibType as Cabal
import Distribution.Types.IncludeRenaming as Cabal
import Distribution.Types.LegacyExeDependency as Cabal
import Distribution.Types.LibraryVisibility as Cabal
import Distribution.Types.Mixin as Cabal
import Distribution.Types.PackageId as Cabal
import Distribution.Types.PackageName as Cabal
import Distribution.Types.PkgconfigDependency as Cabal
import Distribution.Types.PkgconfigName
import Distribution.Types.PkgconfigVersion
import Distribution.Types.PkgconfigVersionRange
import Distribution.Types.UnqualComponentName as Cabal
import Distribution.Types.Version as Cabal
import Distribution.Types.VersionRange as Cabal
import Distribution.Utils.ShortText as Cabal
import Distribution.Verbosity as Cabal
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

      r <-
        runConduit $
          yield extractedDir
            .| C.concatMapM (fmap fst . listDir)
            .| C.concatMapM (fmap fst . listDir) -- Only the first cabal file
            .| C.concatMapM (fmap snd . listDir)
            .| C.filter ((".cabal" `isSuffixOf`) . fromAbsFile)
            .| C.mapM (Cabal.readGenericPackageDescription Cabal.silent . fromAbsFile)
            .| C.map packageDescription
            .| C.map maintainer
            .| C.concatMap (map T.unpack . T.splitOn ", " . T.pack . fromShortText)
            .| C.map (`M.singleton` 1)
            .| C.foldl (M.unionWith (+)) M.empty
      mapM_ print $ sortOn snd $ M.toList r
