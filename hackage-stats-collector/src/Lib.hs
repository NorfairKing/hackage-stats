{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( someFunc,
  )
where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZip
import Conduit
import qualified Data.ByteString.Lazy as LB
import qualified Data.Conduit.Combinators as C
import Data.List
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Distribution.PackageDescription
import Distribution.PackageDescription.Parsec as Cabal
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
            .| C.map (`M.singleton` (1 :: Word))
            .| C.foldl (M.unionWith (+)) M.empty
      mapM_ print $ sortOn snd $ M.toList r
