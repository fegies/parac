{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_pseudocodeCompiler (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/felix/programming/haskell/pseudocodeCompiler/pseudocodeCompiler/.cabal-sandbox/bin"
libdir     = "/home/felix/programming/haskell/pseudocodeCompiler/pseudocodeCompiler/.cabal-sandbox/lib/x86_64-linux-ghc-8.0.1/pseudocodeCompiler-0.1.0.0"
datadir    = "/home/felix/programming/haskell/pseudocodeCompiler/pseudocodeCompiler/.cabal-sandbox/share/x86_64-linux-ghc-8.0.1/pseudocodeCompiler-0.1.0.0"
libexecdir = "/home/felix/programming/haskell/pseudocodeCompiler/pseudocodeCompiler/.cabal-sandbox/libexec"
sysconfdir = "/home/felix/programming/haskell/pseudocodeCompiler/pseudocodeCompiler/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "pseudocodeCompiler_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "pseudocodeCompiler_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "pseudocodeCompiler_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "pseudocodeCompiler_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "pseudocodeCompiler_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
