{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_thoth_contract (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
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
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/amati/.cabal/bin"
libdir     = "/home/amati/.cabal/lib/x86_64-linux-ghc-8.10.4.20210212/thoth-contract-0.1.0.0-inplace-thoth-contract-test"
dynlibdir  = "/home/amati/.cabal/lib/x86_64-linux-ghc-8.10.4.20210212"
datadir    = "/home/amati/.cabal/share/x86_64-linux-ghc-8.10.4.20210212/thoth-contract-0.1.0.0"
libexecdir = "/home/amati/.cabal/libexec/x86_64-linux-ghc-8.10.4.20210212/thoth-contract-0.1.0.0"
sysconfdir = "/home/amati/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "thoth_contract_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "thoth_contract_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "thoth_contract_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "thoth_contract_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "thoth_contract_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "thoth_contract_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
