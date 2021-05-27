{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_hs_ci_server (
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
version = Version [0,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/joona/dev/hs-ci-server/.stack-work/install/x86_64-osx/43fd75d537a6ff39d35be24cfa55011b71c594b922c252cf08471252d15cba04/8.10.4/bin"
libdir     = "/Users/joona/dev/hs-ci-server/.stack-work/install/x86_64-osx/43fd75d537a6ff39d35be24cfa55011b71c594b922c252cf08471252d15cba04/8.10.4/lib/x86_64-osx-ghc-8.10.4/hs-ci-server-0.0.0-AI9giJ1oYjm60UVHaC7ulU-hs-ci-server"
dynlibdir  = "/Users/joona/dev/hs-ci-server/.stack-work/install/x86_64-osx/43fd75d537a6ff39d35be24cfa55011b71c594b922c252cf08471252d15cba04/8.10.4/lib/x86_64-osx-ghc-8.10.4"
datadir    = "/Users/joona/dev/hs-ci-server/.stack-work/install/x86_64-osx/43fd75d537a6ff39d35be24cfa55011b71c594b922c252cf08471252d15cba04/8.10.4/share/x86_64-osx-ghc-8.10.4/hs-ci-server-0.0.0"
libexecdir = "/Users/joona/dev/hs-ci-server/.stack-work/install/x86_64-osx/43fd75d537a6ff39d35be24cfa55011b71c594b922c252cf08471252d15cba04/8.10.4/libexec/x86_64-osx-ghc-8.10.4/hs-ci-server-0.0.0"
sysconfdir = "/Users/joona/dev/hs-ci-server/.stack-work/install/x86_64-osx/43fd75d537a6ff39d35be24cfa55011b71c594b922c252cf08471252d15cba04/8.10.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "hs_ci_server_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hs_ci_server_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "hs_ci_server_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "hs_ci_server_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hs_ci_server_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hs_ci_server_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
