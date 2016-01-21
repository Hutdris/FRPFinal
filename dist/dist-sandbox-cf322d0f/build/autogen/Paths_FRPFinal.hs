module Paths_FRPFinal (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\SaltShineZ\\final\\FRPFinal\\.cabal-sandbox\\bin"
libdir     = "C:\\Users\\SaltShineZ\\final\\FRPFinal\\.cabal-sandbox\\x86_64-windows-ghc-7.10.3\\FRPFinal-0.1.0.0-CikWvFCC6LQ3NJ01Jmt0lh"
datadir    = "C:\\Users\\SaltShineZ\\final\\FRPFinal\\.cabal-sandbox\\x86_64-windows-ghc-7.10.3\\FRPFinal-0.1.0.0"
libexecdir = "C:\\Users\\SaltShineZ\\final\\FRPFinal\\.cabal-sandbox\\FRPFinal-0.1.0.0-CikWvFCC6LQ3NJ01Jmt0lh"
sysconfdir = "C:\\Users\\SaltShineZ\\final\\FRPFinal\\.cabal-sandbox\\etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "FRPFinal_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "FRPFinal_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "FRPFinal_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "FRPFinal_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "FRPFinal_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
