{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_JuicyPixels (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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
version = Version [3,3,9] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/33000221/.cabal/store/ghc-9.4.8/JcyPxls-3.3.9-4c4ca014/bin"
libdir     = "/Users/33000221/.cabal/store/ghc-9.4.8/JcyPxls-3.3.9-4c4ca014/lib"
dynlibdir  = "/Users/33000221/.cabal/store/ghc-9.4.8/lib"
datadir    = "/Users/33000221/.cabal/store/ghc-9.4.8/JcyPxls-3.3.9-4c4ca014/share"
libexecdir = "/Users/33000221/.cabal/store/ghc-9.4.8/JcyPxls-3.3.9-4c4ca014/libexec"
sysconfdir = "/Users/33000221/.cabal/store/ghc-9.4.8/JcyPxls-3.3.9-4c4ca014/etc"

getBinDir     = catchIO (getEnv "JuicyPixels_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "JuicyPixels_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "JuicyPixels_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "JuicyPixels_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "JuicyPixels_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "JuicyPixels_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
