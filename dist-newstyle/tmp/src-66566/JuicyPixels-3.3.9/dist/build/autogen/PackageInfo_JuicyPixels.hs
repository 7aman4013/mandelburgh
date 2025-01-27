{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_JuicyPixels (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "JuicyPixels"
version :: Version
version = Version [3,3,9] []

synopsis :: String
synopsis = "Picture loading/serialization (in png, jpeg, bitmap, gif, tga, tiff and radiance)"
copyright :: String
copyright = ""
homepage :: String
homepage = "https://github.com/Twinside/Juicy.Pixels"
