{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_ten_mvp (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "ten_mvp"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "Minimal Viable Product for Ten Monad Architecture"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
