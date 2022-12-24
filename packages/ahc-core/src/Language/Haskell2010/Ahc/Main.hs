-- vim: set filetype=haskell noet

{-
 - ‘ahc-minimal’: the CLI front-end.
 -}

{-# LANGUAGE Haskell2010 #-}

-- | Select the CLI front-end.
module Main where

import Prelude

import qualified Language.Haskell2010.Ahc.Frontends.CLI (main)

-- | Plug into the CLI front-end entry point.
main :: IO ()
main = Language.Haskell2010.Ahc.Frontends.CLI.main
