-- vim: set filetype=haskell noet

{-
 - ‘ahc-minimal-test’: our test suite.
 -
 - (This module is intended to be a minimal pointer to select the test suite
 - runner entry-point.)
 -}

{-# LANGUAGE Haskell2010 #-}

-- | Select the test suite runner.
module Main where

import Prelude

import qualified Language.Haskell2010.Ahc.Tests.Tests (main)

-- | Plug into the test suite runner entry point.
main :: IO ()
main = Language.Haskell2010.Ahc.Tests.Tests.main
