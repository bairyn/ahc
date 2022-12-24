-- vim: set filetype=haskell noet

{-
 - ‘ahc-minimal-bench’: benchmark runner.
 -
 - (This module is intended to be a minimal pointer to select the benchmark
 - entry-point.)
 -}

{-# LANGUAGE Haskell2010 #-}

-- | Select the test suite runner.
module Main where

import Prelude

import qualified Language.Haskell2010.Ahc.Benchmark.Benchmark (main)

-- | Plug into the benchmark runner entry point.
main :: IO ()
main = Language.Haskell2010.Ahc.Benchmark.Benchmark.main
