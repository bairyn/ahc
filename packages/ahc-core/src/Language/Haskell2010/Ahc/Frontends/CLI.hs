-- vim: set filetype=haskell noet

{-
 - CLI.hs
 -
 - A front-end to ‘ahc-minimal’.
 -}

{-# LANGUAGE Haskell2010 #-}

module Language.Haskell2010.Ahc.Frontends.CLI (
	main,
) where

import Prelude

-- | An entry point that programs can point to as an entry point.
--
-- Note this is meant to be a front-end to the minimal ‘ahc-minimal’ compiler;
-- the CLI to the normal ‘ahc’ compiler may be more suitable for regular
-- purposes.
--
-- Inside ‘ahc-core’, we are more concerned with portability.
main :: IO ()
main = do
	-- TODO
	return ()
