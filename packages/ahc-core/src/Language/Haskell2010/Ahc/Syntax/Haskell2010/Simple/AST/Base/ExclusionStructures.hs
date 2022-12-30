-- vim: set filetype=haskell noet

{-
 - ExclusionStructures.hs
 -
 - Haskell2010 Simple syntax AST: base data structures, organized just for
 - exclusion structures.
 -
 - Instantiates AST structures with intra-module references, providing a
 - default implementation.
 -}

{-# LANGUAGE Haskell2010 #-}

-- | Haskell2010 Simple syntax AST: exclusion data structures.
--
-- The AST structures are organized to split the exclusion structures off.
-- This module contains them.  This enables syntax-level keyword exclusions.
--
-- The default Haskell2010 syntax uses these, although it is possible to build
-- an AST based on the Simple syntax except that uses simpler structures
-- without exclusions, and handles exclusions at a higher level, e.g. as a
-- detail in semantics, not syntax.
module Language.Haskell2010.Ahc.Syntax.Haskell2010.Simple.AST.Base.ExclusionStructures (
	-- TODO
) where

-- TODO
