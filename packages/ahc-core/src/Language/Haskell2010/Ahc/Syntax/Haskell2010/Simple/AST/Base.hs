-- vim: set filetype=haskell noet

{-
 - Base.hs
 -
 - Haskell2010 Simple syntax AST: base data structures.
 -
 - Instantiates AST structures with intra-module references, providing a
 - default implementation.
 -}

{-# LANGUAGE Haskell2010 #-}

-- | Haskell2010 Simple syntax AST: base data structures.
--
-- This module re-exports all its submodules, to forward all definitions
-- within.
module Language.Haskell2010.Ahc.Syntax.Haskell2010.Simple.AST.Base (
	module Language.Haskell2010.Ahc.Syntax.Haskell2010.Simple.AST.Base.RegularStructures,
	module Language.Haskell2010.Ahc.Syntax.Haskell2010.Simple.AST.Base.ExclusionStructures,
	module Language.Haskell2010.Ahc.Syntax.Haskell2010.Simple.AST.Base.LexicalFoundation,
) where

import Prelude ()

import Language.Haskell2010.Ahc.Syntax.Haskell2010.Simple.AST.Base.RegularStructures
import Language.Haskell2010.Ahc.Syntax.Haskell2010.Simple.AST.Base.ExclusionStructures
import Language.Haskell2010.Ahc.Syntax.Haskell2010.Simple.AST.Base.LexicalFoundation
