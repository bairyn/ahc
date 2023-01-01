-- vim: set filetype=haskell noet

{-
 - AST.hs
 -
 - A Haskell2010 syntax.
 -
 - (Note: lenses would likely be useful to interact with ASTs in various ways,
 - but such lenses would belong outside this library, or else this library
 - alone would have more dependencies and would be less conveniently
 - bootstrapped.  These structures belong in the ‘ahc-core’ library, which
 - depends on packages only that provide a Haskell2010 implementation.)
 -
 - (For more information about the fixpoint argument, look for resources on
 - catamorphisms and F-algebras.)
 -
 - (This module lacks automatic derivations, for greater and more explicit
 - control.)
 -
 - (At least partly for more explicit linking, even local enough to be within the same module,
 - the base data structures refer to other attributes in the module and outside
 - only through type variables, and thees variables can be filled in later,
 - even within the same module.  These structures have no Upper Case
 - references in the constructor fields.)
 -
 - (Since normally base case branches are styled before recursive step
 - branches, a ‘maximum lunch’ parser probably should normally try branches at
 - least in reverse group order.)
 -
 - (Long lines here are a compromise for some consistencies.)
 -}

{-# LANGUAGE Haskell2010 #-}

-- | A Simple Haskell2010 syntax.
--
-- This module provides a strictly Haskell2010-conformant AST data structure.
--
-- Variations may be introduced to their own Haskell2010 variant AST data
-- structures.
--
-- This does not include extensions, such as permitting more trailing
-- semicolons for more convenient automatic text manipulation.
module Language.Haskell2010.Ahc.Syntax.Haskell2010.Simple.AST (
	module Language.Haskell2010.Ahc.Syntax.Haskell2010.Simple.AST.Base,
	module Language.Haskell2010.Ahc.Syntax.Haskell2010.Simple.AST.StandardLinking,
) where

import Prelude ()

import Language.Haskell2010.Ahc.Syntax.Haskell2010.Simple.AST.Base
import Language.Haskell2010.Ahc.Syntax.Haskell2010.Simple.AST.StandardLinking
