-- vim: set filetype=haskell noet

{-
 - DefaultLinking.hs
 -
 - Haskell2010 Simple syntax AST: default intra-module linking.
 -
 - Instantiates AST structures with intra-module references, providing a
 - default implementation.
 -}

{-# LANGUAGE Haskell2010 #-}

-- | Haskell2010 Simple syntax AST: default intra-module and inter-module
-- linking.
--
-- This module provides default intra-module AST instantiations and exports
-- these with type aliases.
--
-- This module additionally chooses Prelude attributes such as ‘Maybe’ to
-- provide for common Prelude variables.
--
-- This module, however, still leaves the lexical foundation unspecified.  This
-- will need to be provided elsewhere.  (Perhaps another module can be
-- provided similar to this one, that provides a default lexical foundation.)
--
-- The chosen pattern here chooses the following variables needed to complete
-- building up the module instantiations:
-- - lexicalAnnotation: all section 2 structures.
-- - grammarAnnotation: all section 3-5 structures.
--
-- Finally, this module uses 'Language.Haskell2010.Ahc.Data.Fixed.Fixed' to
-- wrap each structure to provide F-algebra style recursion.  (This is not
-- directly provided by the Haskell2010 specification, which would have been a
-- convenient Schelling point.)
module Language.Haskell2010.Ahc.Syntax.Haskell2010.Simple.AST.DefaultLinking (
	-- * Structures with default linking.

	-- ** Intra-module and inter-module linking.

	-- *** Lexical Foundation structures.

	-- TODO
) where

-- (Note: the import ordering style I use here is that since Prelude handling
-- has more built-in relevance (implicit preludes), implicit prelude related
-- imports go first, and then intra-package imports, then Haskell2010 or base
-- imports, and then imports external to the package.)

import Prelude ()

import Language.Haskell2010.Ahc.Syntax.Haskell2010.Simple.AST.Base

import qualified Prelude

{-
 - ----------------------------------------------------------------
 - Structures with default linking.
 - ----------------------------------------------------------------
 -}

--type ModuleF blah blah blah = ModuleBase Prelude.Maybe () () () () () () () ()
--data ModuleBase maybe lexicalModule modid exports lexicalWhere body annotation fixpoint =

-- TODO
