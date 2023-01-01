-- vim: set filetype=haskell noet

{-
 - AliasLinking.hs
 -
 - Haskell2010 Simple syntax AST: alias intra-module linking.
 -
 - A Haskell2010 syntax.  Provides further instantiated type aliases with
 - 'StandardLinking' structures as the fixpoint.
 -}

{-# LANGUAGE Haskell2010 #-}

-- | A Simple Haskell2010 syntax.
--
-- Provide type aliases with 'StandardLinking' structures as the fixpoint.
module Language.Haskell2010.Ahc.Syntax.Haskell2010.Simple.AST.AliasLinking (
	-- TODO
) where

import           Prelude ()

import           Language.Haskell2010.Ahc.Syntax.Haskell2010.Simple.AST.Base
import qualified Language.Haskell2010.Ahc.Syntax.Haskell2010.Simple.AST.StandardLinking as StandardLinking

-- TODO implement this, and then you can update DefaultLinking to alias to this
-- module instead.
