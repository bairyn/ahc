-- vim: set filetype=haskell noet

{-
 - DefaultLinking.hs
 -
 - A Haskell2010 syntax.
 -
 - An alias of 'StandardLinking'.
 -}

{-# LANGUAGE Haskell2010 #-}

-- | A Simple Haskell2010 syntax.
--
-- This module is an alias for a default linking module, 'StandardLinking'.
module Language.Haskell2010.Ahc.Syntax.Haskell2010.Simple.AST.DefaultLinking (
	module Language.Haskell2010.Ahc.Syntax.Haskell2010.Simple.AST.StandardLinking,
) where

import Prelude ()

import Language.Haskell2010.Ahc.Syntax.Haskell2010.Simple.AST.StandardLinking
