-- vim: set filetype=haskell noet

{-
 - CLI.hs
 -
 - A front-end to ‘ahc-minimal’.
 -
 - TODO: finish.
 -}

{-# LANGUAGE Haskell2010 #-}

-- | Module providing a strictly Haskell2010-conformant AST data structure.
--
-- Does not include extensions, such as permitting trailing semicolons for more
-- convenient automatic text manipulation.
module Language.Haskell2010.Ahc.Syntax.Haskell2010.Simple.AST (
	-- * Base structures.
	ModuleBase(..),
	BodyBase(..),
	ImplDecls(..),
	TopDecls(..),

	-- * Structures with default linking.
) where

import Prelude ()

{-
 - ----------------------------------------------------------------
 - Base structures.
 - ----------------------------------------------------------------
 -}

-- TODO: representing cons order?

-- | A module.
data ModuleBase maybe lexemeModule modid exports lexemeWhere body annotation fixpoint =
	  ModuleWithHeader    annotation lexemeModule modid (maybe exports) lexemeWhere body
	| ModuleWithoutHeader annotation body

-- | A module's body.
data BodyBase lexemeLeftBrace implDecls lexemeSemicolon topDecls lexemeRightBrace annotation fixpoint =
	  BodyImportsTops annotation lexemeLeftBrace implDecls lexemeSemicolon  topDecls lexemeRightBrace
	| BodyImportsOnly annotation lexemeLeftBrace implDecls lexemeRightBrace
	| BodyTopsOnly    annotation lexemeLeftBrace topDecls  lexemeRightBrace

-- | Import block.
data ImplDecls implDecl lexemeSemicolon annotation fixpoint =
	  ImplDeclsNotLast annotation implDecl lexemeSemicolon fixpoint
	| ImplDeclsLast    annotation implDecl

-- | Top-level declarations: what a module defines.
data TopDecls topDecl lexemeSemicolon annotation fixpoint =
	  TopDeclsNotLast annotation topDecl lexemeSemicolon fixpoint
	| TopDeclsLast    annotation topDecl

{-
 - ----------------------------------------------------------------
 - Structures with default linking.
 - ----------------------------------------------------------------
 -}
