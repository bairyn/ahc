-- vim: set filetype=haskell noet

{-
 - CLI.hs
 -
 - A front-end to ‘ahc-minimal’.
 -
 - TODO: finish.
 -}

{-# LANGUAGE Haskell2010 #-}

-- | A Simple Haskell2010 syntax.
--
-- This module provides a strictly Haskell2010-conformant AST data structure.
--
-- Does not include extensions, such as permitting trailing semicolons for more
-- convenient automatic text manipulation.
module Language.Haskell2010.Ahc.Syntax.Haskell2010.Simple.AST (
	-- Export everything, explicitly.

	-- * Base structures.

	-- ** § 5.1 Module Structure types.
	ModuleBase(ModuleWithHeader, ModuleWithoutHeader),
	BodyBase(BodyImportsTops, BodyImportsOnly, BodyTopsOnly),
	ImpDeclsBase(ImpDeclsNotLast, ImpDeclsLast),
	TopDeclsBase(TopDeclsNotLast, TopDeclsLast),

	-- ** § 5.2 Export Lists types.
	ExportsBase(ExportsFromFirst),
	ExportsRestBase(ExportsNotLast, ExportsLast),
	ExportBase(ExportVariable, ExportTypeVariable, ExportClassVariable, ExportModule),
	ExportCnamesBase(ExportCnamesEmpty, ExportCnamesPush),
	ExportVarsBase(ExportVarsEmpty, ExportVarsPush),
	CnameBase(CnameVar, CnameCon),

	-- ** § 5.3 Import Declarations types.
	ImpDeclBase(ImportStatement, NullImport),
	ImpSpecBase(ImportWhitelist, ImportBlacklist),
	ImportAttrsBase(ImportAttrsEmpty, ImportAttrsPush),
	ImportBase(ImportVariableAttribute,ImportTypeVariableAttribute,ImportTypeClassVariableAttribute),
	ImportCnamesBase(ImportCnamesEmpty, ImportCnamesPush),
	ImportVarsBase(ImportVarsEmpty, ImportVarsPush),

	-- ** § 4 Declarations and Bindings types.
	TopDeclBase(TopDeclType, TopDeclData, TopDeclNewtype, TopDeclClass, TopDeclInstance, TopDeclDefault, TopDeclForeign, TopDeclRegular),
	TopDeclDefaultTypesBase(TopDeclDefaultTypesEmpty, TopDeclDefaultTypesPush),

	-- ** § 4.1 Module Structure types.

	-- * Structures with default linking.
) where

import Prelude ()

-- (General data pattern (within groups, order of appearance):
-- data *datas *typeCons *types annotation fixpoint = …

{-
 - ----------------------------------------------------------------
 - Base structures.
 - ----------------------------------------------------------------
 -}

-- § 5.1 Module Structure types.

-- | A module.
--
-- (modid: module identifier.)
data ModuleBase maybe lexemeModule modid exports lexemeWhere body annotation fixpoint =
	  ModuleWithHeader    annotation lexemeModule modid (maybe exports) lexemeWhere body
	| ModuleWithoutHeader annotation body

-- | A module's body.
data BodyBase lexemeLeftBrace impDecls lexemeSemicolon topDecls lexemeRightBrace annotation fixpoint =
	  BodyImportsTops annotation lexemeLeftBrace impDecls lexemeSemicolon  topDecls lexemeRightBrace
	| BodyImportsOnly annotation lexemeLeftBrace impDecls lexemeRightBrace
	| BodyTopsOnly    annotation lexemeLeftBrace topDecls lexemeRightBrace

-- | Import block.
data ImpDeclsBase impDecl lexemeSemicolon annotation fixpoint =
	  ImpDeclsNotLast annotation impDecl lexemeSemicolon fixpoint
	| ImpDeclsLast    annotation impDecl

-- | Top-level declarations: what a module defines.
data TopDeclsBase topDecl lexemeSemicolon annotation fixpoint =
	  TopDeclsNotLast annotation topDecl lexemeSemicolon fixpoint
	| TopDeclsLast    annotation topDecl

-- § 5.2 Export Lists types.

-- | A module's export list.
-- TODO: parens
data ExportsBase lexemeLeftParenthesis exportsRest annotation fixpoint =
	  ExportsFromFirst annotation lexemeLeftParenthesis exportsRest

-- | The rest of a module's export list.
data ExportsRestBase maybe export lexemeSemicolon lexemeComma lexemeRightParenthesis annotation fixpoint =
	  ExportsNotLast annotation export lexemeSemicolon fixpoint
	| ExportsLast    annotation export (maybe lexemeComma) lexemeRightParenthesis

-- | An export, in a module's export list.
--
-- (modid: module identifier.)
data ExportBase data3 maybe either qvar qtycon lexemeLeftParenthesis lexemeDotDot lexemeRightParenthesis exportCnames qtycls exportVars lexemeModule modid annotation fixpoint =
	  ExportVariable      annotation qvar
	| ExportTypeVariable  annotation qtycon (maybe (either (data3 lexemeLeftParenthesis lexemeDotDot lexemeRightParenthesis) (data3 lexemeLeftParenthesis exportCnames lexemeRightParenthesis)))
	| ExportClassVariable annotation qtycls (maybe (either (data3 lexemeLeftParenthesis lexemeDotDot lexemeRightParenthesis) (data3 lexemeLeftParenthesis exportVars lexemeRightParenthesis)))
	| ExportModule        annotation lexemeModule modid

-- | An ADT (‘data’) or ‘newtype’ export item.
--
-- Export a data type or newtype type and its fields and constructors.
data ExportCnamesBase cname lexemeComma annotation fixpoint =
	  ExportCnamesEmpty annotation
	| ExportCnamesPush  annotation cname lexemeComma fixpoint

-- | Export a type class's members.
data ExportVarsBase var lexemeComma annotation fixpoint =
	  ExportVarsEmpty annotation
	| ExportVarsPush  annotation var lexemeComma fixpoint

-- | Export a ‘data’ or ‘newtype’'s fields (‘var’s) and constructors (‘con’s).
data CnameBase var con annotation fixpoint =
	  CnameVar annotation var
	| CnameCon annotation con

-- § 5.3 Import Declarations types.

-- | Import statement.
data ImpDeclBase data2 maybe lexemeImport lexemeQualified modid lexemeAs impSpec annotation fixpoint =
	  ImportStatement annotation lexemeImport (maybe lexemeQualified) modid (maybe (data2 lexemeAs modid)) (maybe impSpec)
	| NullImport      annotation

-- | Import specification: what to import.
data ImpSpecBase lexemeLeftParenthesis importAttrs lexemeRightParenthesis lexemeHiding annotation fixpoint =
	  ImportWhitelist annotation              lexemeLeftParenthesis importAttrs lexemeRightParenthesis
		-- ^ Import only ….
	| ImportBlacklist annotation lexemeHiding lexemeLeftParenthesis importAttrs lexemeRightParenthesis
		-- ^ Import all but ….

-- | List of what elements (attributes) to import from a module.
data ImportAttrsBase maybe lexemeComma import_ annotation fixpoint =
	  ImportAttrsEmpty annotation (maybe lexemeComma)
	| ImportAttrsPush  annotation import_ lexemeComma fixpoint

-- | An individual attribute (e.g. variable) in a list of elements to import from a module in an import statement.
data ImportBase data3 maybe either var lexemeLeftParenthesis lexemeDotDot lexemeRightParenthesis importCnames importVars annotation fixpoint =
	  ImportVariableAttribute          annotation var
		-- ^ Import a regular variable from a module.
	| ImportTypeVariableAttribute      annotation (maybe (either (lexemeLeftParenthesis lexemeDotDot lexemeRightParenthesis) (lexemeLeftParenthesis importCnames lexemeRightParenthesis)))
		-- ^ Import a ‘data’ type or ‘newtype’ type from a module.
	| ImportTypeClassVariableAttribute annotation (maybe (either (lexemeLeftParenthesis lexemeDotDot lexemeRightParenthesis) (lexemeLeftParenthesis importVars lexemeRightParenthesis)))
		-- ^ Import a type class from a module.

-- | An ADT (‘data’) or ‘newtype’ import item.
--
-- Import a data type or newtype type and its fields and constructors from a module.
data ImportCnamesBase cname lexemeComma annotation fixpoint =
	  ImportCnamesEmpty annotation
	| ImportCnamesPush  annotation cname lexemeComma fixpoint

-- | Import a type class's members.
data ImportVarsBase var lexemeComma annotation fixpoint =
	  ImportVarsEmpty annotation
	| ImportVarsPush  annotation var lexemeComma fixpoint

-- § 4 Declarations and Bindings types.

-- | Top-level declarations.
--
-- These are the components of a module, especially regular declarations
-- ('TopDeclRegular') for attributes of modules that can be exported and
-- imported.
data TopDeclBase data2 maybe lexemeType simpleType type_ context lexemeDoubleRightArrow lexemeEquals constrs deriving_ newConstr scontext tycls tyvar lexemeWhere cdecls qtycls inst idecls lexemeDefault lexemeLeftParenthesis topDeclDefaultTypes lexemeRightParenthesis lexemeForeign fdecl decl annotation fixpoint =
	  TopDeclType     annotation lexemeType                                      simpleType            type_
	| TopDeclData     annotation (maybe (data2 context  lexemeDoubleRightArrow)) simpleType            (maybe (data2 lexemeEquals constrs)) (maybe deriving_)
	| TopDeclNewtype  annotation (maybe (data2 context  lexemeDoubleRightArrow)) simpleType            lexemeEquals                         newConstr                          (maybe deriving_)
	| TopDeclClass    annotation (maybe (data2 scontext lexemeDoubleRightArrow)) tycls                 tyvar                                (maybe (data2 lexemeWhere cdecls))
	| TopDeclInstance annotation (maybe (data2 scontext lexemeDoubleRightArrow)) qtycls                inst                                 (maybe (data2 lexemeWhere idecls))
	| TopDeclDefault  annotation lexemeDefault                                   lexemeLeftParenthesis topDeclDefaultTypes                  lexemeRightParenthesis
	| TopDeclForeign  annotation lexemeForeign                                   fdecl
	| TopDeclRegular  annotation decl

-- | A list of types to specify as ‘default’ in a ‘default’ declaration.
data TopDeclDefaultTypesBase type_ lexemeComma annotation fixpoint =
	  TopDeclDefaultTypesEmpty annotation
	| TopDeclDefaultTypesPush  annotation type_ lexemeComma fixpoint

{-
 - ----------------------------------------------------------------
 - Structures with default linking.
 - ----------------------------------------------------------------
 -}
