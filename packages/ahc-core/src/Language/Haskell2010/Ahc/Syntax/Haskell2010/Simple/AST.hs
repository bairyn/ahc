-- vim: set filetype=haskell noet

{-
 - CLI.hs
 -
 - A front-end to ‘ahc-minimal’.
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
	ImpDeclsBase(ImpDeclsLast, ImpDeclsNotLast),
	TopDeclsBase(TopDeclsLast, TopDeclsNotLast),

	-- ** § 5.2 Export Lists types.
	ExportsBase(ExportsFromBeginning),
	ExportsRestBase(ExportsLast, ExportsNotLast),
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
	DeclsBase(DeclarationsFromBeginning),
	DeclsRestBase(DeclarationsEmpty, DeclarationsPush),
	DeclBase(DeclarationMeta, DeclarationValue),
	CdeclsBase(ClassDeclarationsFromBeginning),
	CdeclsRestBase(ClassDeclarationsEmpty, ClassDeclarationsPush),
	CdeclBase(ClassDeclarationMeta, ClassDeclarationValue),
	IdeclsBase(InstanceDeclarationsFromBeginning),
	IdeclsRestBase(InstanceDeclarationsEmpty, InstanceDeclarationsPush),
	IdeclBase(InstanceDeclarationValue, NullInstanceDeclaration),
	GenDeclBase(TypeDeclaration, FixityDeclaration, NullMetaDeclaration),
	OpsBase(OperationsLast, OperationsNotLast),
	VarsBase(VarsLast, VarsNotLast),
	FixityBase(FixityInfixr, FixityInfixl, FixityInfix),

	-- ** § 4.1.2 Syntax of Types types.
	TypeBase(Type),
	BTypeBase(TypeApplication),
	ATypeBase(GeneralTypeConstructor, TypeVariableType, TupleType, ListType, GroupedType),
	GtyconBase(QualifiedTypeConstructor, UnitTypeConstructor, EmptyListTypeConstructor, FunctionTypeConstructor, TupleConstructor),

	-- ** § 4.1.3 Syntax of Class Assertions and Contexts types.
	ContextBase(ContextSingle, Context),
	ClassBase(AssertUnappliedTypeVariableInClass, AssertAppliedTypeVariableInClass),
	AtypesBase(AtypesLast, AtypesNotLast),
	ClassesBase(ClassesFromBeginning),
	ClassesRestBase(ClassesEmpty, ClassesPush),
	QtyclsBase(TypeClass),
	TyclsBase(UnqualifiedTypeClass),
	TyvarBase(TypeVariable),

	-- ** § 4.2.1 Algebraic Datatype Declarations types.
	SimpleTypeBase(NamesType),
	ConstrsBase(ConstrsLast, ConstrsNotLast),
	ConstrBase(BasicConstructor, BinaryOperatorConstructor, RecordConstructor),
	EvalAtypes(EvalAtypesEmpty, EvalAtypesPush),
	EvalAtype(OrdableAtype),
	FieldDecls(FieldDeclsEmpty, FieldDeclsPush),
	FieldDecl(FieldDeclaration),
	DerivingBase(DerivingClause),
	DclassesListBase(DclassesSingle, Dclasses),
	DclassesBase(DclassesEmpty, DclassesPush),
	DclassBase(DerivingClass),

	-- ** § 4.2.2 Type Synonym Declarations types.
	-- TODO

	-- * Structures with default linking.

	-- ** Intra-module linking.
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
	  ImpDeclsLast    annotation impDecl
	| ImpDeclsNotLast annotation impDecl lexemeSemicolon fixpoint

-- | Top-level declarations: what a module defines.
data TopDeclsBase topDecl lexemeSemicolon annotation fixpoint =
	  TopDeclsLast    annotation topDecl
	| TopDeclsNotLast annotation topDecl lexemeSemicolon fixpoint

-- § 5.2 Export Lists types.

-- | A module's export list.
-- TODO: parens
data ExportsBase lexemeLeftParenthesis exportsRest annotation fixpoint =
	  ExportsFromBeginning annotation lexemeLeftParenthesis exportsRest

-- | The rest of a module's export list.
data ExportsRestBase maybe export lexemeComma lexemeRightParenthesis annotation fixpoint =
	  ExportsLast    annotation export (maybe lexemeComma) lexemeRightParenthesis
	| ExportsNotLast annotation export lexemeComma fixpoint

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

-- | Export (or import, etc.) a ‘data’ or ‘newtype’'s fields (‘var’s) and constructors (‘con’s).
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

-- | A block of declarations.
data DeclsBase lexemeLeftBrace declsRest annotation fixpoint =
	  DeclarationsFromBeginning annotation lexemeLeftBrace declsRest

-- | The rest of a block of declarations.
data DeclsRestBase maybe lexemeSemicolon lexemeRightBrace decl annotation fixpoint =
	  DeclarationsEmpty annotation (maybe lexemeSemicolon) lexemeRightBrace
	| DeclarationsPush  annotation decl lexemeSemicolon fixpoint

-- | A regular declaration.
--
-- It defines the type, value in a branch, or specification (e.g. fixity
-- attribute) of an attribute or variable.
data DeclBase either gendecl funlhs pat rhs annotation fixpoint =
	  DeclarationMeta  annotation gendecl
	| DeclarationValue annotation (either funlhs pat) rhs

-- | A block of class-declarations inside a class definition.
data CdeclsBase lexemeLeftBrace declsRest annotation fixpoint =
	  ClassDeclarationsFromBeginning annotation lexemeLeftBrace declsRest

-- | The rest of a block of class-declarations inside a class definition.
data CdeclsRestBase maybe lexemeSemicolon lexemeRightBrace decl annotation fixpoint =
	  ClassDeclarationsEmpty annotation (maybe lexemeSemicolon) lexemeRightBrace
	| ClassDeclarationsPush  annotation decl lexemeSemicolon fixpoint

-- | A regular class-declaration inside a class definition.
--
-- It defines the type, value in a branch, or specification (e.g. fixity
-- attribute) of an attribute or variable in a class definition.
data CdeclBase either gendecl funlhs pat rhs annotation fixpoint =
	  ClassDeclarationMeta  annotation gendecl
	| ClassDeclarationValue annotation (either funlhs pat) rhs

-- | A block of instance-declarations inside an instance body.
data IdeclsBase lexemeLeftBrace declsRest annotation fixpoint =
	  InstanceDeclarationsFromBeginning annotation lexemeLeftBrace declsRest

-- | The rest of a block of instance-declarations inside an instance body.
data IdeclsRestBase maybe lexemeSemicolon lexemeRightBrace decl annotation fixpoint =
	  InstanceDeclarationsEmpty annotation (maybe lexemeSemicolon) lexemeRightBrace
	| InstanceDeclarationsPush  annotation decl lexemeSemicolon fixpoint

-- | A regular instance-declaration inside an instance body.
--
-- It defines the value in a branch of an attribute or variable in an instance
-- body.
data IdeclBase either funlhs pat rhs annotation fixpoint =
	  InstanceDeclarationValue annotation (either funlhs pat) rhs
	| NullInstanceDeclaration  annotation

-- | Regular definition meta-data declarations, e.g. types and infix specifiers.
data GenDeclBase data2 maybe vars lexemeDoubleColon context lexemeDoubleRightArrow type_ fixity integer ops annotation fixpoint =
	  TypeDeclaration     annotation vars lexemeDoubleColon (maybe (data2 context lexemeDoubleRightArrow)) type_
	| FixityDeclaration   annotation fixity (maybe integer) ops
	| NullMetaDeclaration annotation

-- | A list of binary operators (not with parentheses around each).
--
-- Normally you'll see this list as part of a fixity declaration.
data OpsBase op lexemeComma annotation fixpoint =
	  OperationsLast    annotation op
	| OperationsNotLast annotation op lexemeComma fixpoint

-- | A list of variables, e.g in a type declaration.
data VarsBase var lexemeComma annotation fixpoint =
	  VarsLast    annotation var
	| VarsNotLast annotation var lexemeComma fixpoint

-- | A choice of fixity direction.
data FixityBase lexemeInfixl lexemeInfixr lexemeInfix annotation fixpoint =
	  FixityInfixr annotation lexemeInfixl
	| FixityInfixl annotation lexemeInfixr
	| FixityInfix  annotation lexemeInfix

-- § 4.1.2 Syntax of Types types.

-- | A type.
data TypeBase data2 maybe btype lexemeRightArrow annotation fixpoint =
	Type annotation btype (maybe (data2 lexemeRightArrow fixpoint))

-- | A type with possible type application.
data BTypeBase maybe atype annotation fixpoint =
	TypeApplication annotation (maybe fixpoint) atype

-- | An applicable type, one that can be part of type application.
data ATypeBase gtycon tyvar lexemeLeftParenthesis types lexemeRightParenthesis lexemeLeftAngleBracket type_ lexemeRightAngleBracket annotation fixpoint =
	  GeneralTypeConstructor annotation gtycon
	| TypeVariableType       annotation tyvar
	| TupleType              annotation lexemeLeftParenthesis  types lexemeRightParenthesis
	| ListType               annotation lexemeLeftAngleBracket type_ lexemeRightAngleBracket
	| GroupedType            annotation lexemeLeftParenthesis  type_ lexemeRightParenthesis

data GtyconBase list qtycon lexemeLeftParenthesis lexemeRightParenthesis lexemeLeftBracket lexemeRightBracket lexemeRightArrow lexemeComma annotation fixpoint =
	  QualifiedTypeConstructor annotation qtycon
	| UnitTypeConstructor      annotation lexemeLeftParenthesis lexemeRightParenthesis
	| EmptyListTypeConstructor annotation lexemeLeftBracket     lexemeRightBracket
	| FunctionTypeConstructor  annotation lexemeLeftParenthesis lexemeRightArrow       lexemeRightParenthesis
	| TupleConstructor         annotation lexemeLeftParenthesis lexemeComma            (list lexemeComma)

-- § 4.1.3 Syntax of Class Assertions and Contexts types.

-- | The context of a type class.
data ContextBase class_ classes annotation fixpoint =
	  ContextSingle annotation class_
		-- ^ The context form without parentheses, which requires the number of class assertions to be 1.
	| Context       annotation classes
		-- ^ The general context form.

-- | An assertion inside a context.
--
-- This assertion specifies that a type must be a member of a class, for what follows.
data ClassBase qtycls tyvar lexemeLeftParenthesis atypes lexemeRightParenthesis annotation fixpoint =
	  AssertUnappliedTypeVariableInClass annotation qtycls tyvar
	| AssertAppliedTypeVariableInClass   annotation qtycls lexemeLeftParenthesis atypes lexemeRightParenthesis

-- | A non-empty list of 'ATypeBase's.
--
-- This list of applicable types can appear in a type class assertion to a type
-- variable, where the resulting (applied) type is asserted to be a member of
-- the type class.
data AtypesBase atype annotation fixpoint =
	  AtypesLast    annotation atype
	| AtypesNotLast annotation atype fixpoint

-- | A list of class assertions.
data ClassesBase lexemeLeftParenthesis classesRest annotation fixpoint =
	ClassesFromBeginning annotation lexemeLeftParenthesis classesRest

-- | The rest of a list of class assetrions.
data ClassesRestBase class_ lexemeComma annotation fixpoint =
	  ClassesEmpty annotation
	| ClassesPush  annotation class_ lexemeComma fixpoint

-- | A type class name, optionally with a location qualifier by module.
data QtyclsBase data2 maybe modid lexemeDot tycls annotation fixpoint =
	TypeClass annotation (maybe (data2 modid lexemeDot)) tycls

-- | An unqualified type class name.
data TyclsBase conid annotation fixpoint =
	UnqualifiedTypeClass annotation conid

-- | A type variable.
data TyvarBase varid annotation fixpoint =
	TypeVariable annotation varid

-- § 4.2.1 Algebraic Datatype Declarations types.

-- | A type constructor name possibly with type variable names.
--
-- This is suitable e.g. as a ‘data’ type lhs.
data SimpleTypeBase list tycon tyvar annotation fixpoint =
	NamesType annotation tycon (list tyvar)

-- | One or more data constructions, for a ‘data’ ADT type declaration / definition.
data ConstrsBase constr lexemePipe annotation fixpoint =
	  ConstrsLast    annotation constr
	| ConstrsNotLast annotation constr lexemePipe fixpoint

-- | A data constructor, for a ‘data’ ADT type declaration / definition.
data ConstrBase either con evalAtypes btype evalAtype conop lexemeLeftBrace fieldDecls lexemeRightBrace annotation fixpoints =
	  BasicConstructor          annotation con evalAtypes
	| BinaryOperatorConstructor annotation (either btype evalAtype) conop (either btype evalAtype)
	| RecordConstructor         annotation con lexemeLeftBrace fieldDecls lexemeRightBrace

-- | Zero or more a-types, where each a-type is specified to have either strict
-- or lazy evaluation order.
--
-- An optional lexeme in the semantics phase can declare an evaluation ordering
-- dependency of a constructor field before the value.
data EvalAtypes evalAtype annotation fixpoint =
	  EvalAtypesEmpty annotation
	| EvalAtypesPush  annotation evalAtype fixpoint

-- | An applicable type that optionally can have a strict evaluation order specifier applied to it.
data EvalAtype maybe lexemeExclamation atype annotation fixpoint =
	OrdableAtype annotation (maybe lexemeExclamation) atype

-- | The zero or more field declarations that with the constructor name constitute a record.
data FieldDecls fieldDecl annotation fixpoint =
	  FieldDeclsEmpty annotation
	| FieldDeclsPush  annotation fieldDecl fixpoint

-- | A field declaration is one or more field names assigned to a type.
data FieldDecl either vars lexemeDoubleColon type_ evalAtype annotation fixpoint =
	FieldDeclaration annotation vars lexemeDoubleColon (either type_ evalAtype)

-- | A ‘deriving’ clause to declare automatic derivation for a ‘data’ ADT or ‘newtype’ type.
data DerivingBase dclassesList annotation fixpoint =
	DerivingClause annotation dclassesList

-- | A collection of classes to derive an instance for.
--
-- Supports omission of parentheses if there is only one class.
data DclassesListBase either dclass lexemeLeftParenthesis dclasses lexemeRightParenthesis annotation fixpoint =
	  DclassesSingle annotation dclass
		-- ^ The deriving class list form without parentheses, which requires the number of listed classes to be 1.
	| Dclasses       annotation lexemeLeftParenthesis dclasses lexemeRightParenthesis
		-- ^ The general deriving list form.

-- | A list of classes to derive an instance for.
data DclassesBase dclass lexemeComma annotation fixpoint =
	  DclassesEmpty annotation
	| DclassesPush  annotation dclass lexemeComma fixpoint

-- | A class name inside a ‘deriving’ declaration.
data DclassBase qtycls annotation fixpoints =
	DerivingClass annotation qtycls

-- § 4.2.2 Type Synonym Declarations types.

-- | TODO
-- TODO

{-
 - ----------------------------------------------------------------
 - Structures with default linking.
 - ----------------------------------------------------------------
 -}

-- Intra-module linking.
