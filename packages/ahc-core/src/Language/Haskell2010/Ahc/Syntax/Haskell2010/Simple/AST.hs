-- vim: set filetype=haskell noet

{-
 - CLI.hs
 -
 - A front-end to ‘ahc-minimal’.
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
	GtyconBase(QualifiableTypeConstructor, UnitTypeConstructor, EmptyListTypeConstructor, FunctionTypeConstructor, TupleTypeConstructor),

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

	-- ** § 4.2.3 Datatype Renamings types.
	NewConstrBase(BasicNewtypeConstructor, RecordNewtypeConstructor),

	-- ** § 4.3.1 Type Classes and Overloading types.
	SContextBase(SimpleContextSingle, SimpleContextList),
	SimpleClassBase(SimpleClassAssertion),
	SimpleClassesBase(SimpleClassesEmpty, SimpleClassesPush),

	-- ** § 4.3.1 Instance Declarations types.
	InstBase(GeneralTypeConstructorInstance, AppliableGeneralTypeConstructorInstance, TypeVariableTupleInstance, ListInstance, FunctionInstance),
	TyVarsBase(TyVarsEmpty, TyVarsPush),
	TypeVariablesTupleBase(TypeVariablesTupleFromFirst),
	TypeVariablesTupleRestBase(TypeVariablesTupleLast, TypeVariablesTupleNotLast),

	{-
	-- ** § 4.4.2 Fixity Declarations types.
	FixityOpBase(VariableOperation, ConstructorOperation),
	-}

	-- ** § 4.4.3 Function and Pattern Bindings types.
	FunLhsBase(RegularFunctionClause, InfixFunctionClause, AppendingFunctionClause),

	-- ** § 3 Expressions types.
	ExpBase(TypedExpression, UntypedExpression),
	InfixExpBase(RightInfixExpression, UnaryPrefixExpression, LeftExpression),
	LexpBase(LambdaExpression, LetExpression, ConditionalExpression, CaseExpression, DoExpression, BaseExpression),
	ApatsBase(ApatsEmpty, ApatsPush),
	FexpBase(ApplicationExpression),
	AexpBase(VariableExpression, ConstructorExpression, LiteralExpression, ParenthesesExpression, TupleExpression, ListExpression, ArithmeticSequenceExpression, ListComprehensionExpression, LeftSectionExpression, RightSectionExpression, ConstructRecordExpression, ModifyRecordExpression),
	Exps2Base(Exps2FromFirst),
	Exps2RestBase(Exps2Last, Exps2NotLast),
	Exps1Base(Exps1FromFirst),
	Exps1RestBase(Exps1End, Exps1Push),
	QualsBase(QualsLast, QualsNotLast),
	Fbinds0Base(Fbinds0Empty, Fbinds0FromFirst),
	Fbinds0RestBase(Fbinds0End, Fbinds0Push),
	Fbinds1Base(Fbinds1FromFirst),
	Fbinds1RestBase(Fbinds1End, Fbinds1Push),

	-- ** § 3.2 Variables, Constructors, Operators, and Literals types.
	GconBase(UnitConstructor, EmptyListConstuctor, TupleConstructor, QualifiableConstructor),
	VarBase(VariableNonsymbolic, VariableSymbolic),
	QvarBase(QualifiableVariableNonsymbolic, QualifiableVariableSymbolic),
	ConBase(ConstructorNonsymbolic, ConstructorSymbolic),
	QconBase(QualifiableConstructorNonsymbolic, QualifiableConstructorSymbolic),
	VarOpBase(SymbolicNonconstructorBinaryOperator, NonsymbolicNonconstructorBinaryOperator),
	QvarOpBase(QualifiableSymbolicNonconstructorBinaryOperator, QualifiableNonsymbolicNonconstructorBinaryOperator),
	ConOpBase(SymbolicConstructorBinaryOperator, NonsymbolicConstructorBinaryOperator),
	QconOpBase(QualifiableSymbolicConstructorBinaryOperator, QualifiableNonsymbolicConstructorBinaryOperator),
	OpBase(NonConstructorBinaryOperator, ConstructorBinaryOperator),
	QopBase(QualifiableNonConstructorBinaryOperator, QualifiableConstructorBinaryOperator),
	GconSymBase(ConsListConstructor, NonbuiltinQualifiableConstructorSymbolic),

	-- ** § 3.15.2 Construction Using Field Labels types.
	FbindBase(FieldBinding),

	-- ** § 3.17.1 Patterns types.
	PatBase(ConstructorBinaryOperationPattern, LeftPattern),
	LpatBase(BasePattern, MinusNumberPattern, ExposedConstructorPattern),
	ApatBase(AsPattern, ConstructorPattern, RecordPattern, LiteralPattern, WildcardPattern, GroupedPattern, TuplePattern, ListPattern, IrrefutablePattern),
	FpatsBase(FpatsEmpty, FpatsFromFirst),
	FpatsRestBase(FpatsLast, FpatsNotLast),
	Pats2Base(Pats2FromFirst),
	Pats2RestBase(Pats2Last, Pats2NotLast),
	Pats1Base(Pats1FromFirst),
	Pats1RestBase(Pats1End, Pats1Push),
	FpatBase(FieldPattern),

	-- TODO

	-- * Structures with default linking.

	-- ** Intra-module linking.

	-- TODO
) where

import Prelude ()

-- (General data pattern (within groups, order of appearance):
-- data *datas *typeCons *types annotation fixpoint = …

-- (Note: indentation should be consistent, agnostic to all valid space
-- equivalence settings.  I personally used 4.)

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
data TopDeclBase data2 maybe lexemeType simpleType type_ lexemeData context lexemeDoubleRightArrow lexemeEquals constrs deriving_ lexemeNewtype newConstr lexemeClass scontext tycls tyvar lexemeWhere cdecls lexemeInstance qtycls inst idecls lexemeDefault lexemeLeftParenthesis topDeclDefaultTypes lexemeRightParenthesis lexemeForeign fdecl decl annotation fixpoint =
	  TopDeclType     annotation lexemeType     simpleType            type_
	| TopDeclData     annotation lexemeData     (maybe (data2 context  lexemeDoubleRightArrow)) simpleType            (maybe (data2 lexemeEquals constrs)) (maybe deriving_)
	| TopDeclNewtype  annotation lexemeNewtype  (maybe (data2 context  lexemeDoubleRightArrow)) simpleType            lexemeEquals                         newConstr                          (maybe deriving_)
	| TopDeclClass    annotation lexemeClass    (maybe (data2 scontext lexemeDoubleRightArrow)) tycls                 tyvar                                (maybe (data2 lexemeWhere cdecls))
	| TopDeclInstance annotation lexemeInstance (maybe (data2 scontext lexemeDoubleRightArrow)) qtycls                inst                                 (maybe (data2 lexemeWhere idecls))
	| TopDeclDefault  annotation lexemeDefault  lexemeLeftParenthesis                           topDeclDefaultTypes   lexemeRightParenthesis
	| TopDeclForeign  annotation lexemeForeign  fdecl
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

-- | A type constructor, extended with a selection of built-in type constructors.
data GtyconBase list qtycon lexemeLeftParenthesis lexemeRightParenthesis lexemeLeftBracket lexemeRightBracket lexemeRightArrow lexemeComma annotation fixpoint =
	  QualifiableTypeConstructor annotation qtycon
	| UnitTypeConstructor        annotation lexemeLeftParenthesis lexemeRightParenthesis
	| EmptyListTypeConstructor   annotation lexemeLeftBracket     lexemeRightBracket
	| FunctionTypeConstructor    annotation lexemeLeftParenthesis lexemeRightArrow       lexemeRightParenthesis
	| TupleTypeConstructor       annotation lexemeLeftParenthesis lexemeComma            (list lexemeComma)

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

-- § 4.2.3 Datatype Renamings types.

-- | The constructor of a ‘newtype’ type.
data NewConstrBase con atype lexemeLeftBrace var lexemeDoubleColon type_ lexemeRightBrace annotation fixpoint =
	  BasicNewtypeConstructor  annotation con atype
	| RecordNewtypeConstructor annotation con lexemeLeftBrace var lexemeDoubleColon type_ lexemeRightBrace

-- § 4.3.1 Type Classes and Overloading types.

-- | A more restricted context with support just for simple class assertions.
--
-- Class assertions consist of names.
--
-- This is used rather than the full context structure by e.g. ‘class’
-- declarations.
data SContextBase simpleClass simpleClasses annotation fixpoint =
	  SimpleContextSingle annotation simpleClass
		-- ^ Form with ommitted parentheses, requiring exactly 1 simple class.
	| SimpleContextList   annotation simpleClasses
		-- ^ Normal form, with parentheses.

-- | A class assertion consisting of names.
--
-- This is found in simple contexts, which are like contexts, but they don't
-- support applications of types to other types, as such application is not
-- represented by a type variable name.  (e.g. ‘data’ supports full contexts,
-- and a Haskell2010 ‘class’ declaration only supports simple contexts.)
data SimpleClassBase qtycls tyvar annotation fixpoint =
	SimpleClassAssertion annotation qtycls tyvar

-- | A list of simple class assertions, used in simple contexts to list them out in one form.
--
-- This is in the form used with parentheses.
data SimpleClassesBase simpleClass lexemeComma annotation fixpoint =
	  SimpleClassesEmpty annotation
	| SimpleClassesPush  annotation simpleClass lexemeComma fixpoint

-- § 4.3.1 Instance Declarations types.

-- | The member part of an instance declaration.
--
-- This is what is declared to be an instance of a type class.
--
-- (The context and type class have already been written.  This is what
-- follows up through before the possible ‘where’ clause.)
--data InstBase gtycon tyvars lexemeLeftBracket tyvar lexemeRightBracket lexemeLeftParenthesis lexemeRightArrow lexemeRightParenthesis annotation fixpoint =
data InstBase gtycon lexemeLeftParenthesis tyvars lexemeRightParenthesis typeVariablesTuple lexemeLeftBracket tyvar lexemeRightBracket lexemeRightArrow annotation fixpoint =
	  GeneralTypeConstructorInstance          annotation gtycon
		-- ^ A type or a type constructor (but not in a form where it can be
		-- further applied with type variables after it is referenced here.)
	| AppliableGeneralTypeConstructorInstance annotation lexemeLeftParenthesis gtycon tyvars lexemeRightParenthesis
		-- ^ Like 'GeneralTypeConstructorInstance', but it has parentheses, so it can accommodate syntax for application.
		--
		-- (Note: in the separate semantics phase, the spec says the vars are distinct.)
		-- (In the syntax phase, we specify the structure of the variables.)
	| TypeVariableTupleInstance               annotation typeVariablesTuple
		-- (Note: again, in the separate semantics phase, the spec says the vars are distinct.)
	| ListInstance                            annotation lexemeLeftBracket tyvar lexemeRightBracket
		-- ^ Like 'AppliableGeneralTypeConstructorInstance' but with a sugar
		-- syntax for list type constructor application.
	| FunctionInstance                        annotation lexemeLeftParenthesis tyvar lexemeRightArrow tyvar lexemeRightParenthesis
		-- ^ Like 'AppliableGeneralTypeConstructorInstance' but with a sugar
		-- syntax for function (arrow) type constructor application.
		--
		-- (Note: again, in the separate semantics phase, the spec says the vars are distinct.)

-- | Zero or more type variables.
data TyVarsBase tyvar lexemeComma annotation fixpoint =
	  TyVarsEmpty annotation
	| TyVarsPush  annotation tyvar lexemeComma fixpoint

-- | Two or more type variables, represented as a tuple.
data TypeVariablesTupleBase lexemeLeftParenthesis tyvar typeVariablesTupleRest annotation fixpoint =
	TypeVariablesTupleFromFirst annotation lexemeLeftParenthesis tyvar typeVariablesTupleRest

-- | The rest of two or more type variables, represented as a tuple.
data TypeVariablesTupleRestBase tyvar lexemeRightParenthesis lexemeComma annotation fixpoint =
	  TypeVariablesTupleLast    annotation tyvar lexemeRightParenthesis
	| TypeVariablesTupleNotLast annotation tyvar lexemeComma            fixpoint

{-
-- § 4.4.2 Fixity Declarations types.

-- | A binary operation structure for fixity contexts.
data FixityOpBase varop conop annotation fixpoint =
	  VariableOperation    annotation varop
		-- ^ (Lowercase-style binary operation.)
	| ConstructorOperation annotation conop
		-- ^ (Upper-style binary operation.)
-}

-- § 4.4.3 Function and Pattern Bindings types.

-- | A branch into a function value in a function definition.
--
-- This is the left-hand side of a pattern-matching clause that has at least an
-- arity of 1.  (Arity-0 bindings are represented in the syntax at the ‘decl’-level,
-- inside the ‘either funlhs pat’ part.)  A clause is the individual
-- declaration, and the declarations constitute the definition of a function or
-- constant variable in a module as one of its attributes.
--
-- (Note then that ‘where’ blocks belong to the clause (and all its guard
-- groupings), rather than the last group of guards ‘guards’ but not the previous
-- ‘| guard guard … guard’ belonging to the same clause.)
data FunLhsBase list var apat pat varop lexemeLeftParenthesis lexemeRightParenthesis annotation fixpoint =
	  RegularFunctionClause   annotation var                   apat  (list apat)
	| InfixFunctionClause     annotation pat                   varop pat
	| AppendingFunctionClause annotation lexemeLeftParenthesis apat  lexemeRightParenthesis (list apat)

-- § 3 Expression types.

-- | An expression.
--
-- This is an expression that represents a value or what can be evaluated to
-- a value.
data ExpBase data2 maybe infixexp lexemeDoubleColon context lexemeDoubleRightArrow type_ annotation fixpoint =
	  TypedExpression   annotation infixexp lexemeDoubleColon (maybe (data2 context lexemeDoubleRightArrow)) type_
	| UntypedExpression annotation infixexp

-- | An infixable expression.
--
-- (We check first for right associativity.)
data InfixExpBase lexp qop lexemeMinus annotation fixpoint =
	  RightInfixExpression  annotation lexp qop fixpoint
		-- ^ An expression comprised of a binary operation application.
	| UnaryPrefixExpression annotation lexemeMinus fixpoint
		-- ^ (This is currently just ‘-’.)
	| LeftExpression        annotation lexp
		-- ^ An expression that is not a binary operation application on top.

-- | A left-expression, one not directly a unary or binary operation application.
data LexpBase maybe lexemeLambda apats lexemeRightArrow exp lexemeLet decls lexemeIn lexemeIf lexemeSemicolon lexemeThen lexemeElse lexemeCase lexemeOf lexemeLeftBrace alts lexemeRightBrace stmts annotation fexp fixpoint =
	  LambdaExpression      annotation lexemeLambda    apats lexemeRightArrow exp
	| LetExpression         annotation lexemeLet       decls lexemeIn exp
	| ConditionalExpression annotation lexemeIf        exp   (maybe lexemeSemicolon) lexemeThen exp (maybe lexemeSemicolon) lexemeElse exp
	| CaseExpression        annotation lexemeCase      exp   lexemeOf lexemeLeftBrace alts lexemeRightBrace
	| DoExpression          annotation lexemeLeftBrace stmts lexemeRightBrace
	| BaseExpression        annotation fexp
		-- ^ Function application, possibly with 0-arity.

-- | A non-empty sequence of apat patterns.
data ApatsBase apat annotation fixpoint =
	  ApatsEmpty annotation
	| ApatsPush  annotation apat fixpoint

-- | A base expression that support for application.
--
-- (At the top-level / root, this expression does not have one of the regular
-- wrapper expressions like ‘let’ or lambda constructs (but of course there may
-- be these within child nodes).)
data FexpBase maybe aexp annotation fixpoint =
	ApplicationExpression annotation (maybe fixpoint) aexp
		-- ^ Function application (of aexp base expressions), possibly with
		-- 0-arity, not a regular ‘annotating’ expression as described above.

-- | A base expression: variables, literals, or explicit groupings, etc.
data AexpBase maybe qvar gcon literal lexemeLeftParenthesis exp lexemeRightParenthesis exps2 exps1 lexemeLeftBracket lexemeDotDot lexemeRightBracket lexemePipe quals infixexp qop qopSansUnaryMinus qcon fbinds0 aexpSansQcon fbinds1 annotation fixpoint =
	  VariableExpression           annotation qvar
	| ConstructorExpression        annotation gcon
	| LiteralExpression            annotation literal
	| ParenthesesExpression        annotation lexemeLeftParenthesis exp               lexemeRightParenthesis
	| TupleExpression              annotation exps2
	| ListExpression               annotation exps1
	| ArithmeticSequenceExpression annotation lexemeLeftBracket     exp               (maybe exp) lexemeDotDot (maybe exp) lexemeRightBracket
	| ListComprehensionExpression  annotation lexemeLeftBracket     exp               lexemePipe  quals        lexemeRightBracket
	| LeftSectionExpression        annotation lexemeLeftParenthesis infixexp          qop         lexemeRightParenthesis
		-- ^ (The left section form of partial application, with a binary operation.)
	| RightSectionExpression       annotation lexemeLeftParenthesis qopSansUnaryMinus infixexp    lexemeRightParenthesis
		-- ^ (The right section form of partial application, with a binary operation.)
	| ConstructRecordExpression    annotation qcon                  fbinds0
	| ModifyRecordExpression       annotation aexpSansQcon          fbinds1

-- | Two or more expressions in a tuple, including the parentheses.
data Exps2Base lexemeLeftParenthesis exp lexemeComma exps2Rest annotation fixpoint =
	Exps2FromFirst annotation lexemeLeftParenthesis exp lexemeComma exps2Rest

-- | The rest of two or more expressions in a tuple.
--
-- This is after the first has been handled, so we can handle the last and
-- everything in between.  We can apply a different syntax depending on which
-- of these 3 categories the element falls under (first, between, last).
data Exps2RestBase exp lexemeRightParenthesis lexemeComma annotation fixpoint =
	  Exps2Last     annotation exp lexemeRightParenthesis
	| Exps2NotLast  annotation exp lexemeComma            fixpoint

-- | One or more expressions in a list, including the brackets.
data Exps1Base lexemeLeftBracket exp exps1Rest annotation fixpoint =
	Exps1FromFirst annotation lexemeLeftBracket exp exps1Rest

-- | The rest of one or more expressions in a list.
--
-- This is after the first has been handled, so we can handle the last and
-- everything in between.
data Exps1RestBase lexemeRightBracket lexemeComma exp annotation fixpoint =
	  Exps1End  annotation lexemeRightBracket
	| Exps1Push annotation lexemeComma        exp fixpoint

-- | A list comprehension's qualifiers (components after the pipe).
data QualsBase qual lexemeComma annotation fixpoint =
	  QualsLast    annotation qual
	| QualsNotLast annotation qual lexemeComma fixpoint

-- | Zero or more record field bindings, with the brackets.
--
-- This is used in record construction expressions.
data Fbinds0Base lexemeLeftBracket lexemeRightBracket fbind fbinds0Rest annotation fixpoint =
	  Fbinds0Empty     annotation lexemeLeftBracket lexemeRightBracket
	| Fbinds0FromFirst annotation lexemeLeftBracket fbind              fbinds0Rest

-- | The rest of zero or more record field bindings.
data Fbinds0RestBase lexemeRightBracket lexemeComma fbind annotation fixpoint =
	  Fbinds0End  annotation lexemeRightBracket
	| Fbinds0Push annotation lexemeComma        fbind fixpoint

-- | One or more record field bindings, with the brackets.
--
-- This is used in record construction expressions.
data Fbinds1Base lexemeLeftBracket fbind fbinds1Rest annotation fixpoint =
	Fbinds1FromFirst annotation lexemeLeftBracket fbind fbinds1Rest

-- | The rest of one or more record field bindings.
data Fbinds1RestBase lexemeRightBracket lexemeComma fbind annotation fixpoint =
	  Fbinds1End  annotation lexemeRightBracket
	| Fbinds1Push annotation lexemeComma        fbind fixpoint

-- § 3.2 Variables, Constructors, Operators, and Literals types.

-- | A value constructor, extended with a selection of built-in constructors.
data GconBase list lexemeLeftParenthesis lexemeRightParenthesis lexemeLeftBracket lexemeRightBracket lexemeComma qcon annotation fixpoint =
	  UnitConstructor        annotation lexemeLeftParenthesis lexemeRightParenthesis
	| EmptyListConstuctor    annotation lexemeLeftBracket     lexemeRightBracket
	| TupleConstructor       annotation lexemeLeftParenthesis lexemeComma            (list lexemeComma) lexemeRightParenthesis
	| QualifiableConstructor annotation qcon

-- | An unqualified variable, represented as a non-symbolic identifier name or a symbolic variable name.
data VarBase varid lexemeLeftParenthesis varSym lexemeRightParenthesis annotation fixpoint =
	  VariableNonsymbolic annotation varid
		-- ^ (E.g. ‘plus’.)
	| VariableSymbolic    annotation lexemeLeftParenthesis varSym lexemeRightParenthesis
		-- ^ (E.g. ‘(+)’.)

-- | A qualifiable variable, represented as a non-symbolic identifier name or a symbolic variable name.
data QvarBase qvarid lexemeLeftParenthesis qvarSym lexemeRightParenthesis annotation fixpoint =
	  QualifiableVariableNonsymbolic annotation qvarid
		-- ^ (E.g. ‘plus’.)
	| QualifiableVariableSymbolic    annotation lexemeLeftParenthesis qvarSym lexemeRightParenthesis
		-- ^ (E.g. ‘(+)’.)

-- | An unqualified constructor, represented as a non-symbolic identifier name or a symbolic variable name.
--
-- (This is like 'VarBase' but with capitalized syntax.)
data ConBase conid lexemeLeftParenthesis conSym lexemeRightParenthesis annotation fixpoint =
	  ConstructorNonsymbolic annotation conid
		-- ^ (E.g. ‘Plus’.)
	| ConstructorSymbolic    annotation lexemeLeftParenthesis conSym lexemeRightParenthesis
		-- ^ (E.g. ‘(:+)’.)

-- | A qualifiable constructor, represented as a non-symbolic identifier name or a symbolic variable name.
data QconBase qconid lexemeLeftParenthesis gconSym lexemeRightParenthesis annotation fixpoint =
	  QualifiableConstructorNonsymbolic annotation qconid
		-- ^ (E.g. ‘Plus’.)
	| QualifiableConstructorSymbolic    annotation lexemeLeftParenthesis gconSym lexemeRightParenthesis
		-- ^ (E.g. ‘(:+)’.)

-- | An unqualified binary operation, lowercase-style, for non-constructor variables.
data VarOpBase varSym lexemeBacktick varid annotation fixpoint =
	  SymbolicNonconstructorBinaryOperator    annotation varSym
	| NonsymbolicNonconstructorBinaryOperator annotation lexemeBacktick varid lexemeBacktick

-- | A qualifiable binary operation, lowercase-style, for non-constructor variables.
data QvarOpBase qvarSym lexemeBacktick qvarid annotation fixpoint =
	  QualifiableSymbolicNonconstructorBinaryOperator    annotation qvarSym
	| QualifiableNonsymbolicNonconstructorBinaryOperator annotation lexemeBacktick qvarid lexemeBacktick

-- | An unqualified binary operation, capitalized-style, for constructors and constructor variables.
data ConOpBase conSym lexemeBacktick conid annotation fixpoint =
	  SymbolicConstructorBinaryOperator    annotation conSym
	| NonsymbolicConstructorBinaryOperator annotation lexemeBacktick conid lexemeBacktick

-- | A qualifiable binary operation, capitalized-style, for constructors and constructor variables.
data QconOpBase qconSym lexemeBacktick qconid annotation fixpoint =
	  QualifiableSymbolicConstructorBinaryOperator    annotation qconSym
	| QualifiableNonsymbolicConstructorBinaryOperator annotation lexemeBacktick qconid lexemeBacktick

-- | An unqualified binary operation operator.
data OpBase varop conop annotation fixpoint =
	  NonConstructorBinaryOperator annotation varop
	| ConstructorBinaryOperator    annotation conop

-- | A qualifiable binary operation operator.
data QopBase qvarop qconop annotation fixpoint =
	  QualifiableNonConstructorBinaryOperator annotation qvarop
	| QualifiableConstructorBinaryOperator    annotation qconop

-- | A qualifiable symbolic constructor name, extended with a selection of built-in names.
data GconSymBase lexemeColon qcon annotation fixpoint =
	  ConsListConstructor                      annotation lexemeColon
	| NonbuiltinQualifiableConstructorSymbolic annotation qcon

-- § 3.15.2 Construction Using Field Labels types.

-- | A record field value binding.
--
-- Record construction and modification expressions can have these.
data FbindBase qvar lexemeEquals exp annotation fixpoint =
	FieldBinding annotation qvar lexemeEquals exp

-- § 3.17.1 Patterns types.

-- | A pattern, at the top-level, with support for a root-level constructor binary operation.
data PatBase lpat qconop annotation fixpoint =
	  ConstructorBinaryOperationPattern annotation lpat qconop fixpoint
	| LeftPattern                       annotation lpat

-- | Left patterns, embeddable in right binary operations, base patterns with a
-- few extensions like exposed arity-1+ constructor applications and negative
-- number patterns.
data LpatBase either apat lexemeMinus integer float gcon apats annotation fixpoint =
	  BasePattern               annotation apat
	| MinusNumberPattern        annotation lexemeMinus (either integer float)
	| ExposedConstructorPattern annotation gcon apats

-- | Base pattern.
--
-- (E.g. these can be parameters in a lambda expression.)
data ApatBase maybe var lexemeAt gcon qcon fpats literal lexemeUnderscore lexemeLeftParenthesis pat lexemeRightParenthesis pats2 pats1 lexemeTilde annotation fixpoint =
	  AsPattern          annotation var (maybe lexemeAt fixpoint)
		-- ^ As-pattern: add a name and assign it to a value that matches this
		-- pattern.
	| ConstructorPattern annotation gcon
	| RecordPattern      annotation qcon fpats
	| LiteralPattern     annotation literal
	| WildcardPattern    annotation lexemeUnderscore
	| GroupedPattern     annotation lexemeLeftParenthesis pat lexemeRightParenthesis
	| TuplePattern       annotation pats2
	| ListPattern        annotation pats1
	| IrrefutablePattern annotation lexemeTilde fixpoint
		-- ^ Lazy pattern matching, with ‘~’.

-- | Zero or more field patterns.
data FpatsBase lexemeLeftBrace lexemeRightBrace fpat fpatsRest annotation fixpoint =
	  FpatsEmpty     annotation lexemeLeftBrace lexemeRightBrace
	| FpatsFromFirst annotation lexemeLeftBrace fpat             fpatsRest

-- | The rest of zero or more field patterns.
data FpatsRestBase lexemeRightBrace lexemeComma fpat annotation fixpoint =
	  FpatsLast    annotation lexemeRightBrace
	| FpatsNotLast annotation lexemeComma      fpat fixpoint

-- | The 2 or more patterns constituting a tuple pattern, with parentheses.
data Pats2Base lexemeLeftParenthesis pat lexemeComma pats2Rest annotation fixpoint =
	Pats2FromFirst annotation lexemeLeftParenthesis pat lexemeComma pats2Rest

-- | The rest of 2 or more patterns constituting a tuple pattern.
data Pats2RestBase pat lexemeRightParenthesis lexemeComma annotation fixpoint =
	  Pats2Last    annotation pat lexemeRightParenthesis
	| Pats2NotLast annotation pat lexemeComma fixpoint

-- | The 1 or more patterns constituting a list pattern, with brackets.
data Pats1Base lexemeLeftBracket pat pats1Rest annotation fixpoint =
	Pats1FromFirst annotation lexemeLeftBracket pat pats1Rest

-- | The rest of 1 or more patterns constituting a list pattern.
data Pats1RestBase lexemeRightBracket lexemeComma pat annotation fixpoint =
	  Pats1End  annotation lexemeRightBracket
	| Pats1Push annotation lexemeComma pat fixpoint

-- | A field pattern.
--
-- Deconstruct a field using pattern matching to assign names to evaluable
-- components.
data FpatBase qvar lexemeEquals pat annotation fixpoint =
	FieldPattern annotation qvar lexemeEquals pat

-- TODO

{-
 - ----------------------------------------------------------------
 - Structures with default linking.
 - ----------------------------------------------------------------
 -}

-- Intra-module linking.

-- TODO: available option: lexemeLexer lexemeGrammar.  (Note that our linking method requires selecting lexeme annotations from what we have.)

-- TODO
