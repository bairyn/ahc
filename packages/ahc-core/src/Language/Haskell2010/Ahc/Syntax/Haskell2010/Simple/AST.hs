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
 -
 - TODO: finish.
 -}

{-# LANGUAGE Haskell2010 #-}

-- TODO: while it works without, it may be better to use lists, maybes, and repetitions rather than extra structures where there are sequences.

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
	{-
	ClassQtyclsBase(TypeClass),
	ClassTyclsBase(UnqualifiedTypeClass),
	ClassTyvarBase(TypeVariable),
	-}

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

	-- ** § 2.2 Lexical Program Structure types.
	ProgramBase(LexicalStructure),
	LexemeBase(QvaridLexeme, QconidLexeme, QvarSymLexeme, QconSymLexeme, LiteralLexeme, SpecialLexeme, ReservedOpLexeme, ReservedidLexeme),
	LiteralBase(IntegerLiteral, FloatLiteral, CharLiteral, StringLiteral),
	SpecialBase(LeftParenthesisSpecial, RightParenthesisSpecial, CommaSpecial, SemicolonSpecial, LeftBracketSpecial, RightBracketSpecial, BacktickSpecial, LeftBraceSpecial, RightBraceSpecial),
	SpecialSansNcBase(LeftParenthesisSpecialSansNc, RightParenthesisSpecialSansNc, CommaSpecialSansNc, SemicolonSpecialSansNc, LeftBracketSpecialSansNc, RightBracketSpecialSansNc, BacktickSpecialSansNc),

	WhitespaceBase(WholeWhitespace),
	WhitestuffBase(WhitecharWhitestuff, CommentWhitestuff, MultilinableComment),
	WhitecharBase(NewlineWhitechar, VerticalTabWhitechar, SpaceWhitechar, TabWhitechar, UnicodeWhitechar),
	NewlineBase(WindowsNewline, OldMacNewline, UnixNewline, FormfeedNewline),
	ReturnBase(CRReturn),
	LinefeedBase(LFLinefeed),
	VertabBase(VTVertab),
	FormfeedBase(FFFormfeed),
	SpaceBase(SPSpace),
	TabBase(HTTab),
	UniWhiteBase(UnicodeWhitespaceChar),

	CommentBase(SinglelineComment),
	DashesBase(SinglelineCommentDashes),
	OpenComBase(MultilineCommentOpening),
	CloseComBase(MultilineCommentClosing),
	NcommentBase(MultilineComment),
	BigAnySeqBase(EmptyBigAnySeqBase, NotNcomChar0BigAnySeqBase, ValidNcomChar0BigAnySeqBase, ValidNcomChar0_0BigAnySeqBase, ValidNcomChar0_1BigAnySeqBase),
	BigAnySeqValidNcomChar1_0Base(NotNcomChar1_0, LeftBraceChar1_0, RightBraceChar1_0, EOPChar1_0),
	BigAnySeqValidNcomChar1_1Base(NotNcomChar1_1, LeftBraceChar1_1, HyphenChar1_1, EOFChar1_1),
	BigAnyBase(GraphicBigAny, WhitecharBigAny),
	BigAnySansNcBase(GraphicBigAnySansNc, WhitecharBigAnySansNc),
	AnyBase(GraphicAny, SpaceAny, TabAny),
	GraphicBase(SmallGraphic, LargeGraphic, SymbolGraphic, DigitGraphic, SpecialGraphic, DoubleQuoteGraphic, SingleQuoteGraphic),
	GraphicSansNcBase(SmallGraphicSansNc, LargeGraphicSansNc, SymbolGraphicSansNc, DigitGraphicSansNc, SpecialGraphicSansNc, DoubleQuoteGraphicSansNc, SingleQuoteGraphicSansNc),

	SmallBase(AsciiSmall, UnicodeSmall, UnderscoreSmall),
	AscSmallBase(AsciiSmallA, AsciiSmallB, AsciiSmallC, AsciiSmallD, AsciiSmallE, AsciiSmallF, AsciiSmallG, AsciiSmallH, AsciiSmallI, AsciiSmallJ, AsciiSmallK, AsciiSmallL, AsciiSmallM, AsciiSmallN, AsciiSmallO, AsciiSmallP, AsciiSmallQ, AsciiSmallR, AsciiSmallS, AsciiSmallT, AsciiSmallU, AsciiSmallV, AsciiSmallW, AsciiSmallX, AsciiSmallY, AsciiSmallZ),
	UniSmallBase(UnicodeSmallUniSmall),

	LargeBase(AsciiLarge, UnicodeLarge),
	AscLargeBase(AsciiLargeA, AsciiLargeB, AsciiLargeC, AsciiLargeD, AsciiLargeE, AsciiLargeF, AsciiLargeG, AsciiLargeH, AsciiLargeI, AsciiLargeJ, AsciiLargeK, AsciiLargeL, AsciiLargeM, AsciiLargeN, AsciiLargeO, AsciiLargeP, AsciiLargeQ, AsciiLargeR, AsciiLargeS, AsciiLargeT, AsciiLargeU, AsciiLargeV, AsciiLargeW, AsciiLargeX, AsciiLargeY, AsciiLargeZ),
	UniLargeBase(UnicodeLargeUniLarge),
	SymbolBase(AsciiNonspecialSymbol, UnicodeNonspecialNonscorequoteSymbol),
	SymbolSansNcBase(AsciiNonspecialSymbolSansNc, UnicodeNonspecialNonscorequoteSymbolSansNc),

	AscSymbolBase(ExclamationAsciiSymbol, HashAsciiSymbol, DollarAsciiSymbol, PercentAsciiSymbol, AmpersandAsciiSymbol, AsteriskAsciiSymbol, PlusAsciiSymbol, DotAsciiSymbol, SlashAsciiSymbol, LeftAngleBracketAsciiSymbol, EqualsAsciiSymbol, RightAngleBracketAsciiSymbol, QuestionmarkAsciiSymbol, AtAsciiSymbol, BackslashAsciiSymbol, CaretAsciiSymbol, PipeAsciiSymbol, HyphenAsciiSymbol, TildeAsciiSymbol, ColonAsciiSymbol),
	AscSymbolSansNcBase(ExclamationAsciiSymbolSansNc, HashAsciiSymbolSansNc, DollarAsciiSymbolSansNc, PercentAsciiSymbolSansNc, AmpersandAsciiSymbolSansNc, AsteriskAsciiSymbolSansNc, PlusAsciiSymbolSansNc, DotAsciiSymbolSansNc, SlashAsciiSymbolSansNc, LeftAngleBracketAsciiSymbolSansNc, EqualsAsciiSymbolSansNc, RightAngleBracketAsciiSymbolSansNc, QuestionmarkAsciiSymbolSansNc, AtAsciiSymbolSansNc, BackslashAsciiSymbolSansNc, CaretAsciiSymbolSansNc, PipeAsciiSymbolSansNc, TildeAsciiSymbolSansNc, ColonAsciiSymbolSansNc),
	UniSymbolBase(UnicodeSymbol),
	UniSymbolSansSpecialishBase(UnicodeSymbolSansSpecialish),
	UniSymbolSansSpecialishSansNcBase(UnicodeSymbolSansSpecialishSansNc),
	DigitBase(AsciiDigit, UnicodeDigit),
	AscDigitBase(N0AsciiDigit, N1AsciiDigit, N2AsciiDigit, N3AsciiDigit, N4AsciiDigit, N5AsciiDigit, N6AsciiDigit, N7AsciiDigit, N8AsciiDigit, N9AsciiDigit),
	UniDigitBase(UnicodeDigitUniDigit),
	OctitBase(N0Octit, N1Octit, N2Octit, N3Octit, N4Octit, N5Octit, N6Octit, N7Octit),
	HexitBase(DigitHexit, AHexit, BHexit, CHexit, DHexit, EHexit, FHexit, ALowerHexit, BLowerHexit, CLowerHexit, DLowerHexit, ELowerHexit, FLowerHexit),

	-- ** § 2.4 Identifiers and Operators types.
	UnqualifiedNameBase(UnqualifiedVaridName, UnqualifiedConidName, UnqualifiedTyvarName, UnqualifiedTyconName, UnqualifiedTyclsName, UnqualifiedModidName),
	-- | (Note: 'VaridBase', with exclusions, appears later in this section.)
	-- (TODO: VaridBase)
	VaridNoExclusionsBase(VaridNoExclusionsWhole),
	VaridStartBase(SmallVaridStart),
	IdentifierInnerBase(SmallIdentifierInner, LargeIdentifierInner, DigitIdentifierInner, SingleQuoteIdentifierInner),
	VaridInnerBase(IdInnerVaridInner),
	ConidBase(ConidWhole),
	ConidStartBase(LargeConidStart),
	ConidInnerBase(IdInnerConidInner),
	ReservedidBase(CaseReservedId, ClassReservedId, DataReservedId, DefaultReservedId, DerivingReservedId, DoReservedId, ElseReservedId, ForeignReservedId, IfReservedId, ImportReservedId, InReservedId, InfixReservedId, InfixlReservedId, InfixrReservedId, InstanceReservedId, LetReservedId, ModuleReservedId, NewtypeReservedId, OfReservedId, ThenReservedId, TypeReservedId, WhereReservedId, UnderscoreReservedId),
	-- | (Note: 'VaridBase', with exclusions, appears later in this section.)
	-- (TODO: VarsymBase)
	VarsymNoExtraExclusionsBase(VarsymWhole),
	VarsymStartBase(SymbolSansColonVarsymStart),
	-- | (Note: 'ConidBase', with exclusions, appears later in this section.)
	-- (TODO: ConsymBase)
	ConsymNoExtraExclusionsBase(ConsymWhole),
	ConsymStartBase(ColonConsymStart),
	ReservedOpBase(DotDotReservedOp, ColonReservedOp, DoubleColonReservedOp, EqualsReservedOp, BackslashReservedOp, PipeReservedOp, LeftArrowReservedOp, RightArrowReservedOp, AtReservedOp, TildeReservedOp, DoubleRightArrowReservedOp),
	TyvarBase(TypeVariableName),
	TyconBase(TypeConstructorName),
	TyclsBase(TypeClassName),
	ModidBase(ModuleName),
	NameBase(VaridName, ConidName, TyconName, TyclsName, VarSymName, ConSymName),
	QvaridBase(QualifiableVarid),
	QconidBase(QualifiableConid),
	QtyconBase(QualifiableTycon),
	QtyclsBase(QualifiableTycls),
	QvarSymBase(QualifiableVarSym),
	QconSymBase(QualifiableConSym),

	-- (Exclusion structures.)
	-- TODO:
	{-
	VaridBase(_),
	-}

	-- TODO exclusions structures

	-- ** § 2.5 Numeric Literals types.
	DecimalBase(DecimalLiteral),
	OctalBase(OctalLiteral),
	HexadecimalBase(HexadecimalLiteral),
	IntegerBase(DecimalInteger, OctalInteger, CapitalOctalInteger, HexadecimalInteger, CapitalHexadecimalInteger),
	FloatBase(PointFloatLiteral, ExponentFloatLiteral),
	ExponentBase(FloatExponent),

	-- ** § 2.6 Character and String Literals types.
	CharBase(CharLiteralChar),
	CharLiteralInnerBase(CharLiteralInnerGraphic, CharLiteralInnerSpace, CharLiteralInnerEscape),
	StringBase(StringLiteralString),
	StringLiteralInnerUnitBase(StringLiteralInnerGraphic, StringLiteralInnerSpace, StringLiteralInnerEscape, StringLiteralInnerGap),
	EscapeBase(EscapedChar),
	EscapeInnerBase(EscapedCharEsc, EscapedAscii, EscapedDecimal, EscapedOctal, EscapedHexadecimal),
	CharEscBase(EscapedA, EscapedB, EscapedF, EscapedN, EscapedR, EscapedT, EscapedV, EscapedBackslash, EscapedDoubleQuote, EscapedSingleQuote, EscapedAmpersand),
	AsciiBase(AsciiControl, AsciiNull, AsciiStartOfHeading, AsciiStartOfText, AsciiEndOfText, AsciiEndOfTransmission, AsciiEnquiry, AsciiAcknowledgement, AsciiBell, AsciiBackspace, AsciiHorizontalTab, AsciiLineFeed, AsciiVerticalTab, AsciiFormFeed, AsciiCarriageReturn, AsciiShiftOut, AsciiShiftIn, AsciiDataLinkEscape, AsciiDeviceControl1XON, AsciiDeviceControl2, AsciiDeviceControl3XOFF, AsciiDeviceControl4, AsciiNegativeAcknowledgement, AsciiSynchronousIdle, AsciiEndOfTransmissionBlock, AsciiCancel, AsciiEndOfMedium, AsciiSubstitute, AsciiEscape, AsciiFileSeparator, AsciiGroupSeparator, AsciiRecordSeparator, AsciiUnitSeparator, AsciiSpace, AsciiDelete),
	CntrlBase(AsciiControlAscLarge, AsciiControlAt, AsciiControlLeftBracket, AsciiControlBackslash, AsciiControlRightBracket, AsciiControlCaret, AsciiControlUnderscore),
	GapBase(WhitespaceGap),

	-- ** Base lexical structures.

	-- *** Exclusion and miscellaneous lexical structures.

	-- *** Lexical foundation.

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
data ModuleBase maybe lexicalModule modid exports lexicalWhere body annotation fixpoint =
	  ModuleWithHeader    annotation lexicalModule modid (maybe exports) lexicalWhere body
	| ModuleWithoutHeader annotation body

-- | A module's body.
data BodyBase lexicalLeftBrace impDecls lexicalSemicolon topDecls lexicalRightBrace annotation fixpoint =
	  BodyImportsTops annotation lexicalLeftBrace impDecls lexicalSemicolon  topDecls lexicalRightBrace
	| BodyImportsOnly annotation lexicalLeftBrace impDecls lexicalRightBrace
	| BodyTopsOnly    annotation lexicalLeftBrace topDecls lexicalRightBrace

-- | Import block.
data ImpDeclsBase impDecl lexicalSemicolon annotation fixpoint =
	  ImpDeclsLast    annotation impDecl
	| ImpDeclsNotLast annotation impDecl lexicalSemicolon fixpoint

-- | Top-level declarations: what a module defines.
data TopDeclsBase topDecl lexicalSemicolon annotation fixpoint =
	  TopDeclsLast    annotation topDecl
	| TopDeclsNotLast annotation topDecl lexicalSemicolon fixpoint

-- § 5.2 Export Lists types.

-- | A module's export list.
data ExportsBase lexicalLeftParenthesis exportsRest annotation fixpoint =
	  ExportsFromBeginning annotation lexicalLeftParenthesis exportsRest

-- | The rest of a module's export list.
data ExportsRestBase maybe export lexicalComma lexicalRightParenthesis annotation fixpoint =
	  ExportsLast    annotation export (maybe lexicalComma) lexicalRightParenthesis
	| ExportsNotLast annotation export lexicalComma         fixpoint

-- | An export, in a module's export list.
--
-- (modid: module identifier.)
data ExportBase data3 maybe either qvar qtycon lexicalLeftParenthesis lexicalDotDot lexicalRightParenthesis exportCnames qtycls exportVars lexicalModule modid annotation fixpoint =
	  ExportVariable      annotation qvar
	| ExportTypeVariable  annotation qtycon        (maybe (either (data3 lexicalLeftParenthesis lexicalDotDot lexicalRightParenthesis) (data3 lexicalLeftParenthesis exportCnames lexicalRightParenthesis)))
	| ExportClassVariable annotation qtycls        (maybe (either (data3 lexicalLeftParenthesis lexicalDotDot lexicalRightParenthesis) (data3 lexicalLeftParenthesis exportVars lexicalRightParenthesis)))
	| ExportModule        annotation lexicalModule modid

-- | An ADT (‘data’) or ‘newtype’ export item.
--
-- Export a data type or newtype type and its fields and constructors.
data ExportCnamesBase cname lexicalComma annotation fixpoint =
	  ExportCnamesEmpty annotation
	| ExportCnamesPush  annotation cname lexicalComma fixpoint

-- | Export a type class's members.
data ExportVarsBase var lexicalComma annotation fixpoint =
	  ExportVarsEmpty annotation
	| ExportVarsPush  annotation var lexicalComma fixpoint

-- | Export (or import, etc.) a ‘data’ or ‘newtype’'s fields (‘var’s) and constructors (‘con’s).
data CnameBase var con annotation fixpoint =
	  CnameVar annotation var
	| CnameCon annotation con

-- § 5.3 Import Declarations types.

-- | Import statement.
data ImpDeclBase data2 maybe lexicalImport lexicalQualified modid lexicalAs impSpec annotation fixpoint =
	  ImportStatement annotation lexicalImport (maybe lexicalQualified) modid (maybe (data2 lexicalAs modid)) (maybe impSpec)
	| NullImport      annotation

-- | Import specification: what to import.
data ImpSpecBase lexicalLeftParenthesis importAttrs lexicalRightParenthesis lexicalHiding annotation fixpoint =
	  ImportWhitelist annotation               lexicalLeftParenthesis importAttrs lexicalRightParenthesis
		-- ^ Import only ….
	| ImportBlacklist annotation lexicalHiding lexicalLeftParenthesis importAttrs lexicalRightParenthesis
		-- ^ Import all but ….

-- | List of what elements (attributes) to import from a module.
data ImportAttrsBase maybe lexicalComma import_ annotation fixpoint =
	  ImportAttrsEmpty annotation (maybe lexicalComma)
	| ImportAttrsPush  annotation import_              lexicalComma fixpoint

-- | An individual attribute (e.g. variable) in a list of elements to import from a module in an import statement.
data ImportBase data3 maybe either var lexicalLeftParenthesis lexicalDotDot lexicalRightParenthesis importCnames importVars annotation fixpoint =
	  ImportVariableAttribute          annotation var
		-- ^ Import a regular variable from a module.
	| ImportTypeVariableAttribute      annotation (maybe (either (lexicalLeftParenthesis lexicalDotDot lexicalRightParenthesis) (lexicalLeftParenthesis importCnames lexicalRightParenthesis)))
		-- ^ Import a ‘data’ type or ‘newtype’ type from a module.
	| ImportTypeClassVariableAttribute annotation (maybe (either (lexicalLeftParenthesis lexicalDotDot lexicalRightParenthesis) (lexicalLeftParenthesis importVars lexicalRightParenthesis)))
		-- ^ Import a type class from a module.

-- | An ADT (‘data’) or ‘newtype’ import item.
--
-- Import a data type or newtype type and its fields and constructors from a module.
data ImportCnamesBase cname lexicalComma annotation fixpoint =
	  ImportCnamesEmpty annotation
	| ImportCnamesPush  annotation cname lexicalComma fixpoint

-- | Import a type class's members.
data ImportVarsBase var lexicalComma annotation fixpoint =
	  ImportVarsEmpty annotation
	| ImportVarsPush  annotation var lexicalComma fixpoint

-- § 4 Declarations and Bindings types.

-- | Top-level declarations.
--
-- These are the components of a module, especially regular declarations
-- ('TopDeclRegular') for attributes of modules that can be exported and
-- imported.
data TopDeclBase data2 maybe lexicalType simpleType type_ lexicalData context lexicalDoubleRightArrow lexicalEquals constrs deriving_ lexicalNewtype newConstr lexicalClass scontext tycls tyvar lexicalWhere cdecls lexicalInstance qtycls inst idecls lexicalDefault lexicalLeftParenthesis topDeclDefaultTypes lexicalRightParenthesis lexicalForeign fdecl decl annotation fixpoint =
	  TopDeclType     annotation lexicalType     simpleType             type_
	| TopDeclData     annotation lexicalData     (maybe (data2 context  lexicalDoubleRightArrow)) simpleType            (maybe (data2 lexicalEquals constrs)) (maybe deriving_)
	| TopDeclNewtype  annotation lexicalNewtype  (maybe (data2 context  lexicalDoubleRightArrow)) simpleType            lexicalEquals                         newConstr                          (maybe deriving_)
	| TopDeclClass    annotation lexicalClass    (maybe (data2 scontext lexicalDoubleRightArrow)) tycls                 tyvar                                 (maybe (data2 lexicalWhere cdecls))
	| TopDeclInstance annotation lexicalInstance (maybe (data2 scontext lexicalDoubleRightArrow)) qtycls                inst                                  (maybe (data2 lexicalWhere idecls))
	| TopDeclDefault  annotation lexicalDefault  lexicalLeftParenthesis                           topDeclDefaultTypes   lexicalRightParenthesis
	| TopDeclForeign  annotation lexicalForeign  fdecl
	| TopDeclRegular  annotation decl

-- | A list of types to specify as ‘default’ in a ‘default’ declaration.
data TopDeclDefaultTypesBase type_ lexicalComma annotation fixpoint =
	  TopDeclDefaultTypesEmpty annotation
	| TopDeclDefaultTypesPush  annotation type_ lexicalComma fixpoint

-- | A block of declarations.
data DeclsBase lexicalLeftBrace declsRest annotation fixpoint =
	  DeclarationsFromBeginning annotation lexicalLeftBrace declsRest

-- | The rest of a block of declarations.
data DeclsRestBase maybe lexicalSemicolon lexicalRightBrace decl annotation fixpoint =
	  DeclarationsEmpty annotation (maybe lexicalSemicolon) lexicalRightBrace
	| DeclarationsPush  annotation decl                     lexicalSemicolon  fixpoint

-- | A regular declaration.
--
-- It defines the type, value in a branch, or specification (e.g. fixity
-- attribute) of an attribute or variable.
data DeclBase either gendecl funlhs pat rhs annotation fixpoint =
	  DeclarationMeta  annotation gendecl
	| DeclarationValue annotation (either funlhs pat) rhs

-- | A block of class-declarations inside a class definition.
data CdeclsBase lexicalLeftBrace declsRest annotation fixpoint =
	  ClassDeclarationsFromBeginning annotation lexicalLeftBrace declsRest

-- | The rest of a block of class-declarations inside a class definition.
data CdeclsRestBase maybe lexicalSemicolon lexicalRightBrace decl annotation fixpoint =
	  ClassDeclarationsEmpty annotation (maybe lexicalSemicolon) lexicalRightBrace
	| ClassDeclarationsPush  annotation decl                     lexicalSemicolon  fixpoint

-- | A regular class-declaration inside a class definition.
--
-- It defines the type, value in a branch, or specification (e.g. fixity
-- attribute) of an attribute or variable in a class definition.
data CdeclBase either gendecl funlhs pat rhs annotation fixpoint =
	  ClassDeclarationMeta  annotation gendecl
	| ClassDeclarationValue annotation (either funlhs pat) rhs

-- | A block of instance-declarations inside an instance body.
data IdeclsBase lexicalLeftBrace declsRest annotation fixpoint =
	  InstanceDeclarationsFromBeginning annotation lexicalLeftBrace declsRest

-- | The rest of a block of instance-declarations inside an instance body.
data IdeclsRestBase maybe lexicalSemicolon lexicalRightBrace decl annotation fixpoint =
	  InstanceDeclarationsEmpty annotation (maybe lexicalSemicolon) lexicalRightBrace
	| InstanceDeclarationsPush  annotation decl                     lexicalSemicolon  fixpoint

-- | A regular instance-declaration inside an instance body.
--
-- It defines the value in a branch of an attribute or variable in an instance
-- body.
data IdeclBase either funlhs pat rhs annotation fixpoint =
	  InstanceDeclarationValue annotation (either funlhs pat) rhs
	| NullInstanceDeclaration  annotation

-- | Regular definition meta-data declarations, e.g. types and infix specifiers.
data GenDeclBase data2 maybe vars lexicalDoubleColon context lexicalDoubleRightArrow type_ fixity integer ops annotation fixpoint =
	  TypeDeclaration     annotation vars   lexicalDoubleColon (maybe (data2 context lexicalDoubleRightArrow)) type_
	| FixityDeclaration   annotation fixity (maybe integer)    ops
	| NullMetaDeclaration annotation

-- | A list of binary operators (not with parentheses around each).
--
-- Normally you'll see this list as part of a fixity declaration.
data OpsBase op lexicalComma annotation fixpoint =
	  OperationsLast    annotation op
	| OperationsNotLast annotation op lexicalComma fixpoint

-- | A list of variables, e.g in a type declaration.
data VarsBase var lexicalComma annotation fixpoint =
	  VarsLast    annotation var
	| VarsNotLast annotation var lexicalComma fixpoint

-- | A choice of fixity direction.
data FixityBase lexicalInfixl lexicalInfixr lexicalInfix annotation fixpoint =
	  FixityInfixr annotation lexicalInfixl
	| FixityInfixl annotation lexicalInfixr
	| FixityInfix  annotation lexicalInfix

-- § 4.1.2 Syntax of Types types.

-- | A type.
data TypeBase data2 maybe btype lexicalRightArrow annotation fixpoint =
	Type annotation btype (maybe (data2 lexicalRightArrow fixpoint))

-- | A type with possible type application.
data BTypeBase maybe atype annotation fixpoint =
	TypeApplication annotation (maybe fixpoint) atype

-- | An applicable type, one that can be part of type application.
data ATypeBase gtycon tyvar lexicalLeftParenthesis types lexicalRightParenthesis lexicalLeftAngleBracket type_ lexicalRightAngleBracket annotation fixpoint =
	  GeneralTypeConstructor annotation gtycon
	| TypeVariableType       annotation tyvar
	| TupleType              annotation lexicalLeftParenthesis  types lexicalRightParenthesis
	| ListType               annotation lexicalLeftAngleBracket type_ lexicalRightAngleBracket
	| GroupedType            annotation lexicalLeftParenthesis  type_ lexicalRightParenthesis

-- | A type constructor, extended with a selection of built-in type constructors.
data GtyconBase list qtycon lexicalLeftParenthesis lexicalRightParenthesis lexicalLeftBracket lexicalRightBracket lexicalRightArrow lexicalComma annotation fixpoint =
	  QualifiableTypeConstructor annotation qtycon
	| UnitTypeConstructor        annotation lexicalLeftParenthesis lexicalRightParenthesis
	| EmptyListTypeConstructor   annotation lexicalLeftBracket     lexicalRightBracket
	| FunctionTypeConstructor    annotation lexicalLeftParenthesis lexicalRightArrow       lexicalRightParenthesis
	| TupleTypeConstructor       annotation lexicalLeftParenthesis lexicalComma            (list lexicalComma)

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
data ClassBase qtycls tyvar lexicalLeftParenthesis atypes lexicalRightParenthesis annotation fixpoint =
	  AssertUnappliedTypeVariableInClass annotation qtycls tyvar
	| AssertAppliedTypeVariableInClass   annotation qtycls lexicalLeftParenthesis atypes lexicalRightParenthesis

-- | A non-empty list of 'ATypeBase's.
--
-- This list of applicable types can appear in a type class assertion to a type
-- variable, where the resulting (applied) type is asserted to be a member of
-- the type class.
data AtypesBase atype annotation fixpoint =
	  AtypesLast    annotation atype
	| AtypesNotLast annotation atype fixpoint

-- | A list of class assertions.
data ClassesBase lexicalLeftParenthesis classesRest annotation fixpoint =
	ClassesFromBeginning annotation lexicalLeftParenthesis classesRest

-- | The rest of a list of class assetrions.
data ClassesRestBase class_ lexicalComma annotation fixpoint =
	  ClassesEmpty annotation
	| ClassesPush  annotation class_ lexicalComma fixpoint

{-
-- | A type class name, optionally with a location qualifier by module, in a class context.
data ClassQtyclsBase data2 maybe modid lexicalDot tycls annotation fixpoint =
	TypeClass annotation (maybe (data2 modid lexicalDot)) tycls

-- | An unqualified type class name.
data ClassTyclsBase conid annotation fixpoint =
	UnqualifiedTypeClass annotation conid

-- | A type variable.
data ClassTyvarBase varid annotation fixpoint =
	TypeVariable annotation varid
-}

-- § 4.2.1 Algebraic Datatype Declarations types.

-- | A type constructor name possibly with type variable names.
--
-- This is suitable e.g. as a ‘data’ type lhs.
data SimpleTypeBase list tycon tyvar annotation fixpoint =
	NamesType annotation tycon (list tyvar)

-- | One or more data constructions, for a ‘data’ ADT type declaration / definition.
data ConstrsBase constr lexicalPipe annotation fixpoint =
	  ConstrsLast    annotation constr
	| ConstrsNotLast annotation constr lexicalPipe fixpoint

-- | A data constructor, for a ‘data’ ADT type declaration / definition.
data ConstrBase either con evalAtypes btype evalAtype conop lexicalLeftBrace fieldDecls lexicalRightBrace annotation fixpoints =
	  BasicConstructor          annotation con                      evalAtypes
	| BinaryOperatorConstructor annotation (either btype evalAtype) conop            (either btype evalAtype)
	| RecordConstructor         annotation con                      lexicalLeftBrace fieldDecls               lexicalRightBrace

-- | Zero or more a-types, where each a-type is specified to have either strict
-- or lazy evaluation order.
--
-- An optional lexeme in the semantics phase can declare an evaluation ordering
-- dependency of a constructor field before the value.
data EvalAtypes evalAtype annotation fixpoint =
	  EvalAtypesEmpty annotation
	| EvalAtypesPush  annotation evalAtype fixpoint

-- | An applicable type that optionally can have a strict evaluation order specifier applied to it.
data EvalAtype maybe lexicalExclamation atype annotation fixpoint =
	OrdableAtype annotation (maybe lexicalExclamation) atype

-- | The zero or more field declarations that with the constructor name constitute a record.
data FieldDecls fieldDecl annotation fixpoint =
	  FieldDeclsEmpty annotation
	| FieldDeclsPush  annotation fieldDecl fixpoint

-- | A field declaration is one or more field names assigned to a type.
data FieldDecl either vars lexicalDoubleColon type_ evalAtype annotation fixpoint =
	FieldDeclaration annotation vars lexicalDoubleColon (either type_ evalAtype)

-- | A ‘deriving’ clause to declare automatic derivation for a ‘data’ ADT or ‘newtype’ type.
data DerivingBase dclassesList annotation fixpoint =
	DerivingClause annotation dclassesList

-- | A collection of classes to derive an instance for.
--
-- Supports omission of parentheses if there is only one class.
data DclassesListBase either dclass lexicalLeftParenthesis dclasses lexicalRightParenthesis annotation fixpoint =
	  DclassesSingle annotation dclass
		-- ^ The deriving class list form without parentheses, which requires the number of listed classes to be 1.
	| Dclasses       annotation lexicalLeftParenthesis dclasses lexicalRightParenthesis
		-- ^ The general deriving list form.

-- | A list of classes to derive an instance for.
data DclassesBase dclass lexicalComma annotation fixpoint =
	  DclassesEmpty annotation
	| DclassesPush  annotation dclass lexicalComma fixpoint

-- | A class name inside a ‘deriving’ declaration.
data DclassBase qtycls annotation fixpoints =
	DerivingClass annotation qtycls

-- § 4.2.3 Datatype Renamings types.

-- | The constructor of a ‘newtype’ type.
data NewConstrBase con atype lexicalLeftBrace var lexicalDoubleColon type_ lexicalRightBrace annotation fixpoint =
	  BasicNewtypeConstructor  annotation con atype
	| RecordNewtypeConstructor annotation con lexicalLeftBrace var lexicalDoubleColon type_ lexicalRightBrace

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
data SimpleClassesBase simpleClass lexicalComma annotation fixpoint =
	  SimpleClassesEmpty annotation
	| SimpleClassesPush  annotation simpleClass lexicalComma fixpoint

-- § 4.3.1 Instance Declarations types.

-- | The member part of an instance declaration.
--
-- This is what is declared to be an instance of a type class.
--
-- (The context and type class have already been written.  This is what
-- follows up through before the possible ‘where’ clause.)
--data InstBase gtycon tyvars lexicalLeftBracket tyvar lexicalRightBracket lexicalLeftParenthesis lexicalRightArrow lexicalRightParenthesis annotation fixpoint =
data InstBase gtycon lexicalLeftParenthesis tyvars lexicalRightParenthesis typeVariablesTuple lexicalLeftBracket tyvar lexicalRightBracket lexicalRightArrow annotation fixpoint =
	  GeneralTypeConstructorInstance          annotation gtycon
		-- ^ A type or a type constructor (but not in a form where it can be
		-- further applied with type variables after it is referenced here.)
	| AppliableGeneralTypeConstructorInstance annotation lexicalLeftParenthesis gtycon tyvars lexicalRightParenthesis
		-- ^ Like 'GeneralTypeConstructorInstance', but it has parentheses, so it can accommodate syntax for application.
		--
		-- (Note: in the separate semantics phase, the spec says the vars are distinct.)
		-- (In the syntax phase, we specify the structure of the variables.)
	| TypeVariableTupleInstance               annotation typeVariablesTuple
		-- (Note: again, in the separate semantics phase, the spec says the vars are distinct.)
	| ListInstance                            annotation lexicalLeftBracket tyvar lexicalRightBracket
		-- ^ Like 'AppliableGeneralTypeConstructorInstance' but with a sugar
		-- syntax for list type constructor application.
	| FunctionInstance                        annotation lexicalLeftParenthesis tyvar lexicalRightArrow tyvar lexicalRightParenthesis
		-- ^ Like 'AppliableGeneralTypeConstructorInstance' but with a sugar
		-- syntax for function (arrow) type constructor application.
		--
		-- (Note: again, in the separate semantics phase, the spec says the vars are distinct.)

-- | Zero or more type variables.
data TyVarsBase tyvar lexicalComma annotation fixpoint =
	  TyVarsEmpty annotation
	| TyVarsPush  annotation tyvar lexicalComma fixpoint

-- | Two or more type variables, represented as a tuple.
data TypeVariablesTupleBase lexicalLeftParenthesis tyvar typeVariablesTupleRest annotation fixpoint =
	TypeVariablesTupleFromFirst annotation lexicalLeftParenthesis tyvar typeVariablesTupleRest

-- | The rest of two or more type variables, represented as a tuple.
data TypeVariablesTupleRestBase tyvar lexicalRightParenthesis lexicalComma annotation fixpoint =
	  TypeVariablesTupleLast    annotation tyvar lexicalRightParenthesis
	| TypeVariablesTupleNotLast annotation tyvar lexicalComma            fixpoint

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
data FunLhsBase list var apat pat varop lexicalLeftParenthesis lexicalRightParenthesis annotation fixpoint =
	  RegularFunctionClause   annotation var                    apat  (list apat)
	| InfixFunctionClause     annotation pat                    varop pat
	| AppendingFunctionClause annotation lexicalLeftParenthesis apat  lexicalRightParenthesis (list apat)

-- § 3 Expression types.

-- | An expression.
--
-- This is an expression that represents a value or what can be evaluated to
-- a value.
data ExpBase data2 maybe infixexp lexicalDoubleColon context lexicalDoubleRightArrow type_ annotation fixpoint =
	  TypedExpression   annotation infixexp lexicalDoubleColon (maybe (data2 context lexicalDoubleRightArrow)) type_
	| UntypedExpression annotation infixexp

-- | An infixable expression.
--
-- (We check first for right associativity.)
data InfixExpBase lexp qop lexicalMinus annotation fixpoint =
	  RightInfixExpression  annotation lexp qop fixpoint
		-- ^ An expression comprised of a binary operation application.
	| UnaryPrefixExpression annotation lexicalMinus fixpoint
		-- ^ (This is currently just ‘-’.)
	| LeftExpression        annotation lexp
		-- ^ An expression that is not a binary operation application on top.

-- | A left-expression, one not directly a unary or binary operation application.
data LexpBase maybe lexicalLambda apats lexicalRightArrow exp lexicalLet decls lexicalIn lexicalIf lexicalSemicolon lexicalThen lexicalElse lexicalCase lexicalOf lexicalLeftBrace alts lexicalRightBrace stmts annotation fexp fixpoint =
	  LambdaExpression      annotation lexicalLambda    apats lexicalRightArrow        exp
	| LetExpression         annotation lexicalLet       decls lexicalIn                exp
	| ConditionalExpression annotation lexicalIf        exp   (maybe lexicalSemicolon) lexicalThen      exp  (maybe lexicalSemicolon) lexicalElse exp
	| CaseExpression        annotation lexicalCase      exp   lexicalOf                lexicalLeftBrace alts lexicalRightBrace
	| DoExpression          annotation lexicalLeftBrace stmts lexicalRightBrace
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
data AexpBase maybe qvar gcon literal lexicalLeftParenthesis exp lexicalRightParenthesis exps2 exps1 lexicalLeftBracket lexicalDotDot lexicalRightBracket lexicalPipe quals infixexp qop qopSansUnaryMinus qcon fbinds0 aexpSansQcon fbinds1 annotation fixpoint =
	  VariableExpression           annotation qvar
	| ConstructorExpression        annotation gcon
	| LiteralExpression            annotation literal
	| ParenthesesExpression        annotation lexicalLeftParenthesis exp               lexicalRightParenthesis
	| TupleExpression              annotation exps2
	| ListExpression               annotation exps1
	| ArithmeticSequenceExpression annotation lexicalLeftBracket     exp               (maybe exp)             lexicalDotDot            (maybe exp)        lexicalRightBracket
	| ListComprehensionExpression  annotation lexicalLeftBracket     exp               lexicalPipe             quals                   lexicalRightBracket
	| LeftSectionExpression        annotation lexicalLeftParenthesis infixexp          qop                     lexicalRightParenthesis
		-- ^ (The left section form of partial application, with a binary operation.)
	| RightSectionExpression       annotation lexicalLeftParenthesis qopSansUnaryMinus infixexp                lexicalRightParenthesis
		-- ^ (The right section form of partial application, with a binary operation.)
	| ConstructRecordExpression    annotation qcon                   fbinds0
	| ModifyRecordExpression       annotation aexpSansQcon           fbinds1

-- | Two or more expressions in a tuple, including the parentheses.
data Exps2Base lexicalLeftParenthesis exp lexicalComma exps2Rest annotation fixpoint =
	Exps2FromFirst annotation lexicalLeftParenthesis exp lexicalComma exps2Rest

-- | The rest of two or more expressions in a tuple.
--
-- This is after the first has been handled, so we can handle the last and
-- everything in between.  We can apply a different syntax depending on which
-- of these 3 categories the element falls under (first, between, last).
data Exps2RestBase exp lexicalRightParenthesis lexicalComma annotation fixpoint =
	  Exps2Last     annotation exp lexicalRightParenthesis
	| Exps2NotLast  annotation exp lexicalComma            fixpoint

-- | One or more expressions in a list, including the brackets.
data Exps1Base lexicalLeftBracket exp exps1Rest annotation fixpoint =
	Exps1FromFirst annotation lexicalLeftBracket exp exps1Rest

-- | The rest of one or more expressions in a list.
--
-- This is after the first has been handled, so we can handle the last and
-- everything in between.
data Exps1RestBase lexicalRightBracket lexicalComma exp annotation fixpoint =
	  Exps1End  annotation lexicalRightBracket
	| Exps1Push annotation lexicalComma        exp fixpoint

-- | A list comprehension's qualifiers (components after the pipe).
data QualsBase qual lexicalComma annotation fixpoint =
	  QualsLast    annotation qual
	| QualsNotLast annotation qual lexicalComma fixpoint

-- | Zero or more record field bindings, with the brackets.
--
-- This is used in record construction expressions.
data Fbinds0Base lexicalLeftBracket lexicalRightBracket fbind fbinds0Rest annotation fixpoint =
	  Fbinds0Empty     annotation lexicalLeftBracket lexicalRightBracket
	| Fbinds0FromFirst annotation lexicalLeftBracket fbind               fbinds0Rest

-- | The rest of zero or more record field bindings.
data Fbinds0RestBase lexicalRightBracket lexicalComma fbind annotation fixpoint =
	  Fbinds0End  annotation lexicalRightBracket
	| Fbinds0Push annotation lexicalComma        fbind fixpoint

-- | One or more record field bindings, with the brackets.
--
-- This is used in record construction expressions.
data Fbinds1Base lexicalLeftBracket fbind fbinds1Rest annotation fixpoint =
	Fbinds1FromFirst annotation lexicalLeftBracket fbind fbinds1Rest

-- | The rest of one or more record field bindings.
data Fbinds1RestBase lexicalRightBracket lexicalComma fbind annotation fixpoint =
	  Fbinds1End  annotation lexicalRightBracket
	| Fbinds1Push annotation lexicalComma        fbind fixpoint

-- § 3.2 Variables, Constructors, Operators, and Literals types.

-- | A value constructor, extended with a selection of built-in constructors.
data GconBase list lexicalLeftParenthesis lexicalRightParenthesis lexicalLeftBracket lexicalRightBracket lexicalComma qcon annotation fixpoint =
	  UnitConstructor        annotation lexicalLeftParenthesis lexicalRightParenthesis
	| EmptyListConstuctor    annotation lexicalLeftBracket     lexicalRightBracket
	| TupleConstructor       annotation lexicalLeftParenthesis lexicalComma            (list lexicalComma) lexicalRightParenthesis
	| QualifiableConstructor annotation qcon

-- | An unqualified variable, represented as a non-symbolic identifier name or a symbolic variable name.
data VarBase varid lexicalLeftParenthesis varSym lexicalRightParenthesis annotation fixpoint =
	  VariableNonsymbolic annotation varid
		-- ^ (E.g. ‘plus’.)
	| VariableSymbolic    annotation lexicalLeftParenthesis varSym lexicalRightParenthesis
		-- ^ (E.g. ‘(+)’.)

-- | A qualifiable variable, represented as a non-symbolic identifier name or a symbolic variable name.
data QvarBase qvarid lexicalLeftParenthesis qvarSym lexicalRightParenthesis annotation fixpoint =
	  QualifiableVariableNonsymbolic annotation qvarid
		-- ^ (E.g. ‘plus’.)
	| QualifiableVariableSymbolic    annotation lexicalLeftParenthesis qvarSym lexicalRightParenthesis
		-- ^ (E.g. ‘(+)’.)

-- | An unqualified constructor, represented as a non-symbolic identifier name or a symbolic variable name.
--
-- (This is like 'VarBase' but with capitalized syntax.)
data ConBase conid lexicalLeftParenthesis conSym lexicalRightParenthesis annotation fixpoint =
	  ConstructorNonsymbolic annotation conid
		-- ^ (E.g. ‘Plus’.)
	| ConstructorSymbolic    annotation lexicalLeftParenthesis conSym lexicalRightParenthesis
		-- ^ (E.g. ‘(:+)’.)

-- | A qualifiable constructor, represented as a non-symbolic identifier name or a symbolic variable name.
data QconBase qconid lexicalLeftParenthesis gconSym lexicalRightParenthesis annotation fixpoint =
	  QualifiableConstructorNonsymbolic annotation qconid
		-- ^ (E.g. ‘Plus’.)
	| QualifiableConstructorSymbolic    annotation lexicalLeftParenthesis gconSym lexicalRightParenthesis
		-- ^ (E.g. ‘(:+)’.)

-- | An unqualified binary operation, lowercase-style, for non-constructor variables.
data VarOpBase varSym lexicalBacktick varid annotation fixpoint =
	  SymbolicNonconstructorBinaryOperator    annotation varSym
	| NonsymbolicNonconstructorBinaryOperator annotation lexicalBacktick varid lexicalBacktick

-- | A qualifiable binary operation, lowercase-style, for non-constructor variables.
data QvarOpBase qvarSym lexicalBacktick qvarid annotation fixpoint =
	  QualifiableSymbolicNonconstructorBinaryOperator    annotation qvarSym
	| QualifiableNonsymbolicNonconstructorBinaryOperator annotation lexicalBacktick qvarid lexicalBacktick

-- | An unqualified binary operation, capitalized-style, for constructors and constructor variables.
data ConOpBase conSym lexicalBacktick conid annotation fixpoint =
	  SymbolicConstructorBinaryOperator    annotation conSym
	| NonsymbolicConstructorBinaryOperator annotation lexicalBacktick conid lexicalBacktick

-- | A qualifiable binary operation, capitalized-style, for constructors and constructor variables.
data QconOpBase qconSym lexicalBacktick qconid annotation fixpoint =
	  QualifiableSymbolicConstructorBinaryOperator    annotation qconSym
	| QualifiableNonsymbolicConstructorBinaryOperator annotation lexicalBacktick qconid lexicalBacktick

-- | An unqualified binary operation operator.
data OpBase varop conop annotation fixpoint =
	  NonConstructorBinaryOperator annotation varop
	| ConstructorBinaryOperator    annotation conop

-- | A qualifiable binary operation operator.
data QopBase qvarop qconop annotation fixpoint =
	  QualifiableNonConstructorBinaryOperator annotation qvarop
	| QualifiableConstructorBinaryOperator    annotation qconop

-- | A qualifiable symbolic constructor name, extended with a selection of built-in names.
data GconSymBase lexicalColon qcon annotation fixpoint =
	  ConsListConstructor                      annotation lexicalColon
	| NonbuiltinQualifiableConstructorSymbolic annotation qcon

-- § 3.15.2 Construction Using Field Labels types.

-- | A record field value binding.
--
-- Record construction and modification expressions can have these.
data FbindBase qvar lexicalEquals exp annotation fixpoint =
	FieldBinding annotation qvar lexicalEquals exp

-- § 3.17.1 Patterns types.

-- | A pattern, at the top-level, with support for a root-level constructor binary operation.
data PatBase lpat qconop annotation fixpoint =
	  ConstructorBinaryOperationPattern annotation lpat qconop fixpoint
	| LeftPattern                       annotation lpat

-- | Left patterns, embeddable in right binary operations, base patterns with a
-- few extensions like exposed arity-1+ constructor applications and negative
-- number patterns.
data LpatBase either apat lexicalMinus integer float gcon apats annotation fixpoint =
	  BasePattern               annotation apat
	| MinusNumberPattern        annotation lexicalMinus (either integer float)
	| ExposedConstructorPattern annotation gcon         apats

-- | Base pattern.
--
-- (E.g. these can be parameters in a lambda expression.)
data ApatBase maybe var lexicalAt gcon qcon fpats literal lexicalUnderscore lexicalLeftParenthesis pat lexicalRightParenthesis pats2 pats1 lexicalTilde annotation fixpoint =
	  AsPattern          annotation var (maybe lexicalAt fixpoint)
		-- ^ As-pattern: add a name and assign it to a value that matches this
		-- pattern.
	| ConstructorPattern annotation gcon
	| RecordPattern      annotation qcon                   fpats
	| LiteralPattern     annotation literal
	| WildcardPattern    annotation lexicalUnderscore
	| GroupedPattern     annotation lexicalLeftParenthesis pat      lexicalRightParenthesis
	| TuplePattern       annotation pats2
	| ListPattern        annotation pats1
	| IrrefutablePattern annotation lexicalTilde           fixpoint
		-- ^ Lazy pattern matching, with ‘~’.

-- | Zero or more field patterns.
data FpatsBase lexicalLeftBrace lexicalRightBrace fpat fpatsRest annotation fixpoint =
	  FpatsEmpty     annotation lexicalLeftBrace lexicalRightBrace
	| FpatsFromFirst annotation lexicalLeftBrace fpat              fpatsRest

-- | The rest of zero or more field patterns.
data FpatsRestBase lexicalRightBrace lexicalComma fpat annotation fixpoint =
	  FpatsLast    annotation lexicalRightBrace
	| FpatsNotLast annotation lexicalComma      fpat fixpoint

-- | The 2 or more patterns constituting a tuple pattern, with parentheses.
data Pats2Base lexicalLeftParenthesis pat lexicalComma pats2Rest annotation fixpoint =
	Pats2FromFirst annotation lexicalLeftParenthesis pat lexicalComma pats2Rest

-- | The rest of 2 or more patterns constituting a tuple pattern.
data Pats2RestBase pat lexicalRightParenthesis lexicalComma annotation fixpoint =
	  Pats2Last    annotation pat lexicalRightParenthesis
	| Pats2NotLast annotation pat lexicalComma            fixpoint

-- | The 1 or more patterns constituting a list pattern, with brackets.
data Pats1Base lexicalLeftBracket pat pats1Rest annotation fixpoint =
	Pats1FromFirst annotation lexicalLeftBracket pat pats1Rest

-- | The rest of 1 or more patterns constituting a list pattern.
data Pats1RestBase lexicalRightBracket lexicalComma pat annotation fixpoint =
	  Pats1End  annotation lexicalRightBracket
	| Pats1Push annotation lexicalComma        pat fixpoint

-- | A field pattern.
--
-- Deconstruct a field using pattern matching to assign names to evaluable
-- components.
data FpatBase qvar lexicalEquals pat annotation fixpoint =
	FieldPattern annotation qvar lexicalEquals pat

-- § 2.2 Lexical Program Structure types.

-- | The sequence of lexical units constituting a program module.
--
-- It is like a sequence of characters, except the characters are grouped into
-- low-level syntactical units like keywords, contiguous whitespace,
-- identifiers, symbols, and so on.
data ProgramBase list either lexeme whitespace annotation fixpoint =
	LexicalStructure annotation (list (either lexeme whitespace))

-- | A lexeme.
--
-- A lexeme is a low-level syntactical unit token.
data LexemeBase qvarid qconid qvarSym qconSym literal special reservedOp reservedid annotation fixpoint =
	  QvaridLexeme     annotation qvarid
		-- ^ (E.g. ‘TODO’.)
	| QconidLexeme     annotation qconid
		-- ^ (E.g. ‘TODO’.)
	| QvarSymLexeme    annotation qvarSym
		-- ^ (E.g. ‘TODO’.)
	| QconSymLexeme    annotation qconSym
		-- ^ (E.g. ‘TODO’.)
	| LiteralLexeme    annotation literal
		-- ^ (E.g. ‘42’.)
	| SpecialLexeme    annotation special
		-- ^ (E.g. ‘TODO’.)
	| ReservedOpLexeme annotation reservedOp
		-- ^ (E.g. ‘TODO’.)
	| ReservedidLexeme annotation reservedid
		-- ^ (E.g. ‘TODO’.)

-- | A written literal, like ‘87’, ‘'C'’, etc.
data LiteralBase integer float char string annotation fixpoint =
	  IntegerLiteral annotation integer
	| FloatLiteral   annotation float
	| CharLiteral    annotation char
	| StringLiteral  annotation string

-- | A set of symbolic characters.
--
-- ('symbol' excludes these.)
data SpecialBase lexicalLeftParenthesis lexicalRightParenthesis lexicalComma lexicalSemicolon lexicalLeftBracket lexicalRightBracket lexicalBacktick lexicalLeftBrace lexicalRightBrace annotation fixpoint =
	  LeftParenthesisSpecial  annotation lexicalLeftParenthesis
	| RightParenthesisSpecial annotation lexicalRightParenthesis
	| CommaSpecial            annotation lexicalComma
	| SemicolonSpecial        annotation lexicalSemicolon
	| LeftBracketSpecial      annotation lexicalLeftBracket
	| RightBracketSpecial     annotation lexicalRightBracket
	| BacktickSpecial         annotation lexicalBacktick
	| LeftBraceSpecial        annotation lexicalLeftBrace
	| RightBraceSpecial       annotation lexicalRightBrace

-- | A set of symbolic characters, excluding those that can be part of any
-- special ‘nc’ (multiline-style) comment construct.
--
-- That is, it excludes, ‘{’ and ‘}’ (and would exclude ‘-’).
data SpecialSansNcBase lexicalLeftParenthesis lexicalRightParenthesis lexicalComma lexicalSemicolon lexicalLeftBracket lexicalRightBracket lexicalBacktick annotation fixpoint =
	  LeftParenthesisSpecialSansNc  annotation lexicalLeftParenthesis
	| RightParenthesisSpecialSansNc annotation lexicalRightParenthesis
	| CommaSpecialSansNc            annotation lexicalComma
	| SemicolonSpecialSansNc        annotation lexicalSemicolon
	| LeftBracketSpecialSansNc      annotation lexicalLeftBracket
	| RightBracketSpecialSansNc     annotation lexicalRightBracket
	| BacktickSpecialSansNc         annotation lexicalBacktick


-- | Contiguous whitespace, comprising one or more whitestuffs.
data WhitespaceBase list whitestuff annotation fixpoint =
	WholeWhitespace annotation whitestuff (list whitestuff)

-- | A whitespace unit: a whitespace character or comment.
data WhitestuffBase whitechar comment ncomment annotation fixpoint =
	  WhitecharWhitestuff annotation whitechar
	| CommentWhitestuff   annotation comment
	| MultilinableComment annotation ncomment

-- | A whitespace character: a newline, space, tab, vertical tab, or unicode
-- whitespace character.
data WhitecharBase newline vertab space tab uniWhite annotation fixpoint =
	  NewlineWhitechar     annotation newline
	| VerticalTabWhitechar annotation vertab
	| SpaceWhitechar       annotation space
	| TabWhitechar         annotation tab
	| UnicodeWhitechar     annotation uniWhite

-- | A newline character or unit character sequence, as some newlines are represented with more than one
-- character.
data NewlineBase return linefeed formfeed annotation fixpoint =
	  WindowsNewline  annotation return linefeed
		-- ^ (‘\r\n’ (CR LF).)
	| OldMacNewline   annotation return
		-- ^ (‘\r’ (CR).)
	| UnixNewline     annotation linefeed
		-- ^ (‘\n’ (LF).)
	| FormfeedNewline annotation formfeed
		-- ^ (‘\f’ (FF).)

-- | Carriage-return, ‘\r’, CR.
data ReturnBase lexicalCR annotation fixpoint =
	CRReturn annotation lexicalCR

-- | Line-feed, ‘\n’, LF.
data LinefeedBase lexicalLF annotation fixpoint =
	LFLinefeed annotation lexicalLF

-- | A vertical tab ASCII control character.
--
-- Vertical-tab, ‘\v’, VT.
data VertabBase lexicalVT annotation fixpoint =
	VTVertab annotation lexicalVT

-- | A form-feed ASCII control character.
--
-- Form-feed, ‘\f’, FF.
data FormfeedBase lexicalFF annotation fixpoint =
	FFFormfeed annotation lexicalFF

-- | A space character.
data SpaceBase lexicalSP annotation fixpoint =
	SPSpace annotation lexicalSP

-- | A tab character.
--
-- Tab, ‘\t’, HT.
data TabBase lexicalHT annotation fixpoint =
	HTTab annotation lexicalHT

-- | A unicode whitespace character.
data UniWhiteBase lexicalUnicodeWhitespaceChar annotation fixpoint =
	UnicodeWhitespaceChar annotation lexicalUnicodeWhitespaceChar


-- | A single-line style comment.
--
-- This form starts with 2 dashes not in a further symbol.
data CommentBase data2 maybe list dashes anySansSymbol any newline annotation fixpoint =
	SinglelineComment annotation dashes (maybe (data2 anySansSymbol (list any))) newline

-- | The dashes that can begin a single-line style comment.
data DashesBase list lexicalHyphen annotation fixpoint =
	SinglelineCommentDashes annotation lexicalHyphen lexicalHyphen (list lexicalHyphen)

-- | The opening of a multi-line style comment, ‘{-’.
data OpenComBase lexicalLeftBrace lexicalHyphen annotation fixpoint =
	MultilineCommentOpening annotation lexicalLeftBrace lexicalHyphen

-- | The closing of a multi-line style comment, ‘-}’.
data CloseComBase lexicalHyphen lexicalRightBrace annotation fixpoint =
	MultilineCommentClosing annotation lexicalHyphen lexicalRightBrace

-- | A multi-line style comment that can span multiple lines.
data NcommentBase data2 list openCom bigAnySeq closeCom annotation fixpoint =
	MultilineComment annotation openCom bigAnySeq (list (data2 fixpoint bigAnySeq)) closeCom

-- | Zero or more ANY ('BigAnyBase') characters excluding multi-line style
-- comment openings and closings.
--
-- A graphical character without a multi-line comment opening or closing.
--
-- This is specially crafted like a regex exclusion, with its complications.
data BigAnySeqBase bigAnySansNc lexicalRightBrace lexicalLeftBrace bigAnySeqValidNcomChar1_0 lexicalHyphen bigAnySeqValidNcomChar1_1 annotation fixpoint =
	  EmptyBigAnySeqBase annotation
		-- ^ We've reached the end of an ANY sequence.
	| NotNcomChar0BigAnySeqBase       annotation bigAnySansNc      fixpoint
		-- ^ The next character cannot begin either of the 2 excluded sequences
		-- since it does not have an ‘nc’ character.
	| ValidNcomChar0BigAnySeqBase     annotation lexicalRightBrace fixpoint
		-- ^ The next character cannot begin either of the 2 excluded sequences,
		-- even though it is an ‘nc’ character.
	| ValidNcomChar0_0BigAnySeqBase   annotation lexicalLeftBrace  bigAnySeqValidNcomChar1_0
		-- ^ The next character can begin the first of 2 excluded sequences,
		-- but what follows does not complete the first of 1 excluded sequences.
	| ValidNcomChar0_1BigAnySeqBase   annotation lexicalHyphen     bigAnySeqValidNcomChar1_1
		-- ^ The next character can begin the second of 2 excluded sequences,
		-- but what follows does not complete the second of 1 excluded sequences.

-- | An ANY sequence from the second character of a subsequence of characters
-- that has a prefix that could begin an excluded ‘nc’ (multi-line style
-- comment) sequence but does not complete it, for the first of 2 excluded
-- sequences).
--
-- Informally, more simply, we parsed ‘{’, so make sure there's not a ‘-’.
data BigAnySeqValidNcomChar1_0Base bigAnySansNc lexicalLeftBrace lexicalRightBrace lexicalEndOfParse annotation fixpoint =
	  NotNcomChar1_0    annotation bigAnySansNc
		-- ^ The next character is not an ‘nc’ character, so it can't be ‘-’.
	| LeftBraceChar1_0  annotation lexicalLeftBrace fixpoint
		-- ^ It's not ‘-’, but it's ‘nc’ so we might not be able to reset to
		-- ‘BigAnySeqBase’.
	| RightBraceChar1_0 annotation lexicalRightBrace
		-- ^ It's not ‘-’.
	| EOPChar1_0        annotation lexicalEndOfParse
		-- ^ We're provided with a certification that no more characters will
		-- follow in whatever we're parsing, so since we thus know this is the
		-- last ‘{’, this is not a ‘{’ in an ‘nc’ construct, since it's not
		-- part of ‘{-’.

-- | An ANY sequence from the second character of a subsequence of characters
-- that has a prefix that could begin an excluded ‘nc’ (multi-line style
-- comment) sequence but does not complete it, for the second of 2 excluded
-- sequences.
--
-- More simply, we parsed ‘-’, so make sure there's not a ‘}’.
data BigAnySeqValidNcomChar1_1Base bigAnySansNc lexicalLeftBrace bigAnySeqValidNconmChar1_0Base lexicalHyphen lexicalEndOfParse annotation fixpoint =
	  NotNcomChar1_1   annotation bigAnySansNc
		-- ^ The next character is not an ‘nc’ character, so it can't be ‘}’.
	| LeftBraceChar1_1 annotation lexicalLeftBrace bigAnySeqValidNconmChar1_0Base
		-- ^ It's not ‘-’, but it's ‘nc’ so we might not be able to reset to
		-- ‘BigAnySeqBase’.
	| HyphenChar1_1    annotation lexicalHyphen fixpoint
		-- ^ It's not ‘-’, but it's ‘nc’ so we might not be able to reset to
		-- ‘BigAnySeqBase’.
	| EOFChar1_1       annotation lexicalEndOfParse
		-- ^ We're provided with a certification that no more characters will
		-- follow in whatever we're parsing, so since we thus know this is the
		-- last ‘-’, this is not a ‘-’ in an ‘nc’ construct, since it's not
		-- part of ‘-}’.

-- | A graphical or whitespace character.
data BigAnyBase graphic whitechar annotation fixpoint =
	  GraphicBigAny   annotation graphic
	| WhitecharBigAny annotation whitechar

-- | A graphical or whitespace character, except for the beginning of ‘{-’ and
-- ‘-}’, and except for the end of ‘{-’ and ‘-}’: that is, it excludes the 3
-- characters ‘{’, ‘-’, and ‘}’.
data BigAnySansNcBase graphicSansNc whitechar annotation fixpoint =
	  GraphicBigAnySansNc   annotation graphicSansNc
	| WhitecharBigAnySansNc annotation whitechar

-- | A graphical character or a regular, in-line space character.
data AnyBase graphic space tab annotation fixpoint =
	  GraphicAny annotation graphic
	| SpaceAny   annotation space
	| TabAny     annotation tab

-- | A graphical character.
--
-- This is a printable character as opposed to a space character.
data GraphicBase small large symbol digit special lexicalDoubleQuote lexicalSingleQuote annotation fixpoint =
	  SmallGraphic       annotation small
	| LargeGraphic       annotation large
	| SymbolGraphic      annotation symbol
	| DigitGraphic       annotation digit
	| SpecialGraphic     annotation special
	| DoubleQuoteGraphic annotation lexicalDoubleQuote
	| SingleQuoteGraphic annotation lexicalSingleQuote

-- | A graphical character that cannot be part of any special ‘nc’
-- (multiline-style) comment construct.
--
-- That is, it is 'GraphicBase' with 3 characters excluded: ‘{’, ‘-’, and ‘}’.
data GraphicSansNcBase small large symbolSansNc digit specialSansNc lexicalDoubleQuote lexicalSingleQuote annotation fixpoint =
	  SmallGraphicSansNc       annotation small
	| LargeGraphicSansNc       annotation large
	| SymbolGraphicSansNc      annotation symbolSansNc
	| DigitGraphicSansNc       annotation digit
	| SpecialGraphicSansNc     annotation specialSansNc
	| DoubleQuoteGraphicSansNc annotation lexicalDoubleQuote
	| SingleQuoteGraphicSansNc annotation lexicalSingleQuote


-- | A lowercase character.
data SmallBase ascSmall uniSmall lexicalUnderscore annotation fixpoint =
	  AsciiSmall      annotation ascSmall
	| UnicodeSmall    annotation uniSmall
	| UnderscoreSmall annotation lexicalUnderscore

-- | An ASCII lowercase character.
data AscSmallBase lexicalALower lexicalBLower lexicalCLower lexicalDLower lexicalELower lexicalFLower lexicalGLower lexicalHLower lexicalILower lexicalJLower lexicalKLower lexicalLLower lexicalMLower lexicalNLower lexicalOLower lexicalPLower lexicalQLower lexicalRLower lexicalSLower lexicalTLower lexicalULower lexicalVLower lexicalWLower lexicalXLower lexicalYLower lexicalZLower annotation fixpoint =
	  AsciiSmallA annotation lexicalALower
	| AsciiSmallB annotation lexicalBLower
	| AsciiSmallC annotation lexicalCLower
	| AsciiSmallD annotation lexicalDLower
	| AsciiSmallE annotation lexicalELower
	| AsciiSmallF annotation lexicalFLower
	| AsciiSmallG annotation lexicalGLower
	| AsciiSmallH annotation lexicalHLower
	| AsciiSmallI annotation lexicalILower
	| AsciiSmallJ annotation lexicalJLower
	| AsciiSmallK annotation lexicalKLower
	| AsciiSmallL annotation lexicalLLower
	| AsciiSmallM annotation lexicalMLower
	| AsciiSmallN annotation lexicalNLower
	| AsciiSmallO annotation lexicalOLower
	| AsciiSmallP annotation lexicalPLower
	| AsciiSmallQ annotation lexicalQLower
	| AsciiSmallR annotation lexicalRLower
	| AsciiSmallS annotation lexicalSLower
	| AsciiSmallT annotation lexicalTLower
	| AsciiSmallU annotation lexicalULower
	| AsciiSmallV annotation lexicalVLower
	| AsciiSmallW annotation lexicalWLower
	| AsciiSmallX annotation lexicalXLower
	| AsciiSmallY annotation lexicalYLower
	| AsciiSmallZ annotation lexicalZLower

-- | A unicode lowercase character.
data UniSmallBase lexicalUnicodeSmall annotation fixpoint =
	UnicodeSmallUniSmall annotation lexicalUnicodeSmall


-- | An uppercase character.
data LargeBase ascLarge uniLarge annotation fixpoint =
	  AsciiLarge   annotation ascLarge
	| UnicodeLarge annotation uniLarge

-- | An ASCII uppercase character.
data AscLargeBase lexicalA lexicalB lexicalC lexicalD lexicalE lexicalF lexicalG lexicalH lexicalI lexicalJ lexicalK lexicalL lexicalM lexicalN lexicalO lexicalP lexicalQ lexicalR lexicalS lexicalT lexicalU lexicalV lexicalW lexicalX lexicalY lexicalZ annotation fixpoint =
	  AsciiLargeA annotation lexicalA
	| AsciiLargeB annotation lexicalB
	| AsciiLargeC annotation lexicalC
	| AsciiLargeD annotation lexicalD
	| AsciiLargeE annotation lexicalE
	| AsciiLargeF annotation lexicalF
	| AsciiLargeG annotation lexicalG
	| AsciiLargeH annotation lexicalH
	| AsciiLargeI annotation lexicalI
	| AsciiLargeJ annotation lexicalJ
	| AsciiLargeK annotation lexicalK
	| AsciiLargeL annotation lexicalL
	| AsciiLargeM annotation lexicalM
	| AsciiLargeN annotation lexicalN
	| AsciiLargeO annotation lexicalO
	| AsciiLargeP annotation lexicalP
	| AsciiLargeQ annotation lexicalQ
	| AsciiLargeR annotation lexicalR
	| AsciiLargeS annotation lexicalS
	| AsciiLargeT annotation lexicalT
	| AsciiLargeU annotation lexicalU
	| AsciiLargeV annotation lexicalV
	| AsciiLargeW annotation lexicalW
	| AsciiLargeX annotation lexicalX
	| AsciiLargeY annotation lexicalY
	| AsciiLargeZ annotation lexicalZ

-- | A unicode uppercase or titlecase character.
data UniLargeBase lexicalUnicodeLarge annotation fixpoint =
	UnicodeLargeUniLarge annotation lexicalUnicodeLarge

-- | Symbol characters, excluding special symbols, found in 'SpecialBase'.
data SymbolBase ascSymbol uniSymbolSansSpecialish annotation fixpoint =
	  AsciiNonspecialSymbol                annotation ascSymbol
	| UnicodeNonspecialNonscorequoteSymbol annotation uniSymbolSansSpecialish

-- | Symbol characters, excluding special symbols and any character that can be
-- part of an ‘nc’ (multi-line style comment) construct.
--
-- That is, it also excludes ‘-’ (and it would exclude ‘{’ and ‘}’).
data SymbolSansNcBase ascSymbolSansNc uniSymbolSansSpecialishSansNc annotation fixpoint =
	  AsciiNonspecialSymbolSansNc                annotation ascSymbolSansNc
	| UnicodeNonspecialNonscorequoteSymbolSansNc annotation uniSymbolSansSpecialishSansNc


-- | An ASCII symbol except special characters, underscore, and quote characters.
data AscSymbolBase lexicalExclamation lexicalHash lexicalDollar lexicalPercent lexicalAmpersand lexicalAsterisk lexicalPlus lexicalDot lexicalSlash lexicalLeftAngleBracket lexicalEquals lexicalRightAngleBracket lexicalQuestionmark lexicalAt lexicalBackslash lexicalCaret lexicalPipe lexicalHyphen lexicalTilde lexicalColon annotation fixpoint =
	  ExclamationAsciiSymbol       annotation lexicalExclamation
	| HashAsciiSymbol              annotation lexicalHash
	| DollarAsciiSymbol            annotation lexicalDollar
	| PercentAsciiSymbol           annotation lexicalPercent
	| AmpersandAsciiSymbol         annotation lexicalAmpersand
	| AsteriskAsciiSymbol          annotation lexicalAsterisk
	| PlusAsciiSymbol              annotation lexicalPlus
	| DotAsciiSymbol               annotation lexicalDot
	| SlashAsciiSymbol             annotation lexicalSlash
	| LeftAngleBracketAsciiSymbol  annotation lexicalLeftAngleBracket
	| EqualsAsciiSymbol            annotation lexicalEquals
	| RightAngleBracketAsciiSymbol annotation lexicalRightAngleBracket
	| QuestionmarkAsciiSymbol      annotation lexicalQuestionmark
	| AtAsciiSymbol                annotation lexicalAt
	| BackslashAsciiSymbol         annotation lexicalBackslash
	| CaretAsciiSymbol             annotation lexicalCaret
	| PipeAsciiSymbol              annotation lexicalPipe
	| HyphenAsciiSymbol            annotation lexicalHyphen
	| TildeAsciiSymbol             annotation lexicalTilde
	| ColonAsciiSymbol             annotation lexicalColon

-- | An ASCII symbol except special characters, underscore, and quote
-- characters; except ‘nc’ (multiline-style comment) chars.
--
-- That is, it also excludes ‘-’ (and it would exclude ‘{’ and ‘}’).
data AscSymbolSansNcBase lexicalExclamation lexicalHash lexicalDollar lexicalPercent lexicalAmpersand lexicalAsterisk lexicalPlus lexicalDot lexicalSlash lexicalLeftAngleBracket lexicalEquals lexicalRightAngleBracket lexicalQuestionmark lexicalAt lexicalBackslash lexicalCaret lexicalPipe lexicalTilde lexicalColon annotation fixpoint =
	  ExclamationAsciiSymbolSansNc       annotation lexicalExclamation
	| HashAsciiSymbolSansNc              annotation lexicalHash
	| DollarAsciiSymbolSansNc            annotation lexicalDollar
	| PercentAsciiSymbolSansNc           annotation lexicalPercent
	| AmpersandAsciiSymbolSansNc         annotation lexicalAmpersand
	| AsteriskAsciiSymbolSansNc          annotation lexicalAsterisk
	| PlusAsciiSymbolSansNc              annotation lexicalPlus
	| DotAsciiSymbolSansNc               annotation lexicalDot
	| SlashAsciiSymbolSansNc             annotation lexicalSlash
	| LeftAngleBracketAsciiSymbolSansNc  annotation lexicalLeftAngleBracket
	| EqualsAsciiSymbolSansNc            annotation lexicalEquals
	| RightAngleBracketAsciiSymbolSansNc annotation lexicalRightAngleBracket
	| QuestionmarkAsciiSymbolSansNc      annotation lexicalQuestionmark
	| AtAsciiSymbolSansNc                annotation lexicalAt
	| BackslashAsciiSymbolSansNc         annotation lexicalBackslash
	| CaretAsciiSymbolSansNc             annotation lexicalCaret
	| PipeAsciiSymbolSansNc              annotation lexicalPipe
	| TildeAsciiSymbolSansNc             annotation lexicalTilde
	| ColonAsciiSymbolSansNc             annotation lexicalColon

-- | A Unicode symbol or punctuation character.
data UniSymbolBase lexicalUnicodeSymbol annotation fixpoint =
	UnicodeSymbol annotation lexicalUnicodeSymbol

-- | A Unicode symbol except for those found in 'special', the underscore, and
-- the double and single quote characters.
data UniSymbolSansSpecialishBase lexicalUnicodeSymbolSansSpecialUnderscoreDoubleQuoteSingleQuote annotation fixpoint =
	UnicodeSymbolSansSpecialish annotation lexicalUnicodeSymbolSansSpecialUnderscoreDoubleQuoteSingleQuote

-- | A Unicode symbol except for those found in 'special', the underscore, and
-- the double and single quote characters.
data UniSymbolSansSpecialishSansNcBase lexicalUnicodeSymbolSansSpecialUnderscoreDoubleQuoteSingleQuote annotation fixpoint =
	UnicodeSymbolSansSpecialishSansNc annotation lexicalUnicodeSymbolSansSpecialUnderscoreDoubleQuoteSingleQuote

-- | A digit character.
data DigitBase ascDigit uniDigit annotation fixpoint =
	  AsciiDigit   annotation ascDigit
	| UnicodeDigit annotation uniDigit

-- | An ASCII digit character.
data AscDigitBase lexical0 lexical1 lexical2 lexical3 lexical4 lexical5 lexical6 lexical7 lexical8 lexical9 annotation fixpoint =
	  N0AsciiDigit annotation lexical0
	| N1AsciiDigit annotation lexical1
	| N2AsciiDigit annotation lexical2
	| N3AsciiDigit annotation lexical3
	| N4AsciiDigit annotation lexical4
	| N5AsciiDigit annotation lexical5
	| N6AsciiDigit annotation lexical6
	| N7AsciiDigit annotation lexical7
	| N8AsciiDigit annotation lexical8
	| N9AsciiDigit annotation lexical9

-- | A unicode digit character.
data UniDigitBase lexicalUnicodeDigit annotation fixpoint =
	UnicodeDigitUniDigit annotation lexicalUnicodeDigit

-- | An octal digit.
--
-- 0…7.
data OctitBase lexical0 lexical1 lexical2 lexical3 lexical4 lexical5 lexical6 lexical7 annotation fixpoint =
	  N0Octit annotation lexical0
	| N1Octit annotation lexical1
	| N2Octit annotation lexical2
	| N3Octit annotation lexical3
	| N4Octit annotation lexical4
	| N5Octit annotation lexical5
	| N6Octit annotation lexical6
	| N7Octit annotation lexical7

-- | A hexaadecimal digit.
--
-- 0…F, and alias characters a…f.
data HexitBase digit lexicalA lexicalB lexicalC lexicalD lexicalE lexicalF lexicalALower lexicalBLower lexicalCLower lexicalDLower lexicalELower lexicalFLower annotation fixpoint =
	  DigitHexit  annotation digit
	| AHexit      annotation lexicalA
	| BHexit      annotation lexicalB
	| CHexit      annotation lexicalC
	| DHexit      annotation lexicalD
	| EHexit      annotation lexicalE
	| FHexit      annotation lexicalF
	| ALowerHexit annotation lexicalALower
	| BLowerHexit annotation lexicalBLower
	| CLowerHexit annotation lexicalCLower
	| DLowerHexit annotation lexicalDLower
	| ELowerHexit annotation lexicalELower
	| FLowerHexit annotation lexicalFLower

-- § 2.4 Identifiers and Operators types.

-- | An unqualified name.
data UnqualifiedNameBase varid conid tyvar tycon tycls modid annotation fixpoint =
	  UnqualifiedVaridName annotation varid
	| UnqualifiedConidName annotation conid
	| UnqualifiedTyvarName annotation tyvar
	| UnqualifiedTyconName annotation tycon
	| UnqualifiedTyclsName annotation tycls
	| UnqualifiedModidName annotation modid

-- This little part of 2.4 is less simple because some values are excluded.
-- Basically, we just create a structure where the possible exclusions changes,
-- for ambiguity, and we also add a simple, similar structure that does not
-- have exclusions, at least for reference.

-- (Note: 'VaridBase', with exclusions, appears later in this section.)

-- | For reference, a simplified ‘varid’ structure that lacks exclusions.
data VaridNoExclusionsBase list varidStart varidInner annotation fixpoint =
	VaridNoExclusionsWhole annotation varidStart (list varidInner)
		-- ^ The whole part of the 'varid': it includes the starting character
		-- and the zero or more characters afterward.

-- | A valid start to a 'varid'.
data VaridStartBase small annotation fixpoint =
	SmallVaridStart annotation small

-- | A character valid in either a variable (lowercase-style) or constructor
-- (capitalized-style) non-symbolic identifier.
data IdentifierInnerBase small large digit lexicalSingleQuote annotation fixpoint =
	  SmallIdentifierInner       annotation small
	| LargeIdentifierInner       annotation large
	| DigitIdentifierInner       annotation digit
	| SingleQuoteIdentifierInner annotation lexicalSingleQuote

-- | A character valid in a variable (lowercase-style) non-symbolic identifier
-- after its start.
data VaridInnerBase identifierInner annotation fixpoint =
	IdInnerVaridInner annotation identifierInner

-- | A non-symbolic constructor (capitalized-style) identifier name.
data ConidBase list conidStart conidInner annotation fixpoint =
	ConidWhole annotation conidStart (list conidInner)

-- | A valid start to a 'conid'.
data ConidStartBase large annotation fixpoint =
	LargeConidStart annotation large

-- | A character valid in a variable (capitalized-style) non-symbolic identifier
-- after its start.
data ConidInnerBase identifierInner annotation fixpoint =
	IdInnerConidInner annotation identifierInner

-- | A reserved keyword.  Haskell2010 excludes these from 'varid' at the syntax level.
data ReservedidBase lexicalCase lexicalClass lexicalData lexicalDefault lexicalDeriving lexicalDo lexicalElse lexicalForeign lexicalIf lexicalImport lexicalIn lexicalInfix lexicalInfixl lexicalInfixr lexicalInstance lexicalLet lexicalModule lexicalNewtype lexicalOf lexicalThen lexicalType lexicalWhere lexicalUnderscore annotation fixpoint =
	  CaseReservedId       annotation lexicalCase
	| ClassReservedId      annotation lexicalClass
	| DataReservedId       annotation lexicalData
	| DefaultReservedId    annotation lexicalDefault
	| DerivingReservedId   annotation lexicalDeriving
	| DoReservedId         annotation lexicalDo
	| ElseReservedId       annotation lexicalElse
	| ForeignReservedId    annotation lexicalForeign
	| IfReservedId         annotation lexicalIf
	| ImportReservedId     annotation lexicalImport
	| InReservedId         annotation lexicalIn
	| InfixReservedId      annotation lexicalInfix
	| InfixlReservedId     annotation lexicalInfixl
	| InfixrReservedId     annotation lexicalInfixr
	| InstanceReservedId   annotation lexicalInstance
	| LetReservedId        annotation lexicalLet
	| ModuleReservedId     annotation lexicalModule
	| NewtypeReservedId    annotation lexicalNewtype
	| OfReservedId         annotation lexicalOf
	| ThenReservedId       annotation lexicalThen
	| TypeReservedId       annotation lexicalType
	| WhereReservedId      annotation lexicalWhere
	| UnderscoreReservedId annotation lexicalUnderscore

-- (Note: 'VarsymBase', with extra exclusions, appears later in this section.)

-- | A symbolic variable (lowercase-style) identifier name in a simplified version without extra exclusions.
data VarsymNoExtraExclusionsBase list varsymStart symbol annotation fixpoint =
	VarsymWhole annotation varsymStart (list symbol)

-- | A valid start to a symbolic variable (lowercase-style) identifier name.
data VarsymStartBase symbolSansColon annotation fixpoint =
	SymbolSansColonVarsymStart annotation symbolSansColon

-- (Note: 'ConsymBase', with extra exclusions, appears later in this section.)

-- | A symbolic constructor (capitalized-style) identifier name in a simplified version without extra exclusions.
data ConsymNoExtraExclusionsBase list consymStart symbol annotation fixpoint =
	ConsymWhole annotation consymStart (list symbol)

-- | A valid start to a symbolic constructor (capitalized-style) identifier
-- name.
data ConsymStartBase lexicalColon annotation fixpoint =
	ColonConsymStart annotation lexicalColon

-- | Reversed symbolic names.
--
-- Haskell2010 excludes these from 'Consym's and these and 'dashes' from 'Varsym's.
data ReservedOpBase lexicalDotDot lexicalColon lexicalDoubleColon lexicalEquals lexicalBackslash lexicalPipe lexicalLeftArrow lexicalRightArrow lexicalAt lexicalTilde lexicalDoubleRightArrow annotation fixpoint =
	  DotDotReservedOp           annotation lexicalDotDot
	| ColonReservedOp            annotation lexicalColon
	| DoubleColonReservedOp      annotation lexicalDoubleColon
	| EqualsReservedOp           annotation lexicalEquals
	| BackslashReservedOp        annotation lexicalBackslash
	| PipeReservedOp             annotation lexicalPipe
	| LeftArrowReservedOp        annotation lexicalLeftArrow
	| RightArrowReservedOp       annotation lexicalRightArrow
	| AtReservedOp               annotation lexicalAt
	| TildeReservedOp            annotation lexicalTilde
	| DoubleRightArrowReservedOp annotation lexicalDoubleRightArrow

-- | A type variable name.
data TyvarBase conid annotation fixpoint =
	TypeVariableName annotation conid

-- | A type constructor name.
data TyconBase varid annotation fixpoint =
	TypeConstructorName annotation varid

-- | A type class name.
data TyclsBase conid annotation fixpoint =
	TypeClassName annotation conid

-- | A module name.
data ModidBase data2 list conid lexicalDot annotation fixpoint =
	ModuleName annotation (list (data2 conid lexicalDot)) conid

-- | A qualifiable name.
data NameBase qvarid qconid qtycon qtycls qvarsym qconsym annotation fixpoint =
	  VaridName  annotation qvarid
	| ConidName  annotation qconid
	| TyconName  annotation qtycon
	| TyclsName  annotation qtycls
	| VarSymName annotation qvarsym
	| ConSymName annotation qconsym

-- | A qualifiable lowercase-style, non-symbolic variable name.
data QvaridBase data2 maybe modid lexicalDot varid annotation fixpoint =
	QualifiableVarid annotation (maybe (data2 modid lexicalDot)) varid

-- | A qualifiable lowercase-style, non-symbolic variable name.
data QconidBase data2 maybe modid lexicalDot conid annotation fixpoint =
	QualifiableConid annotation (maybe (data2 modid lexicalDot)) conid

-- | A qualifiable lowercase-style, non-symbolic variable name.
data QtyconBase data2 maybe modid lexicalDot tycon annotation fixpoint =
	QualifiableTycon annotation (maybe (data2 modid lexicalDot)) tycon

-- | A qualifiable lowercase-style, non-symbolic variable name.
data QtyclsBase data2 maybe modid lexicalDot tycls annotation fixpoint =
	QualifiableTycls annotation (maybe (data2 modid lexicalDot)) tycls

-- | A qualifiable lowercase-style, non-symbolic variable name.
data QvarSymBase data2 maybe modid lexicalDot varSym annotation fixpoint =
	QualifiableVarSym annotation (maybe (data2 modid lexicalDot)) varSym

-- | A qualifiable lowercase-style, non-symbolic variable name.
data QconSymBase data2 maybe modid lexicalDot conSym annotation fixpoint =
	QualifiableConSym annotation (maybe (data2 modid lexicalDot)) conSym

-- TODO:
{-
-- (Exclusion structures.)

-- | A non-symbolic variable (lowercase-style) identifier name, excluding
-- reserved names.
--
-- The structures are set up specially to exclude reserved names.
data VaridBase annotation fixpoint =
	TODO annotation 

-- | A symbolic variable (lowercase-style) identifier name, excluding reserved
-- names and dash-only sequences.
data VarsymBase annotation fixpoint =
	TODO annotation 

-- | A symbolic constructor (capitalized-style) identifier name, excluding
-- reserved names.
data ConsymBase annotation fixpoint =
	TODO annotation 
-}

-- § 2.5 Numeric Literals types.

-- | A decimal literal, of at least 1 digit.
data DecimalBase list digit annotation fixpoint =
	DecimalLiteral annotation digit (list digit)

-- | An octal literal, of at least 1 octal digit.
data OctalBase list octit annotation fixpoint =
	OctalLiteral annotation octit (list octit)

-- | A hexadecimal literal, of at least 1 hexadecimal digit.
data HexadecimalBase list hexit annotation fixpoint =
	HexadecimalLiteral annotation hexit (list hexit)

-- | An integer literal, with a few supported base prefixes.
data IntegerBase decimal lexical0o octal lexical0O lexical0x hexadecimal lexical0X annotation fixpoint =
	  DecimalInteger            annotation decimal
	| OctalInteger              annotation lexical0o octal
	| CapitalOctalInteger       annotation lexical0O octal
	| HexadecimalInteger        annotation lexical0x hexadecimal
	| CapitalHexadecimalInteger annotation lexical0X hexadecimal

-- | A floating point number in either point or exponent-only form.
data FloatBase maybe decimal lexicalDot exponent annotation fixpoint =
	  PointFloatLiteral    annotation decimal lexicalDot decimal (maybe exponent)
	| ExponentFloatLiteral annotation decimal exponent

-- | A floating point literal's exponent part.
data ExponentBase either maybe lexicalELower lexicalE lexicalPlus lexicalMinus decimal annotation fixpoint =
	FloatExponent annotation (either lexicalELower lexicalE) (maybe (either lexicalPlus lexicalMinus)) decimal

-- § 2.6 Character and String Literals types.

-- | A character literal, written in Haskell as e.g. ‘'x'’.
data CharBase lexicalSingleQuote charLiteralInner annotation fixpoint =
	CharLiteralChar annotation lexicalSingleQuote charLiteralInner lexicalSingleQuote

-- | The part of the characteral around its prefix and suffix, its opening and
-- closing single quote.
data CharLiteralInnerBase graphicSansSingleQuoteOrBackslash space escapeSansBackslashAndAmpersand annotation fixpoint =
	  CharLiteralInnerGraphic annotation graphicSansSingleQuoteOrBackslash
	| CharLiteralInnerSpace   annotation space
	| CharLiteralInnerEscape  annotation escapeSansBackslashAndAmpersand

-- | A string literal, written in Haskell2010 as the string contents surrounded by
-- double quotes.
data StringBase list lexicalDoubleQuote stringLiteralInnerUnit annotation fixpoint =
	StringLiteralString annotation lexicalDoubleQuote (list stringLiteralInnerUnit) lexicalDoubleQuote

-- | A unit inside a string literal: a character, escape, or gap.
data StringLiteralInnerUnitBase graphicSansDoubleQuoteOrBackslash space escape gap annotation fixpoint =
	  StringLiteralInnerGraphic annotation graphicSansDoubleQuoteOrBackslash
	| StringLiteralInnerSpace   annotation space
	| StringLiteralInnerEscape  annotation escape
	| StringLiteralInnerGap     annotation gap

-- | An escaped character.
data EscapeBase lexicalBackslash escapeInner annotation fixpoint =
	EscapedChar annotation lexicalBackslash escapeInner

-- | An escaped character after the backslash prefix.
data EscapeInnerBase charEsc ascii decimal lexicalOLower octal lexicalXLower hexadecimal annotation fixpoint =
	  EscapedCharEsc     annotation charEsc
	| EscapedAscii       annotation ascii
	| EscapedDecimal     annotation decimal
	| EscapedOctal       annotation lexicalOLower octal
	| EscapedHexadecimal annotation lexicalXLower hexadecimal

-- | A character that is denoted to have special meaning when escaped.
data CharEscBase lexicalALower lexicalBLower lexicalFLower lexicalNLower lexicalRLower lexicalTLower lexicalVLower lexicalBackslash lexicalDoubleQuote lexicalSingleQuote lexicalAmpersand annotation fixpoint =
	  EscapedA           lexicalALower
	| EscapedB           lexicalBLower
	| EscapedF           lexicalFLower
	| EscapedN           lexicalNLower
	| EscapedR           lexicalRLower
	| EscapedT           lexicalTLower
	| EscapedV           lexicalVLower
	| EscapedBackslash   lexicalBackslash
	| EscapedDoubleQuote lexicalDoubleQuote
	| EscapedSingleQuote lexicalSingleQuote
	| EscapedAmpersand   lexicalAmpersand

-- | An ASCII control character.
data AsciiBase lexicalCaret cntrl lexicalNUL lexicalSOH lexicalSTX lexicalETX lexicalEOT lexicalENQ lexicalACK lexicalBEL lexicalBS lexicalHT lexicalLF lexicalVT lexicalFF lexicalCR lexicalSO lexicalSI lexicalDLE lexicalDC1 lexicalDC2 lexicalDC3 lexicalDC4 lexicalNAK lexicalSYN lexicalETB lexicalCAN lexicalEM lexicalSUB lexicalESC lexicalFS lexicalGS lexicalRS lexicalUS lexicalSpace lexicalDEL annotation fixpoint =
	  AsciiControl                           annotation lexicalCaret cntrl
	| AsciiNull                              annotation lexicalNUL
	| AsciiStartOfHeading                    annotation lexicalSOH
	| AsciiStartOfText                       annotation lexicalSTX
	| AsciiEndOfText                         annotation lexicalETX
	| AsciiEndOfTransmission                 annotation lexicalEOT
	| AsciiEnquiry                           annotation lexicalENQ
	| AsciiAcknowledgement                   annotation lexicalACK
	| AsciiBell                              annotation lexicalBEL
	| AsciiBackspace                         annotation lexicalBS
	| AsciiHorizontalTab                     annotation lexicalHT
	| AsciiLineFeed                          annotation lexicalLF
	| AsciiVerticalTab                       annotation lexicalVT
	| AsciiFormFeed                          annotation lexicalFF
	| AsciiCarriageReturn                    annotation lexicalCR
	| AsciiShiftOut                          annotation lexicalSO
	| AsciiShiftIn                           annotation lexicalSI
	| AsciiDataLinkEscape                    annotation lexicalDLE
	| AsciiDeviceControl1XON                 annotation lexicalDC1
	| AsciiDeviceControl2                    annotation lexicalDC2
	| AsciiDeviceControl3XOFF                annotation lexicalDC3
	| AsciiDeviceControl4                    annotation lexicalDC4
	| AsciiNegativeAcknowledgement           annotation lexicalNAK
	| AsciiSynchronousIdle                   annotation lexicalSYN
	| AsciiEndOfTransmissionBlock            annotation lexicalETB
	| AsciiCancel                            annotation lexicalCAN
	| AsciiEndOfMedium                       annotation lexicalEM
	| AsciiSubstitute                        annotation lexicalSUB
	| AsciiEscape                            annotation lexicalESC
	| AsciiFileSeparator                     annotation lexicalFS
	| AsciiGroupSeparator                    annotation lexicalGS
	| AsciiRecordSeparator                   annotation lexicalRS
	| AsciiUnitSeparator                     annotation lexicalUS
	| AsciiSpace                             annotation lexicalSpace
	| AsciiDelete                            annotation lexicalDEL

-- | An ascii character that can be put after ‘\^’ to complete a control character escape sequence.
data CntrlBase ascLarge lexicalAt lexicalLeftBracket lexicalBackslash lexicalRightBracket lexicalCaret lexicalUnderscore annotation fixpoint =
	  AsciiControlAscLarge     annotation ascLarge
	| AsciiControlAt           annotation lexicalAt
	| AsciiControlLeftBracket  annotation lexicalLeftBracket
	| AsciiControlBackslash    annotation lexicalBackslash
	| AsciiControlRightBracket annotation lexicalRightBracket
	| AsciiControlCaret        annotation lexicalCaret
	| AsciiControlUnderscore   annotation lexicalUnderscore

-- | An ignored gap in a Haskell-style multiline gap or other form of ignored space gap.
--
-- This is an escaped whitespace followed by the remaining whitespace until the
-- terminating backslash character.
data GapBase list lexicalBackslash whitechar annotation fixpoint =
	WhitespaceGap annotation lexicalBackslash whitechar (list whitechar) lexicalBackslash

-- Base lexical structures.

-- TODO

-- Exclusion and miscellaneous lexical structures.

-- TODO

-- Lexical foundation.

-- TODO

{-
 - ----------------------------------------------------------------
 - Structures with default linking.
 - ----------------------------------------------------------------
 -}

-- Intra-module linking.

-- TODO: available option: annotationLexer annotationGrammar.  (Note that our linking method requires selecting lexeme annotations from what we have.)

-- TODO
