-- vim: set filetype=haskell noet

{-
 - RegularStructures.hs
 -
 - Haskell2010 Simple syntax AST: base data structures, without exclusion
 - structures.
 -
 - Instantiates AST structures with intra-module references, providing a
 - default implementation.
 -}

{-# LANGUAGE Haskell2010 #-}

-- | Haskell2010 Simple syntax AST: regular data structures.
--
-- This module provides regular data structures for the Haskell2010 Simple
-- syntax AST implementation.
module Language.Haskell2010.Ahc.Syntax.Haskell2010.Simple.AST.Base.RegularStructures (
	-- Export everything, explicitly.

	-- * Base structures.

	-- ** § 5.1 Module Structure types.
	ModuleBase(ModuleWithHeader, ModuleWithoutHeader),
	BodyBase(BodyImportsTops, BodyImportsOnly, BodyTopsOnly),
	ImpDeclsBase(ImpDeclSequence),
	TopDeclsBase(TopDeclSequence),

	-- ** § 5.2 Export Lists types.
	ExportsBase(ExportsList),
	ExportBase(ExportVariable, ExportTypeVariable, ExportClassVariable, ExportModule),
	CnameBase(CnameVar, CnameCon),

	-- ** § 5.3 Import Declarations types.
	ImpDeclBase(ImportStatement, NullImport),
	ImpSpecBase(ImportWhitelist, ImportBlacklist),
	ImportBase(ImportVariableAttribute, ImportTypeVariableAttribute, ImportTypeClassVariableAttribute),

	-- ** § 4 Declarations and Bindings types.
	TopDeclBase(TopDeclType, TopDeclData, TopDeclNewtype, TopDeclClass, TopDeclInstance, TopDeclDefault, TopDeclForeign, TopDeclRegular),
	DeclsBase(Declarations),
	DeclBase(DeclarationMeta, DeclarationValue),
	CdeclsBase(ClassDeclarations),
	CdeclBase(ClassDeclarationMeta, ClassDeclarationValue),
	IdeclsBase(InstanceDeclarations),
	IdeclBase(InstanceDeclarationValue, NullInstanceDeclaration),
	GenDeclBase(TypeDeclaration, FixityDeclaration, NullMetaDeclaration),
	OpsBase(OpSequence),
	VarsBase(VarSequence),
	FixityBase(FixityInfixr, FixityInfixl, FixityInfix),

	-- ** § 4.1.2 Syntax of Types types.
	TypeBase(Type),
	BTypeBase(TypeApplication),
	ATypeBase(GeneralTypeConstructor, TypeVariableType, TupleType, ListType, GroupedType),
	GtyconBase(QualifiableTypeConstructor, UnitTypeConstructor, EmptyListTypeConstructor, FunctionTypeConstructor, TupleTypeConstructor),

	-- ** § 4.1.3 Syntax of Class Assertions and Contexts types.
	ContextBase(ContextSingle, Context),
	ClassBase(AssertUnappliedTypeVariableInClass, AssertAppliedTypeVariableInClass),
	{-
	ClassQtyclsBase(TypeClass),
	ClassTyclsBase(UnqualifiedTypeClass),
	ClassTyvarBase(TypeVariable),
	-}

	-- ** § 4.2.1 Algebraic Datatype Declarations types.
	SimpleTypeBase(NamesType),
	ConstrsBase(Constructors),
	ConstrBase(BasicConstructor, BinaryOperatorConstructor, RecordConstructor),
	EvalAtypeBase(OrdableAtype),
	FieldDeclBase(FieldDeclaration),
	DerivingBase(DerivingClause),
	DclassBase(DerivingClass),

	-- ** § 4.2.3 Datatype Renamings types.
	NewConstrBase(BasicNewtypeConstructor, RecordNewtypeConstructor),

	-- ** § 4.3.1 Type Classes and Overloading types.
	SContextBase(SimpleContextSingle, SimpleContextList),
	SimpleClassBase(SimpleClassAssertion),

	-- ** § 4.3.2 Instance Declarations types.
	InstBase(GeneralTypeConstructorInstance, AppliableGeneralTypeConstructorInstance, TypeVariableTupleInstance, ListInstance, FunctionInstance),

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
	FexpBase(ApplicationExpression),
	AexpBase(VariableExpression, ConstructorExpression, LiteralExpression, ParenthesesExpression, TupleExpression, ListExpression, ArithmeticSequenceExpression, ListComprehensionExpression, LeftSectionExpression, RightSectionExpression, ConstructRecordExpression, ModifyRecordExpression),

	-- ** § 3.2 Variables, Constructors, Operators, and Literals types.
	GconBase(UnitConstructor, EmptyListConstructor, TupleConstructor, QualifiableConstructor),
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

	-- ** § 3.13 Case Expressions types.
	AltsBase(CaseBranches),
	AltBase(ExpBranch, GuardedExpBranch, NullExpBranch),
	GdpatBase(GuardClauses),
	GuardsBase(GuardClauseGuards),
	GuardBase(PatternGuard, LocalDeclaration, BooleanGuard),

	-- ** § 3.14 Do Expressions types.
	StmtsBase(DoStatements),
	StmtBase(BaseStmt, BindStmt, LetStmt, NullStmt),

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
	BigAnySeqValidNcomChar1_1Base(NotNcomChar1_1, LeftBraceChar1_1, HyphenChar1_1, EOPChar1_1),
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

	AscSymbolBase(ExclamationAsciiSymbol, HashAsciiSymbol, DollarAsciiSymbol, PercentAsciiSymbol, AmpersandAsciiSymbol, AsteriskAsciiSymbol, PlusAsciiSymbol, DotAsciiSymbol, SlashAsciiSymbol, LeftAngleBracketAsciiSymbol, EqualsAsciiSymbol, RightAngleBracketAsciiSymbol, QuestionMarkAsciiSymbol, AtAsciiSymbol, BackslashAsciiSymbol, CaretAsciiSymbol, PipeAsciiSymbol, HyphenAsciiSymbol, TildeAsciiSymbol, ColonAsciiSymbol),
	AscSymbolSansNcBase(ExclamationAsciiSymbolSansNc, HashAsciiSymbolSansNc, DollarAsciiSymbolSansNc, PercentAsciiSymbolSansNc, AmpersandAsciiSymbolSansNc, AsteriskAsciiSymbolSansNc, PlusAsciiSymbolSansNc, DotAsciiSymbolSansNc, SlashAsciiSymbolSansNc, LeftAngleBracketAsciiSymbolSansNc, EqualsAsciiSymbolSansNc, RightAngleBracketAsciiSymbolSansNc, QuestionMarkAsciiSymbolSansNc, AtAsciiSymbolSansNc, BackslashAsciiSymbolSansNc, CaretAsciiSymbolSansNc, PipeAsciiSymbolSansNc, TildeAsciiSymbolSansNc, ColonAsciiSymbolSansNc),
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
	-- | (Note: 'VaridBase', with exclusions, appears in 'Language.Haskell2010.Ahc.Syntax.Haskell2010.Simple.AST.Base.ExclusionStructures'.)
	VaridNoExclusionsBase(VaridNoExclusionsWhole),
	VaridStartBase(SmallVaridStart),
	IdentifierInnerBase(SmallIdentifierInner, LargeIdentifierInner, DigitIdentifierInner, SingleQuoteIdentifierInner),
	VaridInnerBase(IdInnerVaridInner),
	ConidBase(ConidWhole),
	ConidStartBase(LargeConidStart),
	ConidInnerBase(IdInnerConidInner),
	ReservedidBase(CaseReservedId, ClassReservedId, DataReservedId, DefaultReservedId, DerivingReservedId, DoReservedId, ElseReservedId, ForeignReservedId, IfReservedId, ImportReservedId, InReservedId, InfixReservedId, InfixlReservedId, InfixrReservedId, InstanceReservedId, LetReservedId, ModuleReservedId, NewtypeReservedId, OfReservedId, ThenReservedId, TypeReservedId, WhereReservedId, UnderscoreReservedId),
	-- | (Note: 'VaridBase', with exclusions, appears in 'Language.Haskell2010.Ahc.Syntax.Haskell2010.Simple.AST.Base.ExclusionStructures'.)
	VarSymNoExtraExclusionsBase(VarSymWhole),
	VarSymStartBase(SymbolSansColonVarSymStart),
	-- | (Note: 'ConidBase', with exclusions, appears in 'Language.Haskell2010.Ahc.Syntax.Haskell2010.Simple.AST.Base.ExclusionStructures'.)
	ConSymNoExtraExclusionsBase(ConSymWhole),
	ConSymStartBase(ColonConSymStart),
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

	-- | (Note: the exclusion structures appear in 'Language.Haskell2010.Ahc.Syntax.Haskell2010.Simple.AST.Base.ExclusionStructures'.)

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

	-- $baseLexicalStructures

	-- *** Pseudo-foundational lexical structures.

	-- $pseudoFoundationalLexicalStructures

	LexicalPseudoBase(PseudoLexicalNonsymKeyword, PseudoLexicalNonsymNonkeyword, PseudoLexicalSymAlias, PseudoLexicalAlias, PseudoLexicalNumPrefix),

	-- **** Non-symbolic keyword pseudo-lexical structures.

	LexicalNonsymKeywordBase(CaseNonsymKeyword, ClassNonsymKeyword, DataNonsymKeyword, DefaultNonsymKeyword, DerivingNonsymKeyword, DoNonsymKeyword, ElseNonsymKeyword, ForeignNonsymKeyword, IfNonsymKeyword, ImportNonsymKeyword, InNonsymKeyword, InfixNonsymKeyword, InfixlNonsymKeyword, InfixrNonsymKeyword, InstanceNonsymKeyword, LetNonsymKeyword, ModuleNonsymKeyword, NewtypeNonsymKeyword, OfNonsymKeyword, ThenNonsymKeyword, TypeNonsymKeyword, WhereNonsymKeyword),
	LexicalCaseBase(PseudoLexicalCase),
	LexicalClassBase(PseudoLexicalClass),
	LexicalDataBase(PseudoLexicalData),
	LexicalDefaultBase(PseudoLexicalDefault),
	LexicalDerivingBase(PseudoLexicalDeriving),
	LexicalDoBase(PseudoLexicalDo),
	LexicalElseBase(PseudoLexicalElse),
	LexicalForeignBase(PseudoLexicalForeign),
	LexicalIfBase(PseudoLexicalIf),
	LexicalImportBase(PseudoLexicalImport),
	LexicalInBase(PseudoLexicalIn),
	LexicalInfixBase(PseudoLexicalInfix),
	LexicalInfixlBase(PseudoLexicalInfixl),
	LexicalInfixrBase(PseudoLexicalInfixr),
	LexicalInstanceBase(PseudoLexicalInstance),
	LexicalLetBase(PseudoLexicalLet),
	LexicalModuleBase(PseudoLexicalModule),
	LexicalNewtypeBase(PseudoLexicalNewtype),
	LexicalOfBase(PseudoLexicalOf),
	LexicalThenBase(PseudoLexicalThen),
	LexicalTypeBase(PseudoLexicalType),
	LexicalWhereBase(PseudoLexicalWhere),

	-- **** Non-symbolic non-keyword pseudo-lexical structures.
	LexicalNonsymNonkeywordBase(AsNonsymNonkeyword, HidingNonsymNonkeyword, QualifiedNonsymNonkeyword),
	LexicalAsBase(PseudoLexicalAs),
	LexicalHidingBase(PseudoLexicalHiding),
	LexicalQualifiedBase(PseudoLexicalQualified),

	-- **** Symbolic alias pseudo-lexical structures.
	LexicalSymAliasBase(DotDotSymAlias, DoubleColonSymAlias, DoubleRightArrowSymAlias, LeftArrowSymAlias, RightArrowSymAlias),
	LexicalDotDotBase(PseudoLexicalDotDot),
	LexicalDoubleColonBase(PseudoLexicalDoubleColon),
	LexicalDoubleRightArrowBase(PseudoLexicalDoubleRightArrow),
	LexicalLeftArrowBase(PseudoLexicalLeftArrow),
	LexicalRightArrowBase(PseudoLexicalRightArrow),

	-- **** Alias pseudo-lexical structures.
	LexicalAliasBase(SpaceAlias, MinusAlias, AsciiLambdaAlias),
	LexicalSpaceBase(PseudoLexicalSpace),
	LexicalMinusBase(PseudoLexicalMinus),
	LexicalAsciiLambdaBase(PseudoLexicalAsciiLambda),

	-- **** Non-symbolic numeric literal prefix pseudo-lexical structures.
	LexicalNumPrefixBase(N0oNumPrefix, N0ONumPrefix, N0xNumPrefix, N0XNumPrefix),
	Lexical0oBase(PseudoLexical0o),
	Lexical0OBase(PseudoLexical0O),
	Lexical0xBase(PseudoLexical0x),
	Lexical0XBase(PseudoLexical0X),

	-- *** Lexical foundation.

	-- $lexicalFoundation

	LexicalEndOfParseKeyBase,

	LexicalUnicodeDigitKeyBase,
	LexicalUnicodeLargeKeyBase,
	LexicalUnicodeSmallKeyBase,
	LexicalUnicodeSymbolKeyBase,
	LexicalUnicodeWhitespaceCharKeyBase,

	LexicalUnicodeSmallSansAscUnderscoreKeyBase,
	LexicalUnicodeSymbolSansSpecialUnderscoreDoubleQuoteSingleQuoteKeyBase,
	LexicalUnicodeSymbolSansSpecialUnderscoreDoubleQuoteSingleQuoteAsciiKeyBase,
	LexicalUnicodeSymbolSansSpecialUnderscoreDoubleQuoteSingleQuoteColonKeyBase,
	LexicalUnicodeSymbolSansSpecialUnderscoreDoubleQuoteSingleQuoteHyphenKeyBase,

	LexicalNULKeyBase,
	LexicalSOHKeyBase,
	LexicalSTXKeyBase,
	LexicalETXKeyBase,
	LexicalEOTKeyBase,
	LexicalENQKeyBase,
	LexicalACKKeyBase,
	LexicalBELKeyBase,
	LexicalBSKeyBase,
	LexicalHTKeyBase,
	LexicalLFKeyBase,
	LexicalVTKeyBase,
	LexicalFFKeyBase,
	LexicalCRKeyBase,
	LexicalSOKeyBase,
	LexicalSIKeyBase,
	LexicalDLEKeyBase,
	LexicalDC1KeyBase,
	LexicalDC2KeyBase,
	LexicalDC3KeyBase,
	LexicalDC4KeyBase,
	LexicalNAKKeyBase,
	LexicalSYNKeyBase,
	LexicalETBKeyBase,
	LexicalCANKeyBase,
	LexicalEMKeyBase,
	LexicalSUBKeyBase,
	LexicalESCKeyBase,
	LexicalFSKeyBase,
	LexicalGSKeyBase,
	LexicalRSKeyBase,
	LexicalUSKeyBase,

	LexicalSPKeyBase,
	LexicalExclamationKeyBase,
	LexicalDoubleQuoteKeyBase,
	LexicalHashKeyBase,
	LexicalDollarKeyBase,
	LexicalPercentKeyBase,
	LexicalAmpersandKeyBase,
	LexicalSingleQuoteKeyBase,
	LexicalLeftParenthesisKeyBase,
	LexicalRightParenthesisKeyBase,
	LexicalAsteriskKeyBase,
	LexicalPlusKeyBase,
	LexicalCommaKeyBase,
	LexicalHyphenKeyBase,
	LexicalDotKeyBase,
	LexicalSlashKeyBase,

	Lexical0KeyBase,
	Lexical1KeyBase,
	Lexical2KeyBase,
	Lexical3KeyBase,
	Lexical4KeyBase,
	Lexical5KeyBase,
	Lexical6KeyBase,
	Lexical7KeyBase,
	Lexical8KeyBase,
	Lexical9KeyBase,

	LexicalColonKeyBase,
	LexicalSemicolonKeyBase,
	LexicalLeftAngleBracketKeyBase,
	LexicalEqualsKeyBase,
	LexicalRightAngleBracketKeyBase,
	LexicalQuestionMarkKeyBase,
	LexicalAtKeyBase,

	LexicalAKeyBase,
	LexicalBKeyBase,
	LexicalCKeyBase,
	LexicalDKeyBase,
	LexicalEKeyBase,
	LexicalFKeyBase,
	LexicalGKeyBase,
	LexicalHKeyBase,
	LexicalIKeyBase,
	LexicalJKeyBase,
	LexicalKKeyBase,
	LexicalLKeyBase,
	LexicalMKeyBase,
	LexicalNKeyBase,
	LexicalOKeyBase,
	LexicalPKeyBase,
	LexicalQKeyBase,
	LexicalRKeyBase,
	LexicalSKeyBase,
	LexicalTKeyBase,
	LexicalUKeyBase,
	LexicalVKeyBase,
	LexicalWKeyBase,
	LexicalXKeyBase,
	LexicalYKeyBase,
	LexicalZKeyBase,

	LexicalLeftBracketKeyBase,
	LexicalBackslashKeyBase,
	LexicalRightBracketKeyBase,
	LexicalCaretKeyBase,
	LexicalUnderscoreKeyBase,
	LexicalBacktickKeyBase,

	LexicalALowerKeyBase,
	LexicalBLowerKeyBase,
	LexicalCLowerKeyBase,
	LexicalDLowerKeyBase,
	LexicalELowerKeyBase,
	LexicalFLowerKeyBase,
	LexicalGLowerKeyBase,
	LexicalHLowerKeyBase,
	LexicalILowerKeyBase,
	LexicalJLowerKeyBase,
	LexicalKLowerKeyBase,
	LexicalLLowerKeyBase,
	LexicalMLowerKeyBase,
	LexicalNLowerKeyBase,
	LexicalOLowerKeyBase,
	LexicalPLowerKeyBase,
	LexicalQLowerKeyBase,
	LexicalRLowerKeyBase,
	LexicalSLowerKeyBase,
	LexicalTLowerKeyBase,
	LexicalULowerKeyBase,
	LexicalVLowerKeyBase,
	LexicalWLowerKeyBase,
	LexicalXLowerKeyBase,
	LexicalYLowerKeyBase,
	LexicalZLowerKeyBase,

	LexicalLeftBraceKeyBase,
	LexicalPipeKeyBase,
	LexicalRightBraceKeyBase,
	LexicalTildeKeyBase,

	LexicalDELKeyBase,

	LexicalFoundationBase(
		LexicalFoundationPrimitives,

		_lexicalFoundationPrimitives_lexicalEndOfParse                                                     ,

		_lexicalFoundationPrimitives_lexicalUnicodeDigit                                                   ,
		_lexicalFoundationPrimitives_lexicalUnicodeLarge                                                   ,
		_lexicalFoundationPrimitives_lexicalUnicodeSmall                                                   ,
		_lexicalFoundationPrimitives_lexicalUnicodeSymbol                                                  ,
		_lexicalFoundationPrimitives_lexicalUnicodeWhitespaceChar                                          ,

		_lexicalFoundationPrimitives_lexicalUnicodeSmallSansAscUnderscore                                  ,
		_lexicalFoundationPrimitives_lexicalUnicodeSymbolSansSpecialUnderscoreDoubleQuoteSingleQuote       ,
		_lexicalFoundationPrimitives_lexicalUnicodeSymbolSansSpecialUnderscoreDoubleQuoteSingleQuoteAscii  ,
		_lexicalFoundationPrimitives_lexicalUnicodeSymbolSansSpecialUnderscoreDoubleQuoteSingleQuoteColon  ,
		_lexicalFoundationPrimitives_lexicalUnicodeSymbolSansSpecialUnderscoreDoubleQuoteSingleQuoteHyphen ,

		_lexicalFoundationPrimitives_lexicalNUL                                                            ,
		_lexicalFoundationPrimitives_lexicalSOH                                                            ,
		_lexicalFoundationPrimitives_lexicalSTX                                                            ,
		_lexicalFoundationPrimitives_lexicalETX                                                            ,
		_lexicalFoundationPrimitives_lexicalEOT                                                            ,
		_lexicalFoundationPrimitives_lexicalENQ                                                            ,
		_lexicalFoundationPrimitives_lexicalACK                                                            ,
		_lexicalFoundationPrimitives_lexicalBEL                                                            ,
		_lexicalFoundationPrimitives_lexicalBS                                                             ,
		_lexicalFoundationPrimitives_lexicalHT                                                             ,
		_lexicalFoundationPrimitives_lexicalLF                                                             ,
		_lexicalFoundationPrimitives_lexicalVT                                                             ,
		_lexicalFoundationPrimitives_lexicalFF                                                             ,
		_lexicalFoundationPrimitives_lexicalCR                                                             ,
		_lexicalFoundationPrimitives_lexicalSO                                                             ,
		_lexicalFoundationPrimitives_lexicalSI                                                             ,
		_lexicalFoundationPrimitives_lexicalDLE                                                            ,
		_lexicalFoundationPrimitives_lexicalDC1                                                            ,
		_lexicalFoundationPrimitives_lexicalDC2                                                            ,
		_lexicalFoundationPrimitives_lexicalDC3                                                            ,
		_lexicalFoundationPrimitives_lexicalDC4                                                            ,
		_lexicalFoundationPrimitives_lexicalNAK                                                            ,
		_lexicalFoundationPrimitives_lexicalSYN                                                            ,
		_lexicalFoundationPrimitives_lexicalETB                                                            ,
		_lexicalFoundationPrimitives_lexicalCAN                                                            ,
		_lexicalFoundationPrimitives_lexicalEM                                                             ,
		_lexicalFoundationPrimitives_lexicalSUB                                                            ,
		_lexicalFoundationPrimitives_lexicalESC                                                            ,
		_lexicalFoundationPrimitives_lexicalFS                                                             ,
		_lexicalFoundationPrimitives_lexicalGS                                                             ,
		_lexicalFoundationPrimitives_lexicalRS                                                             ,
		_lexicalFoundationPrimitives_lexicalUS                                                             ,

		_lexicalFoundationPrimitives_lexicalSP                                                             ,
		_lexicalFoundationPrimitives_lexicalExclamation                                                    ,
		_lexicalFoundationPrimitives_lexicalDoubleQuote                                                    ,
		_lexicalFoundationPrimitives_lexicalHash                                                           ,
		_lexicalFoundationPrimitives_lexicalDollar                                                         ,
		_lexicalFoundationPrimitives_lexicalPercent                                                        ,
		_lexicalFoundationPrimitives_lexicalAmpersand                                                      ,
		_lexicalFoundationPrimitives_lexicalSingleQuote                                                    ,
		_lexicalFoundationPrimitives_lexicalLeftParenthesis                                                ,
		_lexicalFoundationPrimitives_lexicalRightParenthesis                                               ,
		_lexicalFoundationPrimitives_lexicalAsterisk                                                       ,
		_lexicalFoundationPrimitives_lexicalPlus                                                           ,
		_lexicalFoundationPrimitives_lexicalComma                                                          ,
		_lexicalFoundationPrimitives_lexicalHyphen                                                         ,
		_lexicalFoundationPrimitives_lexicalDot                                                            ,
		_lexicalFoundationPrimitives_lexicalSlash                                                          ,

		_lexicalFoundationPrimitives_lexical0                                                              ,
		_lexicalFoundationPrimitives_lexical1                                                              ,
		_lexicalFoundationPrimitives_lexical2                                                              ,
		_lexicalFoundationPrimitives_lexical3                                                              ,
		_lexicalFoundationPrimitives_lexical4                                                              ,
		_lexicalFoundationPrimitives_lexical5                                                              ,
		_lexicalFoundationPrimitives_lexical6                                                              ,
		_lexicalFoundationPrimitives_lexical7                                                              ,
		_lexicalFoundationPrimitives_lexical8                                                              ,
		_lexicalFoundationPrimitives_lexical9                                                              ,

		_lexicalFoundationPrimitives_lexicalColon                                                          ,
		_lexicalFoundationPrimitives_lexicalSemicolon                                                      ,
		_lexicalFoundationPrimitives_lexicalLeftAngleBracket                                               ,
		_lexicalFoundationPrimitives_lexicalEquals                                                         ,
		_lexicalFoundationPrimitives_lexicalRightAngleBracket                                              ,
		_lexicalFoundationPrimitives_lexicalQuestionMark                                                   ,
		_lexicalFoundationPrimitives_lexicalAt                                                             ,

		_lexicalFoundationPrimitives_lexicalA                                                              ,
		_lexicalFoundationPrimitives_lexicalB                                                              ,
		_lexicalFoundationPrimitives_lexicalC                                                              ,
		_lexicalFoundationPrimitives_lexicalD                                                              ,
		_lexicalFoundationPrimitives_lexicalE                                                              ,
		_lexicalFoundationPrimitives_lexicalF                                                              ,
		_lexicalFoundationPrimitives_lexicalG                                                              ,
		_lexicalFoundationPrimitives_lexicalH                                                              ,
		_lexicalFoundationPrimitives_lexicalI                                                              ,
		_lexicalFoundationPrimitives_lexicalJ                                                              ,
		_lexicalFoundationPrimitives_lexicalK                                                              ,
		_lexicalFoundationPrimitives_lexicalL                                                              ,
		_lexicalFoundationPrimitives_lexicalM                                                              ,
		_lexicalFoundationPrimitives_lexicalN                                                              ,
		_lexicalFoundationPrimitives_lexicalO                                                              ,
		_lexicalFoundationPrimitives_lexicalP                                                              ,
		_lexicalFoundationPrimitives_lexicalQ                                                              ,
		_lexicalFoundationPrimitives_lexicalR                                                              ,
		_lexicalFoundationPrimitives_lexicalS                                                              ,
		_lexicalFoundationPrimitives_lexicalT                                                              ,
		_lexicalFoundationPrimitives_lexicalU                                                              ,
		_lexicalFoundationPrimitives_lexicalV                                                              ,
		_lexicalFoundationPrimitives_lexicalW                                                              ,
		_lexicalFoundationPrimitives_lexicalX                                                              ,
		_lexicalFoundationPrimitives_lexicalY                                                              ,
		_lexicalFoundationPrimitives_lexicalZ                                                              ,

		_lexicalFoundationPrimitives_lexicalLeftBracket                                                    ,
		_lexicalFoundationPrimitives_lexicalBackslash                                                      ,
		_lexicalFoundationPrimitives_lexicalRightBracket                                                   ,
		_lexicalFoundationPrimitives_lexicalCaret                                                          ,
		_lexicalFoundationPrimitives_lexicalUnderscore                                                     ,
		_lexicalFoundationPrimitives_lexicalBacktick                                                       ,

		_lexicalFoundationPrimitives_lexicalALower                                                         ,
		_lexicalFoundationPrimitives_lexicalBLower                                                         ,
		_lexicalFoundationPrimitives_lexicalCLower                                                         ,
		_lexicalFoundationPrimitives_lexicalDLower                                                         ,
		_lexicalFoundationPrimitives_lexicalELower                                                         ,
		_lexicalFoundationPrimitives_lexicalFLower                                                         ,
		_lexicalFoundationPrimitives_lexicalGLower                                                         ,
		_lexicalFoundationPrimitives_lexicalHLower                                                         ,
		_lexicalFoundationPrimitives_lexicalILower                                                         ,
		_lexicalFoundationPrimitives_lexicalJLower                                                         ,
		_lexicalFoundationPrimitives_lexicalKLower                                                         ,
		_lexicalFoundationPrimitives_lexicalLLower                                                         ,
		_lexicalFoundationPrimitives_lexicalMLower                                                         ,
		_lexicalFoundationPrimitives_lexicalNLower                                                         ,
		_lexicalFoundationPrimitives_lexicalOLower                                                         ,
		_lexicalFoundationPrimitives_lexicalPLower                                                         ,
		_lexicalFoundationPrimitives_lexicalQLower                                                         ,
		_lexicalFoundationPrimitives_lexicalRLower                                                         ,
		_lexicalFoundationPrimitives_lexicalSLower                                                         ,
		_lexicalFoundationPrimitives_lexicalTLower                                                         ,
		_lexicalFoundationPrimitives_lexicalULower                                                         ,
		_lexicalFoundationPrimitives_lexicalVLower                                                         ,
		_lexicalFoundationPrimitives_lexicalWLower                                                         ,
		_lexicalFoundationPrimitives_lexicalXLower                                                         ,
		_lexicalFoundationPrimitives_lexicalYLower                                                         ,
		_lexicalFoundationPrimitives_lexicalZLower                                                         ,

		_lexicalFoundationPrimitives_lexicalLeftBrace                                                      ,
		_lexicalFoundationPrimitives_lexicalPipe                                                           ,
		_lexicalFoundationPrimitives_lexicalRightBrace                                                     ,
		_lexicalFoundationPrimitives_lexicalTilde                                                          ,

		_lexicalFoundationPrimitives_lexicalDEL
	),
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
data ImpDeclsBase data2 list impDecl lexicalSemicolon annotation fixpoint =
	ImpDeclSequence annotation impDecl (list (data2 lexicalSemicolon impDecl))

-- | Top-level declarations: what a module defines.
data TopDeclsBase data2 list topDecl lexicalSemicolon annotation fixpoint =
	TopDeclSequence annotation topDecl (list (data2 lexicalSemicolon topDecl))

-- § 5.2 Export Lists types.

-- | A module's export list.
data ExportsBase data2 maybe list lexicalLeftParenthesis export lexicalComma lexicalRightParenthesis annotation fixpoint =
	ExportsList annotation lexicalLeftParenthesis (maybe (data2 export (list (data2 lexicalComma export)))) lexicalRightParenthesis

-- | An export, in a module's export list.
--
-- (modid: module identifier.)
data ExportBase data2 data3 maybe either list qvar qtycon lexicalLeftParenthesis lexicalDotDot lexicalRightParenthesis cname lexicalComma qtycls var lexicalModule modid annotation fixpoint =
	  ExportVariable      annotation qvar
	| ExportTypeVariable  annotation qtycon        (maybe (either (data3 lexicalLeftParenthesis lexicalDotDot lexicalRightParenthesis) (data3 lexicalLeftParenthesis (maybe (data2 cname (list (lexicalComma cname)))) lexicalRightParenthesis)))
	| ExportClassVariable annotation qtycls        (maybe (either (data3 lexicalLeftParenthesis lexicalDotDot lexicalRightParenthesis) (data3 lexicalLeftParenthesis (maybe (data2 var   (list (lexicalComma var  )))) lexicalRightParenthesis)))
	| ExportModule        annotation lexicalModule modid

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
data ImpSpecBase data2 maybe list lexicalLeftParenthesis import_ lexicalComma lexicalRightParenthesis lexicalHiding annotation fixpoint =
	  ImportWhitelist annotation               lexicalLeftParenthesis (maybe (data2 import_ (list (data2 lexicalComma import_)))) lexicalRightParenthesis
		-- ^ Import only ….
	| ImportBlacklist annotation lexicalHiding lexicalLeftParenthesis (maybe (data2 import_ (list (data2 lexicalComma import_)))) lexicalRightParenthesis
		-- ^ Import all but ….

-- | An individual attribute (e.g. variable) in a list of elements to import from a module in an import statement.
data ImportBase data3 data2 maybe either list var lexicalLeftParenthesis lexicalDotDot lexicalRightParenthesis cname lexicalComma annotation fixpoint =
	  ImportVariableAttribute          annotation var
		-- ^ Import a regular variable from a module.
	| ImportTypeVariableAttribute      annotation (maybe (either (data3 lexicalLeftParenthesis lexicalDotDot lexicalRightParenthesis) (data3 lexicalLeftParenthesis (maybe (data2 cname (list (data2 lexicalComma cname)))) lexicalRightParenthesis)))
		-- ^ Import a ‘data’ type or ‘newtype’ type from a module.
	| ImportTypeClassVariableAttribute annotation (maybe (either (data3 lexicalLeftParenthesis lexicalDotDot lexicalRightParenthesis) (data3 lexicalLeftParenthesis (maybe (data2 var   (list (data2 lexicalComma var  )))) lexicalRightParenthesis)))
		-- ^ Import a type class from a module.

-- § 4 Declarations and Bindings types.

-- | Top-level declarations.
--
-- These are the components of a module, especially regular declarations
-- ('TopDeclRegular') for attributes of modules that can be exported and
-- imported.
data TopDeclBase data2 maybe list lexicalType simpleType type_ lexicalData context lexicalDoubleRightArrow lexicalEquals constrs deriving_ lexicalNewtype newConstr lexicalClass scontext tycls tyvar lexicalWhere cdecls lexicalInstance qtycls inst idecls lexicalDefault lexicalLeftParenthesis lexicalComma lexicalRightParenthesis lexicalForeign fdecl decl annotation fixpoint =
	  TopDeclType     annotation lexicalType     simpleType             type_
	| TopDeclData     annotation lexicalData     (maybe (data2 context  lexicalDoubleRightArrow)) simpleType                                              (maybe (data2 lexicalEquals constrs)) (maybe deriving_)
	| TopDeclNewtype  annotation lexicalNewtype  (maybe (data2 context  lexicalDoubleRightArrow)) simpleType                                              lexicalEquals                         newConstr                          (maybe deriving_)
	| TopDeclClass    annotation lexicalClass    (maybe (data2 scontext lexicalDoubleRightArrow)) tycls                                                   tyvar                                 (maybe (data2 lexicalWhere cdecls))
	| TopDeclInstance annotation lexicalInstance (maybe (data2 scontext lexicalDoubleRightArrow)) qtycls                                                  inst                                  (maybe (data2 lexicalWhere idecls))
	| TopDeclDefault  annotation lexicalDefault  lexicalLeftParenthesis                           (maybe (data2 type_ (list (data2 lexicalComma type_)))) lexicalRightParenthesis
	| TopDeclForeign  annotation lexicalForeign  fdecl
	| TopDeclRegular  annotation decl

-- | A block of declarations.
data DeclsBase data2 maybe list lexicalLeftBrace decl lexicalSemicolon lexicalRightBrace annotation fixpoint =
	Declarations annotation lexicalLeftBrace (maybe (data2 decl (list (data2 lexicalSemicolon decl)))) lexicalRightBrace

-- | A regular declaration.
--
-- It defines the type, value in a branch, or specification (e.g. fixity
-- attribute) of an attribute or variable.
data DeclBase either gendecl funlhs pat rhs annotation fixpoint =
	  DeclarationMeta  annotation gendecl
	| DeclarationValue annotation (either funlhs pat) rhs

-- | A block of class-declarations inside a class definition.
data CdeclsBase data2 maybe list lexicalLeftBrace cdecl lexicalSemicolon lexicalRightBrace annotation fixpoint =
	ClassDeclarations annotation lexicalLeftBrace (maybe (data2 cdecl (list (data2 lexicalSemicolon cdecl)))) lexicalRightBrace

-- | A regular class-declaration inside a class definition.
--
-- It defines the type, value in a branch, or specification (e.g. fixity
-- attribute) of an attribute or variable in a class definition.
data CdeclBase either gendecl funlhs pat rhs annotation fixpoint =
	  ClassDeclarationMeta  annotation gendecl
	| ClassDeclarationValue annotation (either funlhs pat) rhs

-- | A block of instance-declarations inside an instance body.
data IdeclsBase data2 maybe list lexicalLeftBrace idecl lexicalSemicolon lexicalRightBrace annotation fixpoint =
	InstanceDeclarations annotation  lexicalLeftBrace (maybe (data2 idecl (list (data2 lexicalSemicolon idecl)))) lexicalRightBrace

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
data OpsBase data2 list op lexicalComma annotation fixpoint =
	OpSequence annotation op (list (data2 lexicalComma op))

-- | A list of variables, e.g in a type declaration.
data VarsBase data2 list var lexicalComma annotation fixpoint =
	VarSequence annotation var (list (data2 lexicalComma var))

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
data ATypeBase data2 list gtycon tyvar lexicalLeftParenthesis type_ lexicalComma lexicalRightParenthesis lexicalLeftAngleBracket lexicalRightAngleBracket annotation fixpoint =
	  GeneralTypeConstructor annotation gtycon
	| TypeVariableType       annotation tyvar
	| TupleType              annotation lexicalLeftParenthesis  type_ lexicalComma             type_ (list (data2 lexicalComma type_)) lexicalRightParenthesis
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
data ContextBase data2 maybe list class_ lexicalLeftParenthesis lexicalComma lexicalRightParenthesis annotation fixpoint =
	  ContextSingle annotation class_
		-- ^ The context form without parentheses, which requires the number of class assertions to be 1.
	| Context       annotation lexicalLeftParenthesis (maybe (data2 class_ (list (data2 lexicalComma class_)))) lexicalRightParenthesis
		-- ^ The general context form.

-- | An assertion inside a context.
--
-- This assertion specifies that a type must be a member of a class, for what follows.
data ClassBase list qtycls tyvar lexicalLeftParenthesis atype lexicalRightParenthesis annotation fixpoint =
	  AssertUnappliedTypeVariableInClass annotation qtycls tyvar
	| AssertAppliedTypeVariableInClass   annotation qtycls lexicalLeftParenthesis tyvar atype (list atype) lexicalRightParenthesis

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
data ConstrsBase data2 list constr lexicalPipe annotation fixpoint =
	Constructors annotation constr (list (data2 lexicalPipe constr))

-- | A data constructor, for a ‘data’ ADT type declaration / definition.
data ConstrBase data2 list either maybe con evalAtype btype conop lexicalLeftBrace fieldDecl lexicalComma lexicalRightBrace annotation fixpoints =
	  BasicConstructor          annotation con                      (list evalAtype)
	| BinaryOperatorConstructor annotation (either btype evalAtype) conop            (either btype evalAtype)
	| RecordConstructor         annotation con                      lexicalLeftBrace (maybe (data2 fieldDecl (list (data2 lexicalComma fieldDecl)))) lexicalRightBrace

-- | An applicable type that optionally can have a strict evaluation order specifier applied to it.
data EvalAtypeBase maybe lexicalExclamation atype annotation fixpoint =
	OrdableAtype annotation (maybe lexicalExclamation) atype

-- | A field declaration is one or more field names assigned to a type.
data FieldDeclBase either vars lexicalDoubleColon type_ evalAtype annotation fixpoint =
	FieldDeclaration annotation vars lexicalDoubleColon (either type_ evalAtype)

-- | A ‘deriving’ clause to declare automatic derivation for a ‘data’ ADT or ‘newtype’ type.
data DerivingBase data3 data2 either maybe list lexicalDeriving dclass lexicalLeftParenthesis lexicalComma lexicalRightParenthesis annotation fixpoint =
	DerivingClause annotation lexicalDeriving (either dclass (data3 lexicalLeftParenthesis (maybe (data2 dclass (list (data2 lexicalComma dclass)))) lexicalRightParenthesis))

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
data SContextBase data2 maybe list simpleClass lexicalLeftParenthesis lexicalComma lexicalRightParenthesis annotation fixpoint =
	  SimpleContextSingle annotation simpleClass
		-- ^ Form with ommitted parentheses, requiring exactly 1 simple class.
	| SimpleContextList   annotation lexicalLeftParenthesis (maybe (data2 simpleClass (list (data2 lexicalComma simpleClass)))) lexicalRightParenthesis
		-- ^ Normal form, with parentheses.

-- | A class assertion consisting of names.
--
-- This is found in simple contexts, which are like contexts, but they don't
-- support applications of types to other types, as such application is not
-- represented by a type variable name.  (e.g. ‘data’ supports full contexts,
-- and a Haskell2010 ‘class’ declaration only supports simple contexts.)
data SimpleClassBase qtycls tyvar annotation fixpoint =
	SimpleClassAssertion annotation qtycls tyvar

-- § 4.3.2 Instance Declarations types.

-- | The member part of an instance declaration.
--
-- This is what is declared to be an instance of a type class.
--
-- (The context and type class have already been written.  This is what
-- follows up through before the possible ‘where’ clause.)
data InstBase data2 list gtycon lexicalLeftParenthesis tyvar lexicalRightParenthesis lexicalComma lexicalLeftBracket lexicalRightBracket lexicalRightArrow annotation fixpoint =
	  GeneralTypeConstructorInstance          annotation gtycon
		-- ^ A type or a type constructor (but not in a form where it can be
		-- further applied with type variables after it is referenced here.)
	| AppliableGeneralTypeConstructorInstance annotation lexicalLeftParenthesis gtycon (list tyvar)        lexicalRightParenthesis
		-- ^ Like 'GeneralTypeConstructorInstance', but it has parentheses, so it can accommodate syntax for application.
		--
		-- (Note: in the separate semantics phase, the spec says the vars are distinct.)
		-- (In the syntax phase, we specify the structure of the variables.)
	| TypeVariableTupleInstance               annotation lexicalLeftParenthesis tyvar  lexicalComma        tyvar (list (data2 lexicalComma tyvar)) lexicalRightParenthesis
		-- (Note: again, in the separate semantics phase, the spec says the vars are distinct.)
	| ListInstance                            annotation lexicalLeftBracket     tyvar  lexicalRightBracket
		-- ^ Like 'AppliableGeneralTypeConstructorInstance' but with a sugar
		-- syntax for list type constructor application.
	| FunctionInstance                        annotation lexicalLeftParenthesis tyvar  lexicalRightArrow   tyvar lexicalRightParenthesis
		-- ^ Like 'AppliableGeneralTypeConstructorInstance' but with a sugar
		-- syntax for function (arrow) type constructor application.
		--
		-- (Note: again, in the separate semantics phase, the spec says the vars are distinct.)

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
data ExpBase data2 maybe infixExp lexicalDoubleColon context lexicalDoubleRightArrow type_ annotation fixpoint =
	  TypedExpression   annotation infixExp lexicalDoubleColon (maybe (data2 context lexicalDoubleRightArrow)) type_
	| UntypedExpression annotation infixExp

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
data LexpBase maybe list lexicalAsciiLambda apat lexicalRightArrow exp lexicalLet decls lexicalIn lexicalIf lexicalSemicolon lexicalThen lexicalElse lexicalCase lexicalOf lexicalLeftBrace alts lexicalRightBrace stmts annotation fexp fixpoint =
	  LambdaExpression      annotation lexicalAsciiLambda    apat  (list apat)              lexicalRightArrow exp
	| LetExpression         annotation lexicalLet            decls lexicalIn                exp
	| ConditionalExpression annotation lexicalIf             exp   (maybe lexicalSemicolon) lexicalThen       exp  (maybe lexicalSemicolon) lexicalElse exp
	| CaseExpression        annotation lexicalCase           exp   lexicalOf                lexicalLeftBrace  alts lexicalRightBrace
	| DoExpression          annotation lexicalLeftBrace      stmts lexicalRightBrace
	| BaseExpression        annotation fexp
		-- ^ Function application, possibly with 0-arity.

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
data AexpBase data2 maybe list qvar gcon literal lexicalLeftParenthesis exp lexicalRightParenthesis lexicalComma lexicalLeftBracket lexicalRightBracket lexicalDotDot lexicalPipe qual infixExp qop qopSansUnaryMinus qcon lexicalLeftBrace fbind lexicalRightBrace aexpSansQcon fbinds1 annotation fixpoint =
	  VariableExpression           annotation qvar
	| ConstructorExpression        annotation gcon
	| LiteralExpression            annotation literal
	| ParenthesesExpression        annotation lexicalLeftParenthesis exp               lexicalRightParenthesis
	| TupleExpression              annotation lexicalLeftParenthesis exp               lexicalComma                                           exp                     (list  (data2 lexicalComma exp )) lexicalRightParenthesis
	| ListExpression               annotation lexicalLeftBracket     exp               (list (data2 lexicalComma exp))                        lexicalRightBracket
	| ArithmeticSequenceExpression annotation lexicalLeftBracket     exp               (maybe exp)                                            lexicalDotDot           (maybe exp)                       lexicalRightBracket
	| ListComprehensionExpression  annotation lexicalLeftBracket     exp               lexicalPipe                                            qual                    (list  (data2 lexicalComma qual)) lexicalRightBracket
	| LeftSectionExpression        annotation lexicalLeftParenthesis infixExp          qop                                                    lexicalRightParenthesis
		-- ^ (The left section form of partial application, with a binary operation.)
	| RightSectionExpression       annotation lexicalLeftParenthesis qopSansUnaryMinus infixExp                                               lexicalRightParenthesis
		-- ^ (The right section form of partial application, with a binary operation.)
	| ConstructRecordExpression    annotation qcon                   lexicalLeftBrace (maybe (data2 fbind (list (data2 lexicalComma fbind)))) lexicalRightBrace
	| ModifyRecordExpression       annotation aexpSansQcon           lexicalLeftBrace fbind                                                   (list (data2 lexicalComma fbind)) lexicalRightBrace

-- § 3.2 Variables, Constructors, Operators, and Literals types.

-- | A value constructor, extended with a selection of built-in constructors.
data GconBase list lexicalLeftParenthesis lexicalRightParenthesis lexicalLeftBracket lexicalRightBracket lexicalComma qcon annotation fixpoint =
	  UnitConstructor        annotation lexicalLeftParenthesis lexicalRightParenthesis
	| EmptyListConstructor   annotation lexicalLeftBracket     lexicalRightBracket
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

-- § 3.13 Case Expressions types.

-- | A non-empty sequence of ‘case’ branches.
data AltsBase data2 list alt lexicalSemicolon annotation fixpoint =
	CaseBranches annotation alt (list (data2 lexicalSemicolon alt))

-- | A ‘case’ branch.
data AltBase data2 maybe pat lexicalRightArrow exp lexicalWhere decls gdpat annotation fixpoint =
	  ExpBranch        annotation pat lexicalRightArrow exp                                (maybe (data2 lexicalWhere decls))
	| GuardedExpBranch annotation pat gdpat             (maybe (data2 lexicalWhere decls))
	| NullExpBranch    annotation

-- | An expression-level non-empty sequence of guard branches, themselves
-- subbranches of a ‘case’ branch or guarded subbranch.
data GdpatBase maybe guards lexicalRightArrow exp annotation fixpoint =
	GuardClauses annotation guards lexicalRightArrow exp (maybe fixpoint)

-- | One or more guards in a single guard clause at the expression level.
data GuardsBase data2 list lexicalPipe guard lexicalComma annotation fixpoint =
	GuardClauseGuards annotation lexicalPipe guard (list (data2 lexicalComma guard))

-- | An expression-level guard.
--
-- These can be found within ‘case’ expressions.
data GuardBase pat lexicalLeftArrow infixExp lexicalLet decls annotation fixpoint =
	  PatternGuard     annotation pat        lexicalLeftArrow infixExp
	| LocalDeclaration annotation lexicalLet decls
	| BooleanGuard     annotation infixExp

-- § 3.14 Do Expressions types.

-- | Zero or more ‘do’ statements preceding an expression.
data StmtsBase list maybe exp lexicalSemicolon stmt annotation fixpoint =
	DoStatements annotation (list stmt) exp (maybe lexicalSemicolon)

-- | A ‘do’ statement.
data StmtBase exp lexicalSemicolon pat lexicalLeftArrow lexicalLet decls annotation fixpoint =
	  BaseStmt annotation exp              lexicalSemicolon
	| BindStmt annotation pat              lexicalLeftArrow exp              lexicalSemicolon
	| LetStmt  annotation lexicalLet       decls            lexicalSemicolon
	| NullStmt annotation lexicalSemicolon

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
data LpatBase either list apat lexicalMinus integer float gcon annotation fixpoint =
	  BasePattern               annotation apat
	| MinusNumberPattern        annotation lexicalMinus (either integer float)
	| ExposedConstructorPattern annotation gcon         apat                   (list apat)

-- | Base pattern.
--
-- (E.g. these can be parameters in a lambda expression.)
data ApatBase data2 maybe list var lexicalAt gcon qcon lexicalLeftBrace fpat lexicalComma lexicalRightBrace literal lexicalUnderscore lexicalLeftParenthesis pat lexicalRightParenthesis lexicalLeftBracket lexicalRightBracket lexicalTilde annotation fixpoint =
	  AsPattern          annotation var                    (maybe (data2 lexicalAt fixpoint))
		-- ^ As-pattern: add a name and assign it to a value that matches this
		-- pattern.
	| ConstructorPattern annotation gcon
	| RecordPattern      annotation qcon                   lexicalLeftBrace                   (maybe (data2 fpat (list (data2 lexicalComma fpat)))) lexicalRightBrace
	| LiteralPattern     annotation literal
	| WildcardPattern    annotation lexicalUnderscore
	| GroupedPattern     annotation lexicalLeftParenthesis pat                                lexicalRightParenthesis
	| TuplePattern       annotation lexicalLeftParenthesis pat                                lexicalComma                                          pat                 (list (data2 lexicalComma pat)) lexicalRightParenthesis
	| ListPattern        annotation lexicalLeftBracket     pat                                (list (data2 lexicalComma pat))                       lexicalRightBracket
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
		-- ^ (E.g. ‘A.B.var’.)
	| QconidLexeme     annotation qconid
		-- ^ (E.g. ‘Con’.)
	| QvarSymLexeme    annotation qvarSym
		-- ^ (E.g. ‘A.B.+’.)
	| QconSymLexeme    annotation qconSym
		-- ^ (E.g. ‘A.B.:+’.)
	| LiteralLexeme    annotation literal
		-- ^ (E.g. ‘42’.)
	| SpecialLexeme    annotation special
		-- ^ (E.g. ‘|’.)
	| ReservedOpLexeme annotation reservedOp
		-- ^ (E.g. ‘::’.)
	| ReservedidLexeme annotation reservedid
		-- ^ (E.g. ‘data’.)

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
	| EOPChar1_1       annotation lexicalEndOfParse
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


-- | A lowercase character or underscore.
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
data AscSymbolBase lexicalExclamation lexicalHash lexicalDollar lexicalPercent lexicalAmpersand lexicalAsterisk lexicalPlus lexicalDot lexicalSlash lexicalLeftAngleBracket lexicalEquals lexicalRightAngleBracket lexicalQuestionMark lexicalAt lexicalBackslash lexicalCaret lexicalPipe lexicalHyphen lexicalTilde lexicalColon annotation fixpoint =
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
	| QuestionMarkAsciiSymbol      annotation lexicalQuestionMark
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
data AscSymbolSansNcBase lexicalExclamation lexicalHash lexicalDollar lexicalPercent lexicalAmpersand lexicalAsterisk lexicalPlus lexicalDot lexicalSlash lexicalLeftAngleBracket lexicalEquals lexicalRightAngleBracket lexicalQuestionMark lexicalAt lexicalBackslash lexicalCaret lexicalPipe lexicalTilde lexicalColon annotation fixpoint =
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
	| QuestionMarkAsciiSymbolSansNc      annotation lexicalQuestionMark
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

-- (Note: 'VarSymBase', with extra exclusions, appears later in this section.)

-- | A symbolic variable (lowercase-style) identifier name in a simplified version without extra exclusions.
data VarSymNoExtraExclusionsBase list varSymStart symbol annotation fixpoint =
	VarSymWhole annotation varSymStart (list symbol)

-- | A valid start to a symbolic variable (lowercase-style) identifier name.
data VarSymStartBase symbolSansColon annotation fixpoint =
	SymbolSansColonVarSymStart annotation symbolSansColon

-- (Note: 'ConSymBase', with extra exclusions, appears later in this section.)

-- | A symbolic constructor (capitalized-style) identifier name in a simplified version without extra exclusions.
data ConSymNoExtraExclusionsBase list conSymStart symbol annotation fixpoint =
	ConSymWhole annotation conSymStart (list symbol)

-- | A valid start to a symbolic constructor (capitalized-style) identifier
-- name.
data ConSymStartBase lexicalColon annotation fixpoint =
	ColonConSymStart annotation lexicalColon

-- | Reversed symbolic names.
--
-- Haskell2010 excludes these from 'ConSym's and these and 'dashes' from 'VarSym's.
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
data NameBase qvarid qconid qtycon qtycls qvarSym qconSym annotation fixpoint =
	  VaridName  annotation qvarid
	| ConidName  annotation qconid
	| TyconName  annotation qtycon
	| TyclsName  annotation qtycls
	| VarSymName annotation qvarSym
	| ConSymName annotation qconSym

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

-- $baseLexicalStructures
-- ‘lexicalFoo’ base structures belong here.  Normally these are either
-- individual characters or pseudo-foundational lexical structures that reduce
-- to sequences of individual charcaters.

-- Pseudo-foundational lexical structures.

-- $pseudoFoundationalLexicalStructures
-- Keyword aliases and numeric literal prefixes that just reduce to foundational lexical structures like
-- character representations.

data LexicalPseudoBase lexicalNonsymKeyword lexicalNonsymNonkeyword lexicalSymAlias lexicalAlias lexicalNumPrefix annotation fixpoint =
	  PseudoLexicalNonsymKeyword    annotation lexicalNonsymKeyword
	| PseudoLexicalNonsymNonkeyword annotation lexicalNonsymNonkeyword
	| PseudoLexicalSymAlias         annotation lexicalSymAlias
	| PseudoLexicalAlias            annotation lexicalAlias
	| PseudoLexicalNumPrefix        annotation lexicalNumPrefix

-- Non-symbolic keyword pseudo-lexical structures.

-- | A non-symbolic pseudo-lexical keyword.
data LexicalNonsymKeywordBase lexicalCase lexicalClass lexicalData lexicalDefault lexicalDeriving lexicalDo lexicalElse lexicalForeign lexicalIf lexicalImport lexicalIn lexicalInfix lexicalInfixl lexicalInfixr lexicalInstance lexicalLet lexicalModule lexicalNewtype lexicalOf lexicalThen lexicalType lexicalWhere annotation fixpoint =
	  CaseNonsymKeyword     annotation lexicalCase
	| ClassNonsymKeyword    annotation lexicalClass
	| DataNonsymKeyword     annotation lexicalData
	| DefaultNonsymKeyword  annotation lexicalDefault
	| DerivingNonsymKeyword annotation lexicalDeriving
	| DoNonsymKeyword       annotation lexicalDo
	| ElseNonsymKeyword     annotation lexicalElse
	| ForeignNonsymKeyword  annotation lexicalForeign
	| IfNonsymKeyword       annotation lexicalIf
	| ImportNonsymKeyword   annotation lexicalImport
	| InNonsymKeyword       annotation lexicalIn
	| InfixNonsymKeyword    annotation lexicalInfix
	| InfixlNonsymKeyword   annotation lexicalInfixl
	| InfixrNonsymKeyword   annotation lexicalInfixr
	| InstanceNonsymKeyword annotation lexicalInstance
	| LetNonsymKeyword      annotation lexicalLet
	| ModuleNonsymKeyword   annotation lexicalModule
	| NewtypeNonsymKeyword  annotation lexicalNewtype
	| OfNonsymKeyword       annotation lexicalOf
	| ThenNonsymKeyword     annotation lexicalThen
	| TypeNonsymKeyword     annotation lexicalType
	| WhereNonsymKeyword    annotation lexicalWhere

-- | The ‘case’ keyword.
data LexicalCaseBase lexicalCLower lexicalALower lexicalSLower lexicalELower annotation fixpoint =
	PseudoLexicalCase annotation lexicalCLower lexicalALower lexicalSLower lexicalELower

-- | The ‘class’ keyword.
data LexicalClassBase lexicalCLower lexicalLLower lexicalALower lexicalSLower annotation fixpoint =
	PseudoLexicalClass annotation lexicalCLower lexicalLLower lexicalALower lexicalSLower lexicalSLower

-- | The ‘data’ keyword.
data LexicalDataBase lexicalDLower lexicalALower lexicalTLower annotation fixpoint =
	PseudoLexicalData annotation lexicalDLower lexicalALower lexicalTLower lexicalALower

-- | The ‘default’ keyword.
data LexicalDefaultBase lexicalDLower lexicalELower lexicalFLower lexicalALower lexicalULower lexicalLLower lexicalTLower annotation fixpoint =
	PseudoLexicalDefault annotation lexicalDLower lexicalELower lexicalFLower lexicalALower lexicalULower lexicalLLower lexicalTLower

-- | The ‘deriving’ keyword.
data LexicalDerivingBase lexicalDLower lexicalELower lexicalRLower lexicalILower lexicalVLower lexicalNLower lexicalGLower annotation fixpoint =
	PseudoLexicalDeriving annotation lexicalDLower lexicalELower lexicalRLower lexicalILower lexicalVLower lexicalILower lexicalNLower lexicalGLower

-- | The ‘do’ keyword.
data LexicalDoBase lexicalDLower lexicalOLower annotation fixpoint =
	PseudoLexicalDo annotation lexicalDLower lexicalOLower

-- | The ‘else’ keyword.
data LexicalElseBase lexicalELower lexicalLLower lexicalSLower annotation fixpoint =
	PseudoLexicalElse annotation lexicalELower lexicalLLower lexicalSLower lexicalELower

-- | The ‘foreign’ keyword.
data LexicalForeignBase lexicalFLower lexicalOLower lexicalRLower lexicalELower lexicalILower lexicalGLower lexicalNLower annotation fixpoint =
	PseudoLexicalForeign annotation lexicalFLower lexicalOLower lexicalRLower lexicalELower lexicalILower lexicalGLower lexicalNLower

-- | The ‘if’ keyword.
data LexicalIfBase lexicalILower lexicalFLower annotation fixpoint =
	PseudoLexicalIf annotation lexicalILower lexicalFLower

-- | The ‘import’ keyword.
data LexicalImportBase lexicalILower lexicalMLower lexicalPLower lexicalOLower lexicalRLower lexicalTLower annotation fixpoint =
	PseudoLexicalImport annotation lexicalILower lexicalMLower lexicalPLower lexicalOLower lexicalRLower lexicalTLower

-- | The ‘in’ keyword.
data LexicalInBase lexicalILower lexicalNLower annotation fixpoint =
	PseudoLexicalIn annotation lexicalILower lexicalNLower

-- | The ‘infix’ keyword.
data LexicalInfixBase lexicalILower lexicalNLower lexicalFLower lexicalXLower annotation fixpoint =
	PseudoLexicalInfix annotation lexicalILower lexicalNLower lexicalFLower lexicalILower lexicalXLower

-- | The ‘infixl’ keyword.
data LexicalInfixlBase lexicalILower lexicalNLower lexicalFLower lexicalXLower lexicalLLower annotation fixpoint =
	PseudoLexicalInfixl annotation lexicalILower lexicalNLower lexicalFLower lexicalILower lexicalXLower lexicalLLower

-- | The ‘infixr’ keyword.
data LexicalInfixrBase lexicalILower lexicalNLower lexicalFLower lexicalXLower lexicalRLower annotation fixpoint =
	PseudoLexicalInfixr annotation lexicalILower lexicalNLower lexicalFLower lexicalILower lexicalXLower lexicalRLower

-- | The ‘instance’ keyword.
data LexicalInstanceBase lexicalILower lexicalNLower lexicalSLower lexicalTLower lexicalALower lexicalCLower lexicalELower annotation fixpoint =
	PseudoLexicalInstance annotation lexicalILower lexicalNLower lexicalSLower lexicalTLower lexicalALower lexicalNLower lexicalCLower lexicalELower

-- | The ‘let’ keyword.
data LexicalLetBase lexicalLLower lexicalELower lexicalTLower annotation fixpoint =
	PseudoLexicalLet annotation lexicalLLower lexicalELower lexicalTLower

-- | The ‘module’ keyword.
data LexicalModuleBase lexicalMLower lexicalOLower lexicalDLower lexicalULower lexicalLLower lexicalELower annotation fixpoint =
	PseudoLexicalModule annotation lexicalMLower lexicalOLower lexicalDLower lexicalULower lexicalLLower lexicalELower

-- | The ‘newtype’ keyword.
data LexicalNewtypeBase lexicalNLower lexicalELower lexicalWLower lexicalTLower lexicalYLower lexicalPLower annotation fixpoint =
	PseudoLexicalNewtype annotation lexicalNLower lexicalELower lexicalWLower lexicalTLower lexicalYLower lexicalPLower lexicalELower

-- | The ‘of’ keyword.
data LexicalOfBase lexicalOLower lexicalFLower annotation fixpoint =
	PseudoLexicalOf annotation lexicalOLower lexicalFLower

-- | The ‘then’ keyword.
data LexicalThenBase lexicalTLower lexicalHLower lexicalELower lexicalNLower annotation fixpoint =
	PseudoLexicalThen annotation lexicalTLower lexicalHLower lexicalELower lexicalNLower

-- | The ‘type’ keyword.
data LexicalTypeBase lexicalTLower lexicalYLower lexicalPLower lexicalELower annotation fixpoint =
	PseudoLexicalType annotation lexicalTLower lexicalYLower lexicalPLower lexicalELower

-- | The ‘where’ keyword.
data LexicalWhereBase lexicalWLower lexicalHLower lexicalELower lexicalRLower annotation fixpoint =
	PseudoLexicalWhere annotation lexicalWLower lexicalHLower lexicalELower lexicalRLower lexicalELower

-- Non-symbolic non-keyword pseudo-lexical structures.

-- | A non-symbolic pseudo-lexical non-keyword.
data LexicalNonsymNonkeywordBase lexicalAs lexicalHiding lexicalQualified annotation fixpoint =
	  AsNonsymNonkeyword        annotation lexicalAs
	| HidingNonsymNonkeyword    annotation lexicalHiding
	| QualifiedNonsymNonkeyword annotation lexicalQualified

-- | The ‘as’ word.
data LexicalAsBase lexemeALower lexemeSLower annotation fixpoint =
	PseudoLexicalAs annotation lexemeALower lexemeSLower

-- | The ‘hiding’ word.
data LexicalHidingBase lexemeHLower lexemeILower lexemeDLower lexemeNLower lexemeGLower annotation fixpoint =
	PseudoLexicalHiding annotation lexemeHLower lexemeILower lexemeDLower lexemeILower lexemeNLower lexemeGLower

-- | The ‘qualified’ word.
data LexicalQualifiedBase lexemeHLower lexemeILower lexemeDLower lexemeNLower lexemeGLower annotation fixpoint =
	PseudoLexicalQualified annotation lexemeHLower lexemeILower lexemeDLower lexemeILower lexemeNLower lexemeGLower

-- Symbolic alias pseudo-lexical structures.

-- | A symbolic pseudo-lexical alias.
data LexicalSymAliasBase lexicalDotDot lexicalDoubleColon lexicalDoubleRightArrow lexicalLeftArrow lexicalRightArrow annotation fixpoint =
	  DotDotSymAlias           annotation lexicalDotDot
	| DoubleColonSymAlias      annotation lexicalDoubleColon
	| DoubleRightArrowSymAlias annotation lexicalDoubleRightArrow
	| LeftArrowSymAlias        annotation lexicalLeftArrow
	| RightArrowSymAlias       annotation lexicalRightArrow

-- | The ‘..’ symbol sequence.
data LexicalDotDotBase lexicalDot annotation fixpoint =
	PseudoLexicalDotDot annotation lexicalDot lexicalDot

-- | The ‘::’ symbol sequence.
data LexicalDoubleColonBase lexicalColon annotation fixpoint =
	PseudoLexicalDoubleColon annotation lexicalColon lexicalColon

-- | The ‘=>’ symbol sequence.
data LexicalDoubleRightArrowBase lexicalEquals lexicalRightAngleBracket annotation fixpoint =
	PseudoLexicalDoubleRightArrow annotation lexicalEquals lexicalRightAngleBracket

-- | The ‘<-’ symbol sequence.
data LexicalLeftArrowBase lexicalLeftAngleBracket lexicalHyphen annotation fixpoint =
	PseudoLexicalLeftArrow annotation lexicalLeftAngleBracket lexicalHyphen

-- | The ‘->’ symbol sequence.
data LexicalRightArrowBase lexicalLeftAngleBracket lexicalHyphen lexicalRightAngleBracket annotation fixpoint =
	PseudoLexicalRightArrow annotation lexicalHyphen lexicalRightAngleBracket

-- Alias pseudo-lexical structures.

-- | A pseudo-lexical alias.
data LexicalAliasBase lexicalSpace lexicalMinus lexicalAsciiLambda annotation fixpoint =
	  SpaceAlias       annotation lexicalSpace
	| MinusAlias       annotation lexicalMinus
	| AsciiLambdaAlias annotation lexicalAsciiLambda

-- | An alias for the space character.
data LexicalSpaceBase lexemeSP annotation fixpoint =
	PseudoLexicalSpace annotation lexemeSP

-- | An alias for the hyphen character.
data LexicalMinusBase lexemeHyphen annotation fixpoint =
	PseudoLexicalMinus annotation lexemeHyphen

-- | An alias for the backslash character.
data LexicalAsciiLambdaBase lexemeBackslash annotation fixpoint =
	PseudoLexicalAsciiLambda annotation lexemeBackslash

-- Non-symbolic numeric literal prefix pseudo-lexical structures.

-- | A pseudo-lexical alias.
data LexicalNumPrefixBase lexical0o lexical0O lexical0x lexical0X annotation fixpoint =
	  N0oNumPrefix annotation lexical0o
	| N0ONumPrefix annotation lexical0O
	| N0xNumPrefix annotation lexical0x
	| N0XNumPrefix annotation lexical0X

-- | The ‘0o’ octal literal prefix.
data Lexical0oBase lexical0 lexicalOLower annotation fixpoint =
	PseudoLexical0o annotation lexical0 lexicalOLower

-- | The ‘0O’ octal literal prefix.
data Lexical0OBase lexical0 lexicalO annotation fixpoint =
	PseudoLexical0O annotation lexical0 lexicalO

-- | The ‘0x’ hexadecimal literal prefix.
data Lexical0xBase lexical0 lexicalXLower annotation fixpoint =
	PseudoLexical0x annotation lexical0 lexicalXLower

-- | The ‘0X’ hexadecimal literal prefix.
data Lexical0XBase lexical0 lexicalX annotation fixpoint =
	PseudoLexical0X annotation lexical0 lexicalX

-- Lexical foundation.

-- $lexicalFoundation
-- The terminals of Simple Haskell2010 syntax, for both grammar and lexer.
-- This is mostly just individual characters, with a few exceptions, including:
-- - 'lexicalEndOfParse' certifies the end of parsable input.
-- - ‘lexicalFooSansBar’ patterns are groups of characters with exclusions, and
--   shift handling of exclusions elsewhere.
-- - Groups of characters, with greater complexity or additional dependencies.
--   This is especially suitable e.g. for unicode groups, as we can handle unicode
--   details elsewhere.

-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalEndOfParse’: index (for 138) 0.
--
-- Requires zero- and successor- types and type constructors, in order to
-- obtain a type-level representation of ‘index 0’.  The first of the two
-- arguments to ‘lexicalTypeIndexer’ represents the number of keys that appear
-- after this one, and the second argument is the index itself.  Thus adding
-- the two numbers together plus 1 gets you the length of a list of keys.
--
-- There are 139 keys, so the sum should total 138.
--
-- ‘lexicalTypeIndexer (LexicalEndOfParseKey z s)’ is expected to correspond to
-- the actual ‘lexicalEndOfParseKey’ type itself.
type LexicalEndOfParseKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) z

-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalUnicodeDigitKey’: index (for 137) 1.
type LexicalUnicodeDigitKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s z)
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalUnicodeLargeKey’: index (for 136) 2.
type LexicalUnicodeLargeKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s z))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalUnicodeSmallKey’: index (for 135) 3.
type LexicalUnicodeSmallKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s z)))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalUnicodeSymbolKey’: index (for 134) 4.
type LexicalUnicodeSymbolKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s z))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalUnicodeWhitespaceCharKey’: index (for 133) 5.
type LexicalUnicodeWhitespaceCharKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s z)))))

-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalUnicodeSmallSansAscUnderscoreKey’: index (for 132) 6.
type LexicalUnicodeSmallSansAscUnderscoreKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s z))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalUnicodeSymbolSansSpecialUnderscoreDoubleQuoteSingleQuoteKey’: index (for 131) 7.
type LexicalUnicodeSymbolSansSpecialUnderscoreDoubleQuoteSingleQuoteKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s z)))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalUnicodeSymbolSansSpecialUnderscoreDoubleQuoteSingleQuoteAsciiKey’: index (for 130) 8.
type LexicalUnicodeSymbolSansSpecialUnderscoreDoubleQuoteSingleQuoteAsciiKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s z))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalUnicodeSymbolSansSpecialUnderscoreDoubleQuoteSingleQuoteColonKey’: index (for 129) 9.
type LexicalUnicodeSymbolSansSpecialUnderscoreDoubleQuoteSingleQuoteColonKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s z)))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalUnicodeSymbolSansSpecialUnderscoreDoubleQuoteSingleQuoteHyphenKey’: index (for 128) 10.
type LexicalUnicodeSymbolSansSpecialUnderscoreDoubleQuoteSingleQuoteHyphenKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s z))))))))))

-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalNULKey’: index (for 127) 11.
type LexicalNULKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s z)))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalSOHKey’: index (for 126) 12.
type LexicalSOHKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalSTXKey’: index (for 125) 13.
type LexicalSTXKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalETXKey’: index (for 124) 14.
type LexicalETXKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalEOTKey’: index (for 123) 15.
type LexicalEOTKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalENQKey’: index (for 122) 16.
type LexicalENQKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalACKKey’: index (for 121) 17.
type LexicalACKKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalBELKey’: index (for 120) 18.
type LexicalBELKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalBSKey’: index (for 119) 19.
type LexicalBSKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalHTKey’: index (for 118) 20.
type LexicalHTKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalLFKey’: index (for 117) 21.
type LexicalLFKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalVTKey’: index (for 116) 22.
type LexicalVTKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalFFKey’: index (for 115) 23.
type LexicalFFKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalCRKey’: index (for 114) 24.
type LexicalCRKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalSOKey’: index (for 113) 25.
type LexicalSOKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalSIKey’: index (for 112) 26.
type LexicalSIKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalDLEKey’: index (for 111) 27.
type LexicalDLEKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalDC1Key’: index (for 110) 28.
type LexicalDC1KeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalDC2Key’: index (for 109) 29.
type LexicalDC2KeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalDC3Key’: index (for 108) 30.
type LexicalDC3KeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalDC4Key’: index (for 107) 31.
type LexicalDC4KeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalNAKKey’: index (for 106) 32.
type LexicalNAKKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalSYNKey’: index (for 105) 33.
type LexicalSYNKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalETBKey’: index (for 104) 34.
type LexicalETBKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalCANKey’: index (for 103) 35.
type LexicalCANKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalEMKey’: index (for 102) 36.
type LexicalEMKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalSUBKey’: index (for 101) 37.
type LexicalSUBKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalESCKey’: index (for 100) 38.
type LexicalESCKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalFSKey’: index (for 99) 39.
type LexicalFSKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalGSKey’: index (for 98) 40.
type LexicalGSKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalRSKey’: index (for 97) 41.
type LexicalRSKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalUSKey’: index (for 96) 42.
type LexicalUSKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))

-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalSPKey’: index (for 95) 43.
type LexicalSPKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalExclamationKey’: index (for 94) 44.
type LexicalExclamationKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalDoubleQuoteKey’: index (for 93) 45.
type LexicalDoubleQuoteKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalHashKey’: index (for 92) 46.
type LexicalHashKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalDollarKey’: index (for 91) 47.
type LexicalDollarKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalPercentKey’: index (for 90) 48.
type LexicalPercentKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalAmpersandKey’: index (for 89) 49.
type LexicalAmpersandKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalSingleQuoteKey’: index (for 88) 50.
type LexicalSingleQuoteKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalLeftParenthesisKey’: index (for 87) 51.
type LexicalLeftParenthesisKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalRightParenthesisKey’: index (for 86) 52.
type LexicalRightParenthesisKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalAsteriskKey’: index (for 85) 53.
type LexicalAsteriskKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalPlusKey’: index (for 84) 54.
type LexicalPlusKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalCommaKey’: index (for 83) 55.
type LexicalCommaKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalHyphenKey’: index (for 82) 56.
type LexicalHyphenKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalDotKey’: index (for 81) 57.
type LexicalDotKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalSlashKey’: index (for 80) 58.
type LexicalSlashKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexical0Key’: index (for 79) 59.
type Lexical0KeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexical1Key’: index (for 78) 60.
type Lexical1KeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexical2Key’: index (for 77) 61.
type Lexical2KeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexical3Key’: index (for 76) 62.
type Lexical3KeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexical4Key’: index (for 75) 63.
type Lexical4KeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexical5Key’: index (for 74) 64.
type Lexical5KeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexical6Key’: index (for 73) 65.
type Lexical6KeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexical7Key’: index (for 72) 66.
type Lexical7KeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexical8Key’: index (for 71) 67.
type Lexical8KeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexical9Key’: index (for 70) 68.
type Lexical9KeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalColonKey’: index (for 69) 69.
type LexicalColonKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalSemicolonKey’: index (for 68) 70.
type LexicalSemicolonKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalLeftAngleBracketKey’: index (for 67) 71.
type LexicalLeftAngleBracketKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalEqualsKey’: index (for 66) 72.
type LexicalEqualsKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalRightAngleBracketKey’: index (for 65) 73.
type LexicalRightAngleBracketKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalQuestionMarkKey’: index (for 64) 74.
type LexicalQuestionMarkKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalAtKey’: index (for 63) 75.
type LexicalAtKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalAKey’: index (for 62) 76.
type LexicalAKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalBKey’: index (for 61) 77.
type LexicalBKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalCKey’: index (for 60) 78.
type LexicalCKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalDKey’: index (for 59) 79.
type LexicalDKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalEKey’: index (for 58) 80.
type LexicalEKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalFKey’: index (for 57) 81.
type LexicalFKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalGKey’: index (for 56) 82.
type LexicalGKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalHKey’: index (for 55) 83.
type LexicalHKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalIKey’: index (for 54) 84.
type LexicalIKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalJKey’: index (for 53) 85.
type LexicalJKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalKKey’: index (for 52) 86.
type LexicalKKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalLKey’: index (for 51) 87.
type LexicalLKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalMKey’: index (for 50) 88.
type LexicalMKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalNKey’: index (for 49) 89.
type LexicalNKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalOKey’: index (for 48) 90.
type LexicalOKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalPKey’: index (for 47) 91.
type LexicalPKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalQKey’: index (for 46) 92.
type LexicalQKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalRKey’: index (for 45) 93.
type LexicalRKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalSKey’: index (for 44) 94.
type LexicalSKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalTKey’: index (for 43) 95.
type LexicalTKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalUKey’: index (for 42) 96.
type LexicalUKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalVKey’: index (for 41) 97.
type LexicalVKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalWKey’: index (for 40) 98.
type LexicalWKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalXKey’: index (for 39) 99.
type LexicalXKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalYKey’: index (for 38) 100.
type LexicalYKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalZKey’: index (for 37) 101.
type LexicalZKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalLeftBracketKey’: index (for 36) 102.
type LexicalLeftBracketKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalBackslashKey’: index (for 35) 103.
type LexicalBackslashKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalRightBracketKey’: index (for 34) 104.
type LexicalRightBracketKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalCaretKey’: index (for 33) 105.
type LexicalCaretKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalUnderscoreKey’: index (for 32) 106.
type LexicalUnderscoreKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalBacktickKey’: index (for 31) 107.
type LexicalBacktickKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalALowerKey’: index (for 30) 108.
type LexicalALowerKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalBLowerKey’: index (for 29) 109.
type LexicalBLowerKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalCLowerKey’: index (for 28) 110.
type LexicalCLowerKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalDLowerKey’: index (for 27) 111.
type LexicalDLowerKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalELowerKey’: index (for 26) 112.
type LexicalELowerKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalFLowerKey’: index (for 25) 113.
type LexicalFLowerKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalGLowerKey’: index (for 24) 114.
type LexicalGLowerKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalHLowerKey’: index (for 23) 115.
type LexicalHLowerKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalILowerKey’: index (for 22) 116.
type LexicalILowerKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalJLowerKey’: index (for 21) 117.
type LexicalJLowerKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalKLowerKey’: index (for 20) 118.
type LexicalKLowerKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalLLowerKey’: index (for 19) 119.
type LexicalLLowerKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalMLowerKey’: index (for 18) 120.
type LexicalMLowerKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalNLowerKey’: index (for 17) 121.
type LexicalNLowerKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalOLowerKey’: index (for 16) 122.
type LexicalOLowerKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalPLowerKey’: index (for 15) 123.
type LexicalPLowerKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalQLowerKey’: index (for 14) 124.
type LexicalQLowerKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalRLowerKey’: index (for 13) 125.
type LexicalRLowerKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalSLowerKey’: index (for 12) 126.
type LexicalSLowerKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalTLowerKey’: index (for 11) 127.
type LexicalTLowerKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s z))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalULowerKey’: index (for 10) 128.
type LexicalULowerKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s z)))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalVLowerKey’: index (for 9) 129.
type LexicalVLowerKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s z))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalWLowerKey’: index (for 8) 130.
type LexicalWLowerKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s z)))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalXLowerKey’: index (for 7) 131.
type LexicalXLowerKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s z))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalYLowerKey’: index (for 6) 132.
type LexicalYLowerKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s (s z)))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalZLowerKey’: index (for 5) 133.
type LexicalZLowerKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s (s z))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalLeftBraceKey’: index (for 4) 134.
type LexicalLeftBraceKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s (s z)))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalPipeKey’: index (for 3) 135.
type LexicalPipeKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s (s z))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalRightBraceKey’: index (for 2) 136.
type LexicalRightBraceKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s (s z)) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalTildeKey’: index (for 1) 137.
type LexicalTildeKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons (s z) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalDELKey’: index (for 0) 138.
type LexicalDELKeyBase lexicalTypeIndexerKeyCons z s = lexicalTypeIndexerKeyCons z (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

-- | A record of the terminals of the Simple Haskell2010 syntax.
--
-- This record is like a type class, and values of this record provide a
-- complete foundation to build AST types, as they ultimately reduce to types
-- referenced by fields in this record.  (Credit: ‘Scrap your type classes’.)
--
-- The terminals are mostly individual characters, with a few exceptions such
-- as groups, exclusions, and certifications.
--
-- Foundations can support type representations too.
data LexicalFoundationBase lexicalEndOfParseKey lexicalUnicodeDigitKey lexicalUnicodeLargeKey lexicalUnicodeSmallKey lexicalUnicodeSymbolKey lexicalUnicodeWhitespaceCharKey lexicalUnicodeSmallSansAscUnderscoreKey lexicalUnicodeSymbolSansSpecialUnderscoreDoubleQuoteSingleQuoteKey lexicalUnicodeSymbolSansSpecialUnderscoreDoubleQuoteSingleQuoteAsciiKey lexicalUnicodeSymbolSansSpecialUnderscoreDoubleQuoteSingleQuoteColonKey lexicalUnicodeSymbolSansSpecialUnderscoreDoubleQuoteSingleQuoteHyphenKey lexicalNULKey lexicalSOHKey lexicalSTXKey lexicalETXKey lexicalEOTKey lexicalENQKey lexicalACKKey lexicalBELKey lexicalBSKey lexicalHTKey lexicalLFKey lexicalVTKey lexicalFFKey lexicalCRKey lexicalSOKey lexicalSIKey lexicalDLEKey lexicalDC1Key lexicalDC2Key lexicalDC3Key lexicalDC4Key lexicalNAKKey lexicalSYNKey lexicalETBKey lexicalCANKey lexicalEMKey lexicalSUBKey lexicalESCKey lexicalFSKey lexicalGSKey lexicalRSKey lexicalUSKey lexicalSPKey lexicalExclamationKey lexicalDoubleQuoteKey lexicalHashKey lexicalDollarKey lexicalPercentKey lexicalAmpersandKey lexicalSingleQuoteKey lexicalLeftParenthesisKey lexicalRightParenthesisKey lexicalAsteriskKey lexicalPlusKey lexicalCommaKey lexicalHyphenKey lexicalDotKey lexicalSlashKey lexical0Key lexical1Key lexical2Key lexical3Key lexical4Key lexical5Key lexical6Key lexical7Key lexical8Key lexical9Key lexicalColonKey lexicalSemicolonKey lexicalLeftAngleBracketKey lexicalEqualsKey lexicalRightAngleBracketKey lexicalQuestionMarkKey lexicalAtKey lexicalAKey lexicalBKey lexicalCKey lexicalDKey lexicalEKey lexicalFKey lexicalGKey lexicalHKey lexicalIKey lexicalJKey lexicalKKey lexicalLKey lexicalMKey lexicalNKey lexicalOKey lexicalPKey lexicalQKey lexicalRKey lexicalSKey lexicalTKey lexicalUKey lexicalVKey lexicalWKey lexicalXKey lexicalYKey lexicalZKey lexicalLeftBracketKey lexicalBackslashKey lexicalRightBracketKey lexicalCaretKey lexicalUnderscoreKey lexicalBacktickKey lexicalALowerKey lexicalBLowerKey lexicalCLowerKey lexicalDLowerKey lexicalELowerKey lexicalFLowerKey lexicalGLowerKey lexicalHLowerKey lexicalILowerKey lexicalJLowerKey lexicalKLowerKey lexicalLLowerKey lexicalMLowerKey lexicalNLowerKey lexicalOLowerKey lexicalPLowerKey lexicalQLowerKey lexicalRLowerKey lexicalSLowerKey lexicalTLowerKey lexicalULowerKey lexicalVLowerKey lexicalWLowerKey lexicalXLowerKey lexicalYLowerKey lexicalZLowerKey lexicalLeftBraceKey lexicalPipeKey lexicalRightBraceKey lexicalTildeKey lexicalDELKey lexicalTypeIndexerKeyCons lexicalTypeIndexer typeValue zero succ annotation fixpoint =
	LexicalFoundationPrimitives {
		_lexicalFoundationPrimitives_lexicalEndOfParse                                                     :: typeValue (lexicalTypeIndexer (lexicalEndOfParseKey                                                     lexicalTypeIndexerKeyCons zero succ)),

		_lexicalFoundationPrimitives_lexicalUnicodeDigit                                                   :: typeValue (lexicalTypeIndexer (lexicalUnicodeDigitKey                                                   lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalUnicodeLarge                                                   :: typeValue (lexicalTypeIndexer (lexicalUnicodeLargeKey                                                   lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalUnicodeSmall                                                   :: typeValue (lexicalTypeIndexer (lexicalUnicodeSmallKey                                                   lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalUnicodeSymbol                                                  :: typeValue (lexicalTypeIndexer (lexicalUnicodeSymbolKey                                                  lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalUnicodeWhitespaceChar                                          :: typeValue (lexicalTypeIndexer (lexicalUnicodeWhitespaceCharKey                                          lexicalTypeIndexerKeyCons zero succ)),

		_lexicalFoundationPrimitives_lexicalUnicodeSmallSansAscUnderscore                                  :: typeValue (lexicalTypeIndexer (lexicalUnicodeSmallSansAscUnderscoreKey                                  lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalUnicodeSymbolSansSpecialUnderscoreDoubleQuoteSingleQuote       :: typeValue (lexicalTypeIndexer (lexicalUnicodeSymbolSansSpecialUnderscoreDoubleQuoteSingleQuoteKey       lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalUnicodeSymbolSansSpecialUnderscoreDoubleQuoteSingleQuoteAscii  :: typeValue (lexicalTypeIndexer (lexicalUnicodeSymbolSansSpecialUnderscoreDoubleQuoteSingleQuoteAsciiKey  lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalUnicodeSymbolSansSpecialUnderscoreDoubleQuoteSingleQuoteColon  :: typeValue (lexicalTypeIndexer (lexicalUnicodeSymbolSansSpecialUnderscoreDoubleQuoteSingleQuoteColonKey  lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalUnicodeSymbolSansSpecialUnderscoreDoubleQuoteSingleQuoteHyphen :: typeValue (lexicalTypeIndexer (lexicalUnicodeSymbolSansSpecialUnderscoreDoubleQuoteSingleQuoteHyphenKey lexicalTypeIndexerKeyCons zero succ)),

		_lexicalFoundationPrimitives_lexicalNUL                                                            :: typeValue (lexicalTypeIndexer (lexicalNULKey                                                            lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalSOH                                                            :: typeValue (lexicalTypeIndexer (lexicalSOHKey                                                            lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalSTX                                                            :: typeValue (lexicalTypeIndexer (lexicalSTXKey                                                            lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalETX                                                            :: typeValue (lexicalTypeIndexer (lexicalETXKey                                                            lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalEOT                                                            :: typeValue (lexicalTypeIndexer (lexicalEOTKey                                                            lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalENQ                                                            :: typeValue (lexicalTypeIndexer (lexicalENQKey                                                            lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalACK                                                            :: typeValue (lexicalTypeIndexer (lexicalACKKey                                                            lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalBEL                                                            :: typeValue (lexicalTypeIndexer (lexicalBELKey                                                            lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalBS                                                             :: typeValue (lexicalTypeIndexer (lexicalBSKey                                                             lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalHT                                                             :: typeValue (lexicalTypeIndexer (lexicalHTKey                                                             lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalLF                                                             :: typeValue (lexicalTypeIndexer (lexicalLFKey                                                             lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalVT                                                             :: typeValue (lexicalTypeIndexer (lexicalVTKey                                                             lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalFF                                                             :: typeValue (lexicalTypeIndexer (lexicalFFKey                                                             lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalCR                                                             :: typeValue (lexicalTypeIndexer (lexicalCRKey                                                             lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalSO                                                             :: typeValue (lexicalTypeIndexer (lexicalSOKey                                                             lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalSI                                                             :: typeValue (lexicalTypeIndexer (lexicalSIKey                                                             lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalDLE                                                            :: typeValue (lexicalTypeIndexer (lexicalDLEKey                                                            lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalDC1                                                            :: typeValue (lexicalTypeIndexer (lexicalDC1Key                                                            lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalDC2                                                            :: typeValue (lexicalTypeIndexer (lexicalDC2Key                                                            lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalDC3                                                            :: typeValue (lexicalTypeIndexer (lexicalDC3Key                                                            lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalDC4                                                            :: typeValue (lexicalTypeIndexer (lexicalDC4Key                                                            lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalNAK                                                            :: typeValue (lexicalTypeIndexer (lexicalNAKKey                                                            lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalSYN                                                            :: typeValue (lexicalTypeIndexer (lexicalSYNKey                                                            lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalETB                                                            :: typeValue (lexicalTypeIndexer (lexicalETBKey                                                            lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalCAN                                                            :: typeValue (lexicalTypeIndexer (lexicalCANKey                                                            lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalEM                                                             :: typeValue (lexicalTypeIndexer (lexicalEMKey                                                             lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalSUB                                                            :: typeValue (lexicalTypeIndexer (lexicalSUBKey                                                            lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalESC                                                            :: typeValue (lexicalTypeIndexer (lexicalESCKey                                                            lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalFS                                                             :: typeValue (lexicalTypeIndexer (lexicalFSKey                                                             lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalGS                                                             :: typeValue (lexicalTypeIndexer (lexicalGSKey                                                             lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalRS                                                             :: typeValue (lexicalTypeIndexer (lexicalRSKey                                                             lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalUS                                                             :: typeValue (lexicalTypeIndexer (lexicalUSKey                                                             lexicalTypeIndexerKeyCons zero succ)),

		_lexicalFoundationPrimitives_lexicalSP                                                             :: typeValue (lexicalTypeIndexer (lexicalSPKey                                                             lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalExclamation                                                    :: typeValue (lexicalTypeIndexer (lexicalExclamationKey                                                    lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalDoubleQuote                                                    :: typeValue (lexicalTypeIndexer (lexicalDoubleQuoteKey                                                    lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalHash                                                           :: typeValue (lexicalTypeIndexer (lexicalHashKey                                                           lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalDollar                                                         :: typeValue (lexicalTypeIndexer (lexicalDollarKey                                                         lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalPercent                                                        :: typeValue (lexicalTypeIndexer (lexicalPercentKey                                                        lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalAmpersand                                                      :: typeValue (lexicalTypeIndexer (lexicalAmpersandKey                                                      lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalSingleQuote                                                    :: typeValue (lexicalTypeIndexer (lexicalSingleQuoteKey                                                    lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalLeftParenthesis                                                :: typeValue (lexicalTypeIndexer (lexicalLeftParenthesisKey                                                lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalRightParenthesis                                               :: typeValue (lexicalTypeIndexer (lexicalRightParenthesisKey                                               lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalAsterisk                                                       :: typeValue (lexicalTypeIndexer (lexicalAsteriskKey                                                       lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalPlus                                                           :: typeValue (lexicalTypeIndexer (lexicalPlusKey                                                           lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalComma                                                          :: typeValue (lexicalTypeIndexer (lexicalCommaKey                                                          lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalHyphen                                                         :: typeValue (lexicalTypeIndexer (lexicalHyphenKey                                                         lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalDot                                                            :: typeValue (lexicalTypeIndexer (lexicalDotKey                                                            lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalSlash                                                          :: typeValue (lexicalTypeIndexer (lexicalSlashKey                                                          lexicalTypeIndexerKeyCons zero succ)),

		_lexicalFoundationPrimitives_lexical0                                                              :: typeValue (lexicalTypeIndexer (lexical0Key                                                              lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexical1                                                              :: typeValue (lexicalTypeIndexer (lexical1Key                                                              lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexical2                                                              :: typeValue (lexicalTypeIndexer (lexical2Key                                                              lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexical3                                                              :: typeValue (lexicalTypeIndexer (lexical3Key                                                              lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexical4                                                              :: typeValue (lexicalTypeIndexer (lexical4Key                                                              lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexical5                                                              :: typeValue (lexicalTypeIndexer (lexical5Key                                                              lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexical6                                                              :: typeValue (lexicalTypeIndexer (lexical6Key                                                              lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexical7                                                              :: typeValue (lexicalTypeIndexer (lexical7Key                                                              lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexical8                                                              :: typeValue (lexicalTypeIndexer (lexical8Key                                                              lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexical9                                                              :: typeValue (lexicalTypeIndexer (lexical9Key                                                              lexicalTypeIndexerKeyCons zero succ)),

		_lexicalFoundationPrimitives_lexicalColon                                                          :: typeValue (lexicalTypeIndexer (lexicalColonKey                                                          lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalSemicolon                                                      :: typeValue (lexicalTypeIndexer (lexicalSemicolonKey                                                      lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalLeftAngleBracket                                               :: typeValue (lexicalTypeIndexer (lexicalLeftAngleBracketKey                                               lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalEquals                                                         :: typeValue (lexicalTypeIndexer (lexicalEqualsKey                                                         lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalRightAngleBracket                                              :: typeValue (lexicalTypeIndexer (lexicalRightAngleBracketKey                                              lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalQuestionMark                                                   :: typeValue (lexicalTypeIndexer (lexicalQuestionMarkKey                                                   lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalAt                                                             :: typeValue (lexicalTypeIndexer (lexicalAtKey                                                             lexicalTypeIndexerKeyCons zero succ)),

		_lexicalFoundationPrimitives_lexicalA                                                              :: typeValue (lexicalTypeIndexer (lexicalAKey                                                              lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalB                                                              :: typeValue (lexicalTypeIndexer (lexicalBKey                                                              lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalC                                                              :: typeValue (lexicalTypeIndexer (lexicalCKey                                                              lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalD                                                              :: typeValue (lexicalTypeIndexer (lexicalDKey                                                              lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalE                                                              :: typeValue (lexicalTypeIndexer (lexicalEKey                                                              lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalF                                                              :: typeValue (lexicalTypeIndexer (lexicalFKey                                                              lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalG                                                              :: typeValue (lexicalTypeIndexer (lexicalGKey                                                              lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalH                                                              :: typeValue (lexicalTypeIndexer (lexicalHKey                                                              lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalI                                                              :: typeValue (lexicalTypeIndexer (lexicalIKey                                                              lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalJ                                                              :: typeValue (lexicalTypeIndexer (lexicalJKey                                                              lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalK                                                              :: typeValue (lexicalTypeIndexer (lexicalKKey                                                              lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalL                                                              :: typeValue (lexicalTypeIndexer (lexicalLKey                                                              lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalM                                                              :: typeValue (lexicalTypeIndexer (lexicalMKey                                                              lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalN                                                              :: typeValue (lexicalTypeIndexer (lexicalNKey                                                              lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalO                                                              :: typeValue (lexicalTypeIndexer (lexicalOKey                                                              lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalP                                                              :: typeValue (lexicalTypeIndexer (lexicalPKey                                                              lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalQ                                                              :: typeValue (lexicalTypeIndexer (lexicalQKey                                                              lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalR                                                              :: typeValue (lexicalTypeIndexer (lexicalRKey                                                              lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalS                                                              :: typeValue (lexicalTypeIndexer (lexicalSKey                                                              lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalT                                                              :: typeValue (lexicalTypeIndexer (lexicalTKey                                                              lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalU                                                              :: typeValue (lexicalTypeIndexer (lexicalUKey                                                              lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalV                                                              :: typeValue (lexicalTypeIndexer (lexicalVKey                                                              lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalW                                                              :: typeValue (lexicalTypeIndexer (lexicalWKey                                                              lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalX                                                              :: typeValue (lexicalTypeIndexer (lexicalXKey                                                              lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalY                                                              :: typeValue (lexicalTypeIndexer (lexicalYKey                                                              lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalZ                                                              :: typeValue (lexicalTypeIndexer (lexicalZKey                                                              lexicalTypeIndexerKeyCons zero succ)),

		_lexicalFoundationPrimitives_lexicalLeftBracket                                                    :: typeValue (lexicalTypeIndexer (lexicalLeftBracketKey                                                    lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalBackslash                                                      :: typeValue (lexicalTypeIndexer (lexicalBackslashKey                                                      lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalRightBracket                                                   :: typeValue (lexicalTypeIndexer (lexicalRightBracketKey                                                   lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalCaret                                                          :: typeValue (lexicalTypeIndexer (lexicalCaretKey                                                          lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalUnderscore                                                     :: typeValue (lexicalTypeIndexer (lexicalUnderscoreKey                                                     lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalBacktick                                                       :: typeValue (lexicalTypeIndexer (lexicalBacktickKey                                                       lexicalTypeIndexerKeyCons zero succ)),

		_lexicalFoundationPrimitives_lexicalALower                                                         :: typeValue (lexicalTypeIndexer (lexicalALowerKey                                                         lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalBLower                                                         :: typeValue (lexicalTypeIndexer (lexicalBLowerKey                                                         lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalCLower                                                         :: typeValue (lexicalTypeIndexer (lexicalCLowerKey                                                         lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalDLower                                                         :: typeValue (lexicalTypeIndexer (lexicalDLowerKey                                                         lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalELower                                                         :: typeValue (lexicalTypeIndexer (lexicalELowerKey                                                         lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalFLower                                                         :: typeValue (lexicalTypeIndexer (lexicalFLowerKey                                                         lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalGLower                                                         :: typeValue (lexicalTypeIndexer (lexicalGLowerKey                                                         lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalHLower                                                         :: typeValue (lexicalTypeIndexer (lexicalHLowerKey                                                         lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalILower                                                         :: typeValue (lexicalTypeIndexer (lexicalILowerKey                                                         lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalJLower                                                         :: typeValue (lexicalTypeIndexer (lexicalJLowerKey                                                         lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalKLower                                                         :: typeValue (lexicalTypeIndexer (lexicalKLowerKey                                                         lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalLLower                                                         :: typeValue (lexicalTypeIndexer (lexicalLLowerKey                                                         lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalMLower                                                         :: typeValue (lexicalTypeIndexer (lexicalMLowerKey                                                         lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalNLower                                                         :: typeValue (lexicalTypeIndexer (lexicalNLowerKey                                                         lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalOLower                                                         :: typeValue (lexicalTypeIndexer (lexicalOLowerKey                                                         lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalPLower                                                         :: typeValue (lexicalTypeIndexer (lexicalPLowerKey                                                         lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalQLower                                                         :: typeValue (lexicalTypeIndexer (lexicalQLowerKey                                                         lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalRLower                                                         :: typeValue (lexicalTypeIndexer (lexicalRLowerKey                                                         lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalSLower                                                         :: typeValue (lexicalTypeIndexer (lexicalSLowerKey                                                         lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalTLower                                                         :: typeValue (lexicalTypeIndexer (lexicalTLowerKey                                                         lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalULower                                                         :: typeValue (lexicalTypeIndexer (lexicalULowerKey                                                         lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalVLower                                                         :: typeValue (lexicalTypeIndexer (lexicalVLowerKey                                                         lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalWLower                                                         :: typeValue (lexicalTypeIndexer (lexicalWLowerKey                                                         lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalXLower                                                         :: typeValue (lexicalTypeIndexer (lexicalXLowerKey                                                         lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalYLower                                                         :: typeValue (lexicalTypeIndexer (lexicalYLowerKey                                                         lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalZLower                                                         :: typeValue (lexicalTypeIndexer (lexicalZLowerKey                                                         lexicalTypeIndexerKeyCons zero succ)),

		_lexicalFoundationPrimitives_lexicalLeftBrace                                                      :: typeValue (lexicalTypeIndexer (lexicalLeftBraceKey                                                      lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalPipe                                                           :: typeValue (lexicalTypeIndexer (lexicalPipeKey                                                           lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalRightBrace                                                     :: typeValue (lexicalTypeIndexer (lexicalRightBraceKey                                                     lexicalTypeIndexerKeyCons zero succ)),
		_lexicalFoundationPrimitives_lexicalTilde                                                          :: typeValue (lexicalTypeIndexer (lexicalTildeKey                                                          lexicalTypeIndexerKeyCons zero succ)),

		_lexicalFoundationPrimitives_lexicalDEL                                                            :: typeValue (lexicalTypeIndexer (lexicalDELKey                                                            lexicalTypeIndexerKeyCons zero succ))
	}
