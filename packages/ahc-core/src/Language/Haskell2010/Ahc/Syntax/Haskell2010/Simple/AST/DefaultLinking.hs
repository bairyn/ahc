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
-- building up the module instantiations; these may be used as needed by the
-- definitions here, and the rest of the variables are instantiated:
-- - k z s l           : 4 variables encode the type record 'LexicalFoundation'.
-- - lexicalAnnotation : all non-top-level section 2 structures.
-- - grammarAnnotation : all non-top-level section 3-5 structures.
-- - annotation        : top-level annotation.
--
-- Finally, this module uses 'Language.Haskell2010.Ahc.Data.Fixed.Fixed' to
-- wrap each structure to provide F-algebra style recursion.  (This is not
-- directly provided by the Haskell2010 specification, which would have been a
-- convenient Schelling point.)
--
-- (If needed, ‘type’ aliases can be wrapped with another layer with ‘newtype’
-- to enable partial type application.)
module Language.Haskell2010.Ahc.Syntax.Haskell2010.Simple.AST.DefaultLinking (
	-- Export everything, explicitly.

	-- * Structures with default linking.

	-- ** Lexical foundation structures.

	LexicalFoundation,

	-- ** Lexical structures.

	-- TODO: implement, then uncomment.

{-

	-- *** § 2.2 Lexical Program Structure types.
	ProgramF(MkProgramF, _unProgramF),
	Program,
	LexemeF(MkLexemeF, _unLexemeF),
	Lexeme,
	LiteralF(MkLiteralF, _unLiteralF),
	Literal,
	SpecialF(MkSpecialF, _unSpecialF),
	Special,
	SpecialSansNcF(MkSpecialSansNcF, _unSpecialSansNcF),
	SpecialSansNc,

	WhitespaceF(MkWhitespaceF, _unWhitespaceF),
	Whitespace,
	WhitestuffF(MkWhitestuffF, _unWhitestuffF),
	Whitestuff,
	WhitecharF(MkWhitecharF, _unWhitecharF),
	Whitechar,
	NewlineF(MkNewlineF, _unNewlineF),
	Newline,
	ReturnF(MkReturnF, _unReturnF),
	Return,
	LinefeedF(MkLinefeedF, _unLinefeedF),
	Linefeed,
	VertabF(MkVertabF, _unVertabF),
	Vertab,
	FormfeedF(MkFormfeedF, _unFormfeedF),
	Formfeed,
	SpaceF(MkSpaceF, _unSpaceF),
	Space,
	TabF(MkTabF, _unTabF),
	Tab,
	UniWhiteF(MkUniWhiteF, _unUniWhiteF),
	UniWhite,

	CommentF(MkCommentF, _unCommentF),
	Comment,
	DashesF(MkDashesF, _unDashesF),
	Dashes,
	OpenComF(MkOpenComF, _unOpenComF),
	OpenCom,
	CloseComF(MkCloseComF, _unCloseComF),
	CloseCom,
	NcommentF(MkNcommentF, _unNcommentF),
	Ncomment,
	BigAnySeqF(MkBigAnySeqF, _unBigAnySeqF),
	BigAnySeq,
	BigAnySeqValidNcomChar1_0F(MkBigAnySeqValidNcomChar1_0F, _unBigAnySeqValidNcomChar1_0F),
	BigAnySeqValidNcomChar1_0,
	BigAnySeqValidNcomChar1_1F(MkBigAnySeqValidNcomChar1_1F, _unBigAnySeqValidNcomChar1_1F),
	BigAnySeqValidNcomChar1_1,
	BigAnyF(MkBigAnyF, _unBigAnyF),
	BigAny,
	BigAnySansNcF(MkBigAnySansNcF, _unBigAnySansNcF),
	BigAnySansNc,
	AnyF(MkAnyF, _unAnyF),
	Any,
	GraphicF(MkGraphicF, _unGraphicF),
	Graphic,
	GraphicSansNcF(MkGraphicSansNcF, _unGraphicSansNcF),
	GraphicSansNc,

	SmallF(MkSmallF, _unSmallF),
	Small,
	AscSmallF(MkAscSmallF, _unAscSmallF),
	AscSmall,
	UniSmallF(MkUniSmallF, _unUniSmallF),
	UniSmall,

	LargeF(MkLargeF, _unLargeF),
	Large,
	AscLargeF(MkAscLargeF, _unAscLargeF),
	AscLarge,
	UniLargeF(MkUniLargeF, _unUniLargeF),
	UniLarge,
	SymbolF(MkSymbolF, _unSymbolF),
	Symbol,
	SymbolSansNcF(MkSymbolSansNcF, _unSymbolSansNcF),
	SymbolSansNc,

	AscSymbolF(MkAscSymbolF, _unAscSymbolF),
	AscSymbol,
	AscSymbolSansNcF(MkAscSymbolSansNcF, _unAscSymbolSansNcF),
	AscSymbolSansNc,
	UniSymbolF(MkUniSymbolF, _unUniSymbolF),
	UniSymbol,
	UniSymbolSansSpecialishF(MkUniSymbolSansSpecialishF, _unUniSymbolSansSpecialishF),
	UniSymbolSansSpecialish,
	UniSymbolSansSpecialishSansNcF(MkUniSymbolSansSpecialishSansNcF, _unUniSymbolSansSpecialishSansNcF),
	UniSymbolSansSpecialishSansNc,
	DigitF(MkDigitF, _unDigitF),
	Digit,
	AscDigitF(MkAscDigitF, _unAscDigitF),
	AscDigit,
	UniDigitF(MkUniDigitF, _unUniDigitF),
	UniDigit,
	OctitF(MkOctitF, _unOctitF),
	Octit,
	HexitF(MkHexitF, _unHexitF),
	Hexit,

	-- *** § 2.4 Identifiers and Operators types.
	UnqualifiedNameF(MkUnqualifiedNameF, _unUnqualifiedNameF),
	UnqualifiedName,
	VaridNoExclusionsF(MkVaridNoExclusionsF, _unVaridNoExclusionsF),
	VaridNoExclusions,
	VaridStartF(MkVaridStartF, _unVaridStartF),
	VaridStart,
	IdentifierInnerF(MkIdentifierInnerF, _unIdentifierInnerF),
	IdentifierInner,
	VaridInnerF(MkVaridInnerF, _unVaridInnerF),
	VaridInner,
	ConidF(MkConidF, _unConidF),
	Conid,
	ConidStartF(MkConidStartF, _unConidStartF),
	ConidStart,
	ConidInnerF(MkConidInnerF, _unConidInnerF),
	ConidInner,
	ReservedidF(MkReservedidF, _unReservedidF),
	Reservedid,
	VarSymNoExtraExclusionsF(MkVarSymNoExtraExclusionsF, _unVarSymNoExtraExclusionsF),
	VarSymNoExtraExclusions,
	VarSymStartF(MkVarSymStartF, _unVarSymStartF),
	VarSymStart,
	ConSymNoExtraExclusionsF(MkConSymNoExtraExclusionsF, _unConSymNoExtraExclusionsF),
	ConSymNoExtraExclusions,
	ConSymStartF(MkConSymStartF, _unConSymStartF),
	ConSymStart,
	ReservedOpF(MkReservedOpF, _unReservedOpF),
	ReservedOp,
	TyvarF(MkTyvarF, _unTyvarF),
	Tyvar,
	TyconF(MkTyconF, _unTyconF),
	Tycon,
	TyclsF(MkTyclsF, _unTyclsF),
	Tycls,
	ModidF(MkModidF, _unModidF),
	Modid,
	NameF(MkNameF, _unNameF),
	Name,
	QvaridF(MkQvaridF, _unQvaridF),
	Qvarid,
	QconidF(MkQconidF, _unQconidF),
	Qconid,
	QtyconF(MkQtyconF, _unQtyconF),
	Qtycon,
	QtyclsF(MkQtyclsF, _unQtyclsF),
	Qtycls,
	QvarSymF(MkQvarSymF, _unQvarSymF),
	QvarSym,
	QconSymF(MkQconSymF, _unQconSymF),
	QconSym,

	-- *** § 2.5 Numeric Literals types.
	DecimalF(MkDecimalF, _unDecimalF),
	Decimal,
	OctalF(MkOctalF, _unOctalF),
	Octal,
	HexadecimalF(MkHexadecimalF, _unHexadecimalF),
	Hexadecimal,
	IntegerF(MkIntegerF, _unIntegerF),
	Integer,
	FloatF(MkFloatF, _unFloatF),
	Float,
	ExponentF(MkExponentF, _unExponentF),
	Exponent,

	-- *** § 2.6 Character and String Literals types.

	CharF(MkCharF, _unCharF),
	Char,
	CharLiteralInnerF(MkCharLiteralInnerF, _unCharLiteralInnerF),
	CharLiteralInner,
	StringF(MkStringF, _unStringF),
	String,
	StringLiteralInnerUnitF(MkStringLiteralInnerUnitF, _unStringLiteralInnerUnitF),
	StringLiteralInnerUnit,
	EscapeF(MkEscapeF, _unEscapeF),
	Escape,
	EscapeInnerF(MkEscapeInnerF, _unEscapeInnerF),
	EscapeInner,
	CharEscF(MkCharEscF, _unCharEscF),
	CharEsc,
	AsciiF(MkAsciiF, _unAsciiF),
	Ascii,
	CntrlF(MkCntrlF, _unCntrlF),
	Cntrl,
	GapF(MkGapF, _unGapF),
	Gap,

	-- *** Base lexical structures.

	-- **** Pseudo-foundational lexical structures.

	LexicalPseudoF(MkLexicalPseudoF, _unLexicalPseudoF),
	LexicalPseudo,

	-- ***** Non-symbolic keyword pseudo-lexical structures.

	LexicalNonsymKeywordF(MkLexicalNonsymKeywordF, _unLexicalNonsymKeywordF),
	LexicalNonsymKeyword,
	LexicalCaseF(MkLexicalCaseF, _unLexicalCaseF),
	LexicalCase,
	LexicalClassF(MkLexicalClassF, _unLexicalClassF),
	LexicalClass,
	LexicalDataF(MkLexicalDataF, _unLexicalDataF),
	LexicalData,
	LexicalDefaultF(MkLexicalDefaultF, _unLexicalDefaultF),
	LexicalDefault,
	LexicalDerivingF(MkLexicalDerivingF, _unLexicalDerivingF),
	LexicalDeriving,
	LexicalDoF(MkLexicalDoF, _unLexicalDoF),
	LexicalDo,
	LexicalElseF(MkLexicalElseF, _unLexicalElseF),
	LexicalElse,
	LexicalForeignF(MkLexicalForeignF, _unLexicalForeignF),
	LexicalForeign,
	LexicalIfF(MkLexicalIfF, _unLexicalIfF),
	LexicalIf,
	LexicalImportF(MkLexicalImportF, _unLexicalImportF),
	LexicalImport,
	LexicalInF(MkLexicalInF, _unLexicalInF),
	LexicalIn,
	LexicalInfixF(MkLexicalInfixF, _unLexicalInfixF),
	LexicalInfix,
	LexicalInfixlF(MkLexicalInfixlF, _unLexicalInfixlF),
	LexicalInfixl,
	LexicalInfixrF(MkLexicalInfixrF, _unLexicalInfixrF),
	LexicalInfixr,
	LexicalInstanceF(MkLexicalInstanceF, _unLexicalInstanceF),
	LexicalInstance,
	LexicalLetF(MkLexicalLetF, _unLexicalLetF),
	LexicalLet,
	LexicalModuleF(MkLexicalModuleF, _unLexicalModuleF),
	LexicalModule,
	LexicalNewtypeF(MkLexicalNewtypeF, _unLexicalNewtypeF),
	LexicalNewtype,
	LexicalOfF(MkLexicalOfF, _unLexicalOfF),
	LexicalOf,
	LexicalThenF(MkLexicalThenF, _unLexicalThenF),
	LexicalThen,
	LexicalTypeF(MkLexicalTypeF, _unLexicalTypeF),
	LexicalType,
	LexicalWhereF(MkLexicalWhereF, _unLexicalWhereF),
	LexicalWhere,

	-- ***** Non-symbolic non-keyword pseudo-lexical structures.

	LexicalNonsymNonkeywordF(MkLexicalNonsymNonkeywordF, _unLexicalNonsymNonkeywordF),
	LexicalNonsymNonkeyword,
	LexicalAsF(MkLexicalAsF, _unLexicalAsF),
	LexicalAs,
	LexicalHidingF(MkLexicalHidingF, _unLexicalHidingF),
	LexicalHiding,
	LexicalQualifiedF(MkLexicalQualifiedF, _unLexicalQualifiedF),
	LexicalQualified,

-}

	-- ***** Symbolic alias pseudo-lexical structures.

	LexicalSymAliasF(MkLexicalSymAliasF, _unLexicalSymAliasF),
	LexicalSymAlias,
	LexicalDotDotF(MkLexicalDotDotF, _unLexicalDotDotF),
	LexicalDotDot,
	LexicalDoubleColonF(MkLexicalDoubleColonF, _unLexicalDoubleColonF),
	LexicalDoubleColon,
	LexicalDoubleRightArrowF(MkLexicalDoubleRightArrowF, _unLexicalDoubleRightArrowF),
	LexicalDoubleRightArrow,
	LexicalLeftArrowF(MkLexicalLeftArrowF, _unLexicalLeftArrowF),
	LexicalLeftArrow,
	LexicalRightArrowF(MkLexicalRightArrowF, _unLexicalRightArrowF),
	LexicalRightArrow,

	-- ***** Alias pseudo-lexical structures.

	LexicalAliasF(MkLexicalAliasF, _unLexicalAliasF),
	LexicalAlias,
	LexicalSpaceF(MkLexicalSpaceF, _unLexicalSpaceF),
	LexicalSpace,
	LexicalMinusF(MkLexicalMinusF, _unLexicalMinusF),
	LexicalMinus,
	LexicalAsciiLambdaF(MkLexicalAsciiLambdaF, _unLexicalAsciiLambdaF),
	LexicalAsciiLambda,

	-- ***** Non-symbolic numeric literal prefix pseudo-lexical structures.

	LexicalNumPrefixF(MkLexicalNumPrefixF, _unLexicalNumPrefixF),
	LexicalNumPrefix,
	Lexical0oF(MkLexical0oF, _unLexical0oF),
	Lexical0o,
	Lexical0OF(MkLexical0OF, _unLexical0OF),
	Lexical0O,
	Lexical0xF(MkLexical0xF, _unLexical0xF),
	Lexical0x,
	Lexical0XF(MkLexical0XF, _unLexical0XF),
	Lexical0X,

	-- ** Grammatical structures.

	-- TODO: implement, then uncomment.

{-
	-- *** § 5.1 Module Structure types.
	ModuleF(MkModuleF, _unModuleF),
	Module,
	BodyF(MkBodyF, _unBodyF),
	Body,
	ImpDeclsF(MkImpDeclsF, _unImpDeclsF),
	ImpDecls,
	TopDeclsF(MkTopDeclsF, _unTopDeclsF),
	TopDecls,

	-- *** § 5.2 Export Lists types.
	ExportsF(MkExportsF, _unExportsF),
	Exports,
	ExportF(MkExportF, _unExportF),
	Export,
	CnameF(MkCnameF, _unCnameF),
	Cname,

	-- *** § 5.3 Import Declarations types.
	ImpDeclF(MkImpDeclF, _unImpDeclF),
	ImpDecl,
	ImpSpecF(MkImpSpecF, _unImpSpecF),
	ImpSpec,
	ImportF(MkImportF, _unImportF),
	Import,

	-- *** § 4 Declarations and Bindings types.
	TopDeclF(MkTopDeclF, _unTopDeclF),
	TopDecl,
	DeclsF(MkDeclsF, _unDeclsF),
	Decls,
	DeclF(MkDeclF, _unDeclF),
	Decl,
	CdeclsF(MkCdeclsF, _unCdeclsF),
	Cdecls,
	CdeclF(MkCdeclF, _unCdeclF),
	Cdecl,
	IdeclsF(MkIdeclsF, _unIdeclsF),
	Idecls,
	IdeclF(MkIdeclF, _unIdeclF),
	Idecl,
	GenDeclF(MkGenDeclF, _unGenDeclF),
	GenDecl,
	OpsF(MkOpsF, _unOpsF),
	Ops,
	VarsF(MkVarsF, _unVarsF),
	Vars,
	FixityF(MkFixityF, _unFixityF),
	Fixity,

	-- *** § 4.1.2 Syntax of Types types.
	TypeF(MkTypeF, _unTypeF),
	Type,
	BTypeF(MkBTypeF, _unBTypeF),
	BType,
	ATypeF(MkATypeF, _unATypeF),
	AType,
	GtyconF(MkGtyconF, _unGtyconF),
	Gtycon,

	-- *** § 4.1.3 Syntax of Class Assertions and Contexts types.
	ContextF(MkContextF, _unContextF),
	Context,
	ClassF(MkClassF, _unClassF),
	Class,
	{-
	ClassQtyclsF(MkClassQtyclsF, _unClassQtyclsF),
	ClassQtycls,
	ClassTyclsF(MkClassTyclsF, _unClassTyclsF),
	ClassTycls,
	ClassTyvarF(MkClassTyvarF, _unClassTyvarF),
	ClassTyvar,
	-}

	-- *** § 4.2.1 Algebraic Datatype Declarations types.
	SimpleTypeF(MkSimpleTypeF, _unSimpleTypeF),
	SimpleType,
	ConstrsF(MkConstrsF, _unConstrsF),
	Constrs,
	ConstrF(MkConstrF, _unConstrF),
	Constr,
	EvalAtypeF(MkEvalAtypeF, _unEvalAtypeF),
	EvalAtype,
	FieldDeclF(MkFieldDeclF, _unFieldDeclF),
	FieldDecl,
	DerivingF(MkDerivingF, _unDerivingF),
	Deriving,
	DclassF(MkDclassF, _unDclassF),
	Dclass,

	-- *** § 4.2.3 Datatype Renamings types.
	NewConstrF(MkNewConstrF, _unNewConstrF),
	NewConstr,

	-- *** § 4.3.1 Type Classes and Overloading types.
	SContextF(MkSContextF, _unSContextF),
	SContext,
	SimpleClassF(MkSimpleClassF, _unSimpleClassF),
	SimpleClass,

	-- *** § 4.3.2 Instance Declarations types.
	InstF(MkInstF, _unInstF),
	Inst,

	{-
	-- *** § 4.4.2 Fixity Declarations types.
	FixityOpF(MkFixityOpF, _unFixityOpF),
	FixityOp,
	-}

	-- *** § 4.4.3 Function and Pattern Bindings types.
	FunLhsF(MkFunLhsF, _unFunLhsF),
	FunLhs,

	-- *** § 3 Expressions types.
	ExpF(MkExpF, _unExpF),
	Exp,
	InfixExpF(MkInfixExpF, _unInfixExpF),
	InfixExp,
	LexpF(MkLexpF, _unLexpF),
	Lexp,
	FexpF(MkFexpF, _unFexpF),
	Fexp,
	AexpF(MkAexpF, _unAexpF),
	Aexp,

	-- *** § 3.2 Variables, Constructors, Operators, and Literals types.
	GconF(MkGconF, _unGconF),
	Gcon,
	VarF(MkVarF, _unVarF),
	Var,
	QvarF(MkQvarF, _unQvarF),
	Qvar,
	ConF(MkConF, _unConF),
	Con,
	QconF(MkQconF, _unQconF),
	Qcon,
	VarOpF(MkVarOpF, _unVarOpF),
	VarOp,
	QvarOpF(MkQvarOpF, _unQvarOpF),
	QvarOp,
	ConOpF(MkConOpF, _unConOpF),
	ConOp,
	QconOpF(MkQconOpF, _unQconOpF),
	QconOp,
	OpF(MkOpF, _unOpF),
	Op,
	QopF(MkQopF, _unQopF),
	Qop,
	GconSymF(MkGconSymF, _unGconSymF),
	GconSym,

	-- *** § 3.13 Case Expressions types.
	AltsF(MkAltsF, _unAltsF),
	Alts,
	AltF(MkAltF, _unAltF),
	Alt,
	GdpatF(MkGdpatF, _unGdpatF),
	Gdpat,
	GuardsF(MkGuardsF, _unGuardsF),
	Guards,
	GuardF(MkGuardF, _unGuardF),
	Guard,

	-- *** § 3.14 Do Expressions types.
	StmtsF(MkStmtsF, _unStmtsF),
	Stmts,
	StmtF(MkStmtF, _unStmtF),
	Stmt,

	-- *** § 3.15.2 Construction Using Field Labels types.
	FbindF(MkFbindF, _unFbindF),
	Fbind,

	-- *** § 3.17.1 Patterns types.
	PatF(MkPatF, _unPatF),
	Pat,
	LpatF(MkLpatF, _unLpatF),
	Lpat,
	ApatF(MkApatF, _unApatF),
	Apat,
	FpatsF(MkFpatsF, _unFpatsF),
	Fpats,
	FpatsRestF(MkFpatsRestF, _unFpatsRestF),
	FpatsRest,
	Pats2F(MkPats2F, _unPats2F),
	Pats2,
	Pats2RestF(MkPats2RestF, _unPats2RestF),
	Pats2Rest,
	Pats1F(MkPats1F, _unPats1F),
	Pats1,
	Pats1RestF(MkPats1RestF, _unPats1RestF),
	Pats1Rest,
	FpatF(MkFpatF, _unFpatF),
	Fpat,
-}

	-- ** Exclusion structures.

	-- *** § 2.4 Identifiers and Operators types.

	-- **** Exclusion structures types.

	-- TODO: implement, then uncomment.

{-
	VaridInnerSansAscSmallUnderscoreF(MkVaridInnerSansAscSmallUnderscoreF, _unVaridInnerSansAscSmallUnderscoreF),
	VaridInnerSansAscSmallUnderscore,
	SmallSansAscSmallUnderscoreF(MkSmallSansAscSmallUnderscoreF, _unSmallSansAscSmallUnderscoreF),
	SmallSansAscSmallUnderscore,
	UniSmallSansAscF(MkUniSmallSansAscF, _unUniSmallSansAscF),
	UniSmallSansAsc,
	VaridF(MkVaridF, _unVaridF),
	Varid,
	VaridCF(MkVaridCF, _unVaridCF),
	VaridC,
	VaridDF(MkVaridDF, _unVaridDF),
	VaridD,
	VaridEF(MkVaridEF, _unVaridEF),
	VaridE,
	VaridFF(MkVaridFF, _unVaridFF),
	VaridF,
	VaridIF(MkVaridIF, _unVaridIF),
	VaridI,
	VaridLF(MkVaridLF, _unVaridLF),
	VaridL,
	VaridMF(MkVaridMF, _unVaridMF),
	VaridM,
	VaridNF(MkVaridNF, _unVaridNF),
	VaridN,
	VaridOF(MkVaridOF, _unVaridOF),
	VaridO,
	VaridTF(MkVaridTF, _unVaridTF),
	VaridT,
	VaridWF(MkVaridWF, _unVaridWF),
	VaridW,
	VaridCaF(MkVaridCaF, _unVaridCaF),
	VaridCa,
	VaridClF(MkVaridClF, _unVaridClF),
	VaridCl,
	VaridDaF(MkVaridDaF, _unVaridDaF),
	VaridDa,
	VaridDeF(MkVaridDeF, _unVaridDeF),
	VaridDe,
	VaridElF(MkVaridElF, _unVaridElF),
	VaridEl,
	VaridFoF(MkVaridFoF, _unVaridFoF),
	VaridFo,
	VaridImF(MkVaridImF, _unVaridImF),
	VaridIm,
	VaridInF(MkVaridInF, _unVaridInF),
	VaridIn,
	VaridLeF(MkVaridLeF, _unVaridLeF),
	VaridLe,
	VaridMoF(MkVaridMoF, _unVaridMoF),
	VaridMo,
	VaridNeF(MkVaridNeF, _unVaridNeF),
	VaridNe,
	VaridThF(MkVaridThF, _unVaridThF),
	VaridTh,
	VaridTyF(MkVaridTyF, _unVaridTyF),
	VaridTy,
	VaridCasF(MkVaridCasF, _unVaridCasF),
	VaridCas,
	VaridClaF(MkVaridClaF, _unVaridClaF),
	VaridCla,
	VaridDatF(MkVaridDatF, _unVaridDatF),
	VaridDat,
	VaridDefF(MkVaridDefF, _unVaridDefF),
	VaridDef,
	VaridDerF(MkVaridDerF, _unVaridDerF),
	VaridDer,
	VaridElsF(MkVaridElsF, _unVaridElsF),
	VaridEls,
	VaridForF(MkVaridForF, _unVaridForF),
	VaridFor,
	VaridImpF(MkVaridImpF, _unVaridImpF),
	VaridImp,
	VaridInfF(MkVaridInfF, _unVaridInfF),
	VaridInf,
	VaridInsF(MkVaridInsF, _unVaridInsF),
	VaridIns,
	VaridModF(MkVaridModF, _unVaridModF),
	VaridMod,
	VaridNewF(MkVaridNewF, _unVaridNewF),
	VaridNew,
	VaridTheF(MkVaridTheF, _unVaridTheF),
	VaridThe,
	VaridTypF(MkVaridTypF, _unVaridTypF),
	VaridTyp,
	VaridWheF(MkVaridWheF, _unVaridWheF),
	VaridWhe,
	VaridClasF(MkVaridClasF, _unVaridClasF),
	VaridClas,
	VaridDefaF(MkVaridDefaF, _unVaridDefaF),
	VaridDefa,
	VaridDeriF(MkVaridDeriF, _unVaridDeriF),
	VaridDeri,
	VaridForeF(MkVaridForeF, _unVaridForeF),
	VaridFore,
	VaridImpoF(MkVaridImpoF, _unVaridImpoF),
	VaridImpo,
	VaridInfiF(MkVaridInfiF, _unVaridInfiF),
	VaridInfi,
	VaridInstF(MkVaridInstF, _unVaridInstF),
	VaridInst,
	VaridModuF(MkVaridModuF, _unVaridModuF),
	VaridModu,
	VaridNewtF(MkVaridNewtF, _unVaridNewtF),
	VaridNewt,
	VaridWherF(MkVaridWherF, _unVaridWherF),
	VaridWher,
	VaridDefauF(MkVaridDefauF, _unVaridDefauF),
	VaridDefau,
	VaridDerivF(MkVaridDerivF, _unVaridDerivF),
	VaridDeriv,
	VaridForeiF(MkVaridForeiF, _unVaridForeiF),
	VaridForei,
	VaridImporF(MkVaridImporF, _unVaridImporF),
	VaridImpor,
	VaridInfixF(MkVaridInfixF, _unVaridInfixF),
	VaridInfix,
	VaridInstaF(MkVaridInstaF, _unVaridInstaF),
	VaridInsta,
	VaridModulF(MkVaridModulF, _unVaridModulF),
	VaridModul,
	VaridNewtyF(MkVaridNewtyF, _unVaridNewtyF),
	VaridNewty,
	VaridDefaulF(MkVaridDefaulF, _unVaridDefaulF),
	VaridDefaul,
	VaridDeriviF(MkVaridDeriviF, _unVaridDeriviF),
	VaridDerivi,
	VaridForeigF(MkVaridForeigF, _unVaridForeigF),
	VaridForeig,
	VaridInstanF(MkVaridInstanF, _unVaridInstanF),
	VaridInstan,
	VaridNewtypF(MkVaridNewtypF, _unVaridNewtypF),
	VaridNewtyp,
	VaridDerivinF(MkVaridDerivinF, _unVaridDerivinF),
	VaridDerivin,
	VaridInstancF(MkVaridInstancF, _unVaridInstancF),
	VaridInstanc,

	SymbolSansAscF(MkSymbolSansAscF, _unSymbolSansAscF),
	SymbolSansAsc,
	UniSymbolSansSpecialishAscF(MkUniSymbolSansSpecialishAscF, _unUniSymbolSansSpecialishAscF),
	UniSymbolSansSpecialishAsc,
	SymbolSansHyphenF(MkSymbolSansHyphenF, _unSymbolSansHyphenF),
	SymbolSansHyphen,
	AscSymbolSansHyphenF(MkAscSymbolSansHyphenF, _unAscSymbolSansHyphenF),
	AscSymbolSansHyphen,
	UniSymbolSansSpecialishHyphenF(MkUniSymbolSansSpecialishHyphenF, _unUniSymbolSansSpecialishHyphenF),
	UniSymbolSansSpecialishHyphen,
	VarSymF(MkVarSymF, _unVarSymF),
	VarSym,

	SymbolSansColonF(MkSymbolSansColonF, _unSymbolSansColonF),
	SymbolSansColon,
	AscSymbolSansColonF(MkAscSymbolSansColonF, _unAscSymbolSansColonF),
	AscSymbolSansColon,
	UniSymbolSansSpecialishColonF(MkUniSymbolSansSpecialishColonF, _unUniSymbolSansSpecialishColonF),
	UniSymbolSansSpecialishColon,
	ConSymF(MkConSymF, _unConSymF),
	ConSym,
-}
) where

-- (Note: the import ordering style I use here is that since Prelude handling
-- has more built-in relevance (implicit preludes), implicit prelude related
-- imports go first, and then intra-package imports, then Haskell2010 or base
-- imports, and then imports external to the package.)

import           Prelude ()

import           Language.Haskell2010.Ahc.Syntax.Haskell2010.Simple.AST.Base
import qualified Language.Haskell2010.Ahc.Data.Proxy as Proxy
import qualified Language.Haskell2010.Ahc.Data.Fixed as Fixed

import qualified Prelude

{-
 - ----------------------------------------------------------------
 - Structures with default linking.
 - ----------------------------------------------------------------
 -}

-- Lexical Foundation structures.

-- | ‘LexicalFoundationBase’ with default linking.
--
-- - k: ‘lexicalTypeIndexerKeyCons’
-- - z: ‘zero’
-- - s: ‘succ’
-- - l: ‘lexicalTypeIndexer’
type LexicalFoundation k z s l typeValue = LexicalFoundationBase Proxy.Proxy LexicalEndOfParseKeyBase LexicalUnicodeDigitKeyBase LexicalUnicodeLargeKeyBase LexicalUnicodeSmallKeyBase LexicalUnicodeSymbolKeyBase LexicalUnicodeWhitespaceCharKeyBase LexicalUnicodeSmallSansAscUnderscoreKeyBase LexicalUnicodeSymbolSansSpecialUnderscoreDoubleQuoteSingleQuoteKeyBase LexicalUnicodeSymbolSansSpecialUnderscoreDoubleQuoteSingleQuoteAsciiKeyBase LexicalUnicodeSymbolSansSpecialUnderscoreDoubleQuoteSingleQuoteColonKeyBase LexicalUnicodeSymbolSansSpecialUnderscoreDoubleQuoteSingleQuoteHyphenKeyBase LexicalNULKeyBase LexicalSOHKeyBase LexicalSTXKeyBase LexicalETXKeyBase LexicalEOTKeyBase LexicalENQKeyBase LexicalACKKeyBase LexicalBELKeyBase LexicalBSKeyBase LexicalHTKeyBase LexicalLFKeyBase LexicalVTKeyBase LexicalFFKeyBase LexicalCRKeyBase LexicalSOKeyBase LexicalSIKeyBase LexicalDLEKeyBase LexicalDC1KeyBase LexicalDC2KeyBase LexicalDC3KeyBase LexicalDC4KeyBase LexicalNAKKeyBase LexicalSYNKeyBase LexicalETBKeyBase LexicalCANKeyBase LexicalEMKeyBase LexicalSUBKeyBase LexicalESCKeyBase LexicalFSKeyBase LexicalGSKeyBase LexicalRSKeyBase LexicalUSKeyBase LexicalSPKeyBase LexicalExclamationKeyBase LexicalDoubleQuoteKeyBase LexicalHashKeyBase LexicalDollarKeyBase LexicalPercentKeyBase LexicalAmpersandKeyBase LexicalSingleQuoteKeyBase LexicalLeftParenthesisKeyBase LexicalRightParenthesisKeyBase LexicalAsteriskKeyBase LexicalPlusKeyBase LexicalCommaKeyBase LexicalHyphenKeyBase LexicalDotKeyBase LexicalSlashKeyBase Lexical0KeyBase Lexical1KeyBase Lexical2KeyBase Lexical3KeyBase Lexical4KeyBase Lexical5KeyBase Lexical6KeyBase Lexical7KeyBase Lexical8KeyBase Lexical9KeyBase LexicalColonKeyBase LexicalSemicolonKeyBase LexicalLeftAngleBracketKeyBase LexicalEqualsKeyBase LexicalRightAngleBracketKeyBase LexicalQuestionMarkKeyBase LexicalAtKeyBase LexicalAKeyBase LexicalBKeyBase LexicalCKeyBase LexicalDKeyBase LexicalEKeyBase LexicalFKeyBase LexicalGKeyBase LexicalHKeyBase LexicalIKeyBase LexicalJKeyBase LexicalKKeyBase LexicalLKeyBase LexicalMKeyBase LexicalNKeyBase LexicalOKeyBase LexicalPKeyBase LexicalQKeyBase LexicalRKeyBase LexicalSKeyBase LexicalTKeyBase LexicalUKeyBase LexicalVKeyBase LexicalWKeyBase LexicalXKeyBase LexicalYKeyBase LexicalZKeyBase LexicalLeftBracketKeyBase LexicalBackslashKeyBase LexicalRightBracketKeyBase LexicalCaretKeyBase LexicalUnderscoreKeyBase LexicalBacktickKeyBase LexicalALowerKeyBase LexicalBLowerKeyBase LexicalCLowerKeyBase LexicalDLowerKeyBase LexicalELowerKeyBase LexicalFLowerKeyBase LexicalGLowerKeyBase LexicalHLowerKeyBase LexicalILowerKeyBase LexicalJLowerKeyBase LexicalKLowerKeyBase LexicalLLowerKeyBase LexicalMLowerKeyBase LexicalNLowerKeyBase LexicalOLowerKeyBase LexicalPLowerKeyBase LexicalQLowerKeyBase LexicalRLowerKeyBase LexicalSLowerKeyBase LexicalTLowerKeyBase LexicalULowerKeyBase LexicalVLowerKeyBase LexicalWLowerKeyBase LexicalXLowerKeyBase LexicalYLowerKeyBase LexicalZLowerKeyBase LexicalLeftBraceKeyBase LexicalPipeKeyBase LexicalRightBraceKeyBase LexicalTildeKeyBase LexicalDELKeyBase k z s l typeValue

-- Lexical structures.

-- § 2.2 Lexical Program Structure types.

-- TODO

-- § 2.4 Identifiers and Operators types.

-- TODO

-- § 2.5 Numeric Literals types.

-- TODO

-- § 2.6 Character and String Literals types.

-- TODO

-- Base lexical structures.

-- Pseudo-foundational lexical structures.

-- TODO

-- Non-symbolic keyword pseudo-lexical structures.

-- TODO

-- Non-symbolic non-keyword pseudo-lexical structures.

-- TODO

-- Symbolic alias pseudo-lexical structures.

-- | 'LexicalSymAliasBase' with fewer unresolved variables, with default linking.
newtype LexicalSymAliasF k z s l lexicalAnnotation annotation fixpoint = MkLexicalSymAliasF { _unLexicalSymAliasF :: (LexicalSymAliasBase (LexicalDotDot k z s l lexicalAnnotation lexicalAnnotation) (LexicalDoubleColon k z s l lexicalAnnotation lexicalAnnotation) (LexicalDoubleRightArrow k z s l lexicalAnnotation lexicalAnnotation) (LexicalLeftArrow k z s l lexicalAnnotation lexicalAnnotation) (LexicalRightArrow k z s l lexicalAnnotation lexicalAnnotation) annotation fixpoint) }
-- | Fixpoint applied to 'LexicalSymAliasF'
type LexicalSymAlias k z s l lexicalAnnotation annotation = Fixed.Fix (LexicalSymAliasF k z s l lexicalAnnotation annotation)

-- | 'LexicalDotDotArrowBase' with fewer unresolved variables, with default linking.
newtype LexicalDotDotF k z s l lexicalAnnotation annotation fixpoint = MkLexicalDotDotF { _unLexicalDotDotF :: (LexicalDotDotBase (l (LexicalDotKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'LexicalDotDotArrowF'
type LexicalDotDot k z s l lexicalAnnotation annotation = Fixed.Fix (LexicalDotDotF k z s l lexicalAnnotation annotation)

-- | 'LexicalDoubleColonArrowBase' with fewer unresolved variables, with default linking.
newtype LexicalDoubleColonF k z s l lexicalAnnotation annotation fixpoint = MkLexicalDoubleColonF { _unLexicalDoubleColonF :: (LexicalDoubleColonBase (l (LexicalColonKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'LexicalDoubleColonArrowF'
type LexicalDoubleColon k z s l lexicalAnnotation annotation = Fixed.Fix (LexicalDoubleColonF k z s l lexicalAnnotation annotation)

-- | 'LexicalDoubleRightArrowBase' with fewer unresolved variables, with default linking.
newtype LexicalDoubleRightArrowF k z s l lexicalAnnotation annotation fixpoint = MkLexicalDoubleRightArrowF { _unLexicalDoubleRightArrowF :: (LexicalDoubleRightArrowBase (l (LexicalEqualsKeyBase k z s)) (l (LexicalRightAngleBracketKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'LexicalDoubleRightArrowF'
type LexicalDoubleRightArrow k z s l lexicalAnnotation annotation = Fixed.Fix (LexicalDoubleRightArrowF k z s l lexicalAnnotation annotation)

-- | 'LexicalLeftArrowBase' with fewer unresolved variables, with default linking.
newtype LexicalLeftArrowF k z s l lexicalAnnotation annotation fixpoint = MkLexicalLeftArrowF { _unLexicalLeftArrowF :: (LexicalLeftArrowBase (l (LexicalLeftAngleBracketKeyBase k z s)) (l (LexicalHyphenKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'LexicalLeftArrowF'
type LexicalLeftArrow k z s l lexicalAnnotation annotation = Fixed.Fix (LexicalLeftArrowF k z s l lexicalAnnotation annotation)

-- | 'LexicalRightArrowBase' with fewer unresolved variables, with default linking.
newtype LexicalRightArrowF k z s l lexicalAnnotation annotation fixpoint = MkLexicalRightArrowF { _unLexicalRightArrowF :: (LexicalRightArrowBase (l (LexicalHyphenKeyBase k z s)) (l (LexicalRightAngleBracketKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'LexicalRightArrowF'
type LexicalRightArrow k z s l lexicalAnnotation annotation = Fixed.Fix (LexicalRightArrowF k z s l lexicalAnnotation annotation)

-- Alias pseudo-lexical structures.

-- | 'LexicalAliasBase' with fewer unresolved variables, with default linking.
newtype LexicalAliasF k z s l lexicalAnnotation annotation fixpoint = MkLexicalAliasF { _unLexicalAliasF :: (LexicalAliasBase (LexicalSpace k z s l lexicalAnnotation) (LexicalMinus k z s l lexicalAnnotation) (LexicalAsciiLambda k z s l lexicalAnnotation) annotation fixpoint) }
-- | Fixpoint applied to 'LexicalAliasF'
type LexicalAlias k z s l lexicalAnnotation annotation = Fixed.Fix (LexicalAliasF k z s l lexicalAnnotation annotation)

-- | 'LexicalSpaceBase' with fewer unresolved variables, with default linking.
newtype LexicalSpaceF k z s l annotation fixpoint = MkLexicalSpaceF { _unLexicalSpaceF :: (LexicalSpaceBase (l (LexicalSPKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'LexicalSpaceF'
type LexicalSpace k z s l annotation = Fixed.Fix (LexicalSpaceF k z s l annotation)

-- | 'LexicalMinusBase' with fewer unresolved variables, with default linking.
newtype LexicalMinusF k z s l annotation fixpoint = MkLexicalMinusF { _unLexicalMinusF :: (LexicalMinusBase (l (LexicalHyphenKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'LexicalMinusF'
type LexicalMinus k z s l annotation = Fixed.Fix (LexicalMinusF k z s l annotation)

-- | 'LexicalAsciiLambdaBase' with fewer unresolved variables, with default linking.
newtype LexicalAsciiLambdaF k z s l annotation fixpoint = MkLexicalAsciiLambdaF { _unLexicalAsciiLambdaF :: (LexicalAsciiLambdaBase (l (LexicalBackslashKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'LexicalAsciiLambdaF'
type LexicalAsciiLambda k z s l annotation = Fixed.Fix (LexicalAsciiLambdaF k z s l annotation)

-- Non-symbolic numeric literal prefix pseudo-lexical structures.

-- | 'LexicalNumPrefixBase' with fewer unresolved variables, with default linking.
newtype LexicalNumPrefixF k z s l lexicalAnnotation annotation fixpoint = MkLexicalNumPrefixF { _unLexicalNumPrefixF :: (LexicalNumPrefixBase (Lexical0o k z s l lexicalAnnotation) (Lexical0O k z s l lexicalAnnotation) (Lexical0x k z s l lexicalAnnotation) (Lexical0X k z s l lexicalAnnotation) annotation fixpoint) }
-- | Fixpoint applied to 'LexicalNumPrefixF'
type LexicalNumPrefix k z s l lexicalAnnotation annotation = Fixed.Fix (LexicalNumPrefixF k z s l lexicalAnnotation annotation)

-- | 'Lexical0oBase' with fewer unresolved variables, with default linking.
newtype Lexical0oF k z s l annotation fixpoint = MkLexical0oF { _unLexical0oF :: (Lexical0oBase (l (Lexical0KeyBase k z s)) (l (LexicalXKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'Lexical0oF'
type Lexical0o k z s l annotation = Fixed.Fix (Lexical0oF k z s l annotation)

-- | 'Lexical0OBase' with fewer unresolved variables, with default linking.
newtype Lexical0OF k z s l annotation fixpoint = MkLexical0OF { _unLexical0OF :: (Lexical0OBase (l (Lexical0KeyBase k z s)) (l (LexicalXKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'Lexical0OF'
type Lexical0O k z s l annotation = Fixed.Fix (Lexical0OF k z s l annotation)

-- | 'Lexical0xBase' with fewer unresolved variables, with default linking.
newtype Lexical0xF k z s l annotation fixpoint = MkLexical0xF { _unLexical0xF :: (Lexical0xBase (l (Lexical0KeyBase k z s)) (l (LexicalXKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'Lexical0xF'
type Lexical0x k z s l annotation = Fixed.Fix (Lexical0xF k z s l annotation)

-- | 'Lexical0XBase' with fewer unresolved variables, with default linking.
newtype Lexical0XF k z s l annotation fixpoint = MkLexical0XF { _unLexical0XF :: (Lexical0XBase (l (Lexical0KeyBase k z s)) (l (LexicalXKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'Lexical0XF'
type Lexical0X k z s l annotation = Fixed.Fix (Lexical0XF k z s l annotation)

-- Grammatical structures.

-- § 5.1 Module Structure types.

-- TODO

-- § 5.2 Export Lists types.

-- TODO

-- § 5.3 Import Declarations types.

-- TODO

-- § 4 Declarations and Bindings types.

-- TODO

-- § 4.1.2 Syntax of Types types.

-- TODO

-- § 4.1.3 Syntax of Class Assertions and Contexts types.

-- TODO

-- § 4.2.1 Algebraic Datatype Declarations types.

-- TODO

-- § 4.2.3 Datatype Renamings types.

-- TODO

-- § 4.3.1 Type Classes and Overloading types.

-- TODO

-- § 4.3.2 Instance Declarations types.

-- TODO

-- § 4.4.2 Fixity Declarations types.

-- TODO

-- § 4.4.3 Function and Pattern Bindings types.

-- TODO

-- § 3 Expressions types.

-- TODO

-- § 3.2 Variables, Constructors, Operators, and Literals types.

-- TODO

-- § 3.13 Case Expressions types.

-- TODO

-- § 3.14 Do Expressions types.

-- TODO

-- § 3.15.2 Construction Using Field Labels types.

-- TODO

-- § 3.17.1 Patterns types.

-- TODO

-- Exclusion structures.

-- § 2.4 Identifiers and Operators types.

-- Exclusion structures types.

-- TODO
