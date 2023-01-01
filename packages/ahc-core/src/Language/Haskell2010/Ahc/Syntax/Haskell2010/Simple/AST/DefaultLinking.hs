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
	VaridF_,
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

-- | 'ProgramBase' with fewer unresolved variables, with default linking.
newtype ProgramF k z s l lexicalAnnotation annotation fixpoint = MkProgramF { _unProgramF :: (ProgramBase [] Prelude.Either (Lexeme k z s l lexicalAnnotation lexicalAnnotation fixpoint) (Whitespace k z s l lexicalAnnotation lexicalAnnotation fixpoint) annotation fixpoint) }
-- | Fixpoint applied to 'ProgramF'
type Program k z s l lexicalAnnotation annotation = Fixed.Fix (ProgramF k z s l lexicalAnnotation annotation)
-- | 'LexemeBase' with fewer unresolved variables, with default linking.
newtype LexemeF k z s l lexicalAnnotation annotation fixpoint = MkLexemeF { _unLexemeF :: (LexemeBase (Qvarid k z s l lexicalAnnotation lexicalAnnotation fixpoint) (Qconid k z s l lexicalAnnotation lexicalAnnotation fixpoint) (QvarSym k z s l lexicalAnnotation lexicalAnnotation fixpoint) (QconSym k z s l lexicalAnnotation lexicalAnnotation fixpoint) (Literal k z s l lexicalAnnotation lexicalAnnotation fixpoint) (Special k z s l lexicalAnnotation lexicalAnnotation fixpoint) (ReservedOp k z s l lexicalAnnotation lexicalAnnotation fixpoint) (Reservedid k z s l lexicalAnnotation lexicalAnnotation fixpoint) annotation fixpoint) }
-- | Fixpoint applied to 'LexemeF'
type Lexeme k z s l lexicalAnnotation annotation = Fixed.Fix (LexemeF k z s l lexicalAnnotation annotation)
-- | 'LiteralBase' with fewer unresolved variables, with default linking.
newtype LiteralF k z s l lexicalAnnotation annotation fixpoint = MkLiteralF { _unLiteralF :: (LiteralBase (Integer k z s l lexicalAnnotation lexicalAnnotation fixpoint) (Float k z s l lexicalAnnotation lexicalAnnotation fixpoint) (Char k z s l lexicalAnnotation lexicalAnnotation fixpoint) (String k z s l lexicalAnnotation lexicalAnnotation fixpoint) annotation fixpoint) }
-- | Fixpoint applied to 'LiteralF'
type Literal k z s l lexicalAnnotation annotation = Fixed.Fix (LiteralF k z s l lexicalAnnotation annotation)
-- | 'SpecialBase' with fewer unresolved variables, with default linking.
newtype SpecialF k z s l lexicalAnnotation annotation fixpoint = MkSpecialF { _unSpecialF :: (SpecialBase (l (LexicalLeftParenthesisKeyBase k z s)) (l (LexicalRightParenthesisKeyBase k z s)) (l (LexicalCommaKeyBase k z s)) (l (LexicalSemicolonKeyBase k z s)) (l (LexicalLeftBracketKeyBase k z s)) (l (LexicalRightBracketKeyBase k z s)) (l (LexicalBacktickKeyBase k z s)) (l (LexicalLeftBraceKeyBase k z s)) (l (LexicalRightBraceKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'SpecialF'
type Special k z s l lexicalAnnotation annotation = Fixed.Fix (SpecialF k z s l lexicalAnnotation annotation)
-- | 'SpecialSansNcBase' with fewer unresolved variables, with default linking.
newtype SpecialSansNcF k z s l lexicalAnnotation annotation fixpoint = MkSpecialSansNcF { _unSpecialSansNcF :: (SpecialSansNcBase (l (LexicalLeftParenthesisKeyBase k z s)) (l (LexicalRightParenthesisKeyBase k z s)) (l (LexicalCommaKeyBase k z s)) (l (LexicalSemicolonKeyBase k z s)) (l (LexicalLeftBracketKeyBase k z s)) (l (LexicalRightBracketKeyBase k z s)) (l (LexicalBacktickKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'SpecialSansNcF'
type SpecialSansNc k z s l lexicalAnnotation annotation = Fixed.Fix (SpecialSansNcF k z s l lexicalAnnotation annotation)
-- | 'WhitespaceBase' with fewer unresolved variables, with default linking.
newtype WhitespaceF k z s l lexicalAnnotation annotation fixpoint = MkWhitespaceF { _unWhitespaceF :: (WhitespaceBase [] (Whitestuff k z s l lexicalAnnotation lexicalAnnotation fixpoint) annotation fixpoint) }
-- | Fixpoint applied to 'WhitespaceF'
type Whitespace k z s l lexicalAnnotation annotation = Fixed.Fix (WhitespaceF k z s l lexicalAnnotation annotation)
-- | 'WhitestuffBase' with fewer unresolved variables, with default linking.
newtype WhitestuffF k z s l lexicalAnnotation annotation fixpoint = MkWhitestuffF { _unWhitestuffF :: (WhitestuffBase (Whitechar k z s l lexicalAnnotation lexicalAnnotation fixpoint) (Comment k z s l lexicalAnnotation lexicalAnnotation fixpoint) (Ncomment k z s l lexicalAnnotation lexicalAnnotation fixpoint) annotation fixpoint) }
-- | Fixpoint applied to 'WhitestuffF'
type Whitestuff k z s l lexicalAnnotation annotation = Fixed.Fix (WhitestuffF k z s l lexicalAnnotation annotation)
-- | 'WhitecharBase' with fewer unresolved variables, with default linking.
newtype WhitecharF k z s l lexicalAnnotation annotation fixpoint = MkWhitecharF { _unWhitecharF :: (WhitecharBase (Newline k z s l lexicalAnnotation lexicalAnnotation fixpoint) (Vertab k z s l lexicalAnnotation lexicalAnnotation fixpoint) (Space k z s l lexicalAnnotation lexicalAnnotation fixpoint) (Tab k z s l lexicalAnnotation lexicalAnnotation fixpoint) (UniWhite k z s l lexicalAnnotation lexicalAnnotation fixpoint) annotation fixpoint) }
-- | Fixpoint applied to 'WhitecharF'
type Whitechar k z s l lexicalAnnotation annotation = Fixed.Fix (WhitecharF k z s l lexicalAnnotation annotation)
-- | 'NewlineBase' with fewer unresolved variables, with default linking.
newtype NewlineF k z s l lexicalAnnotation annotation fixpoint = MkNewlineF { _unNewlineF :: (NewlineBase (Return k z s l lexicalAnnotation lexicalAnnotation fixpoint) (Linefeed k z s l lexicalAnnotation lexicalAnnotation fixpoint) (Formfeed k z s l lexicalAnnotation lexicalAnnotation fixpoint) annotation fixpoint) }
-- | Fixpoint applied to 'NewlineF'
type Newline k z s l lexicalAnnotation annotation = Fixed.Fix (NewlineF k z s l lexicalAnnotation annotation)
-- | 'ReturnBase' with fewer unresolved variables, with default linking.
newtype ReturnF k z s l lexicalAnnotation annotation fixpoint = MkReturnF { _unReturnF :: (ReturnBase (l (LexicalCRKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'ReturnF'
type Return k z s l lexicalAnnotation annotation = Fixed.Fix (ReturnF k z s l lexicalAnnotation annotation)
-- | 'LinefeedBase' with fewer unresolved variables, with default linking.
newtype LinefeedF k z s l lexicalAnnotation annotation fixpoint = MkLinefeedF { _unLinefeedF :: (LinefeedBase (l (LexicalLFKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'LinefeedF'
type Linefeed k z s l lexicalAnnotation annotation = Fixed.Fix (LinefeedF k z s l lexicalAnnotation annotation)
-- | 'VertabBase' with fewer unresolved variables, with default linking.
newtype VertabF k z s l lexicalAnnotation annotation fixpoint = MkVertabF { _unVertabF :: (VertabBase (l (LexicalVTKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'VertabF'
type Vertab k z s l lexicalAnnotation annotation = Fixed.Fix (VertabF k z s l lexicalAnnotation annotation)
-- | 'FormfeedBase' with fewer unresolved variables, with default linking.
newtype FormfeedF k z s l lexicalAnnotation annotation fixpoint = MkFormfeedF { _unFormfeedF :: (FormfeedBase (l (LexicalFFKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'FormfeedF'
type Formfeed k z s l lexicalAnnotation annotation = Fixed.Fix (FormfeedF k z s l lexicalAnnotation annotation)
-- | 'SpaceBase' with fewer unresolved variables, with default linking.
newtype SpaceF k z s l lexicalAnnotation annotation fixpoint = MkSpaceF { _unSpaceF :: (SpaceBase (l (LexicalSPKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'SpaceF'
type Space k z s l lexicalAnnotation annotation = Fixed.Fix (SpaceF k z s l lexicalAnnotation annotation)
-- | 'TabBase' with fewer unresolved variables, with default linking.
newtype TabF k z s l lexicalAnnotation annotation fixpoint = MkTabF { _unTabF :: (TabBase (l (LexicalHTKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'TabF'
type Tab k z s l lexicalAnnotation annotation = Fixed.Fix (TabF k z s l lexicalAnnotation annotation)
-- | 'UniWhiteBase' with fewer unresolved variables, with default linking.
newtype UniWhiteF k z s l lexicalAnnotation annotation fixpoint = MkUniWhiteF { _unUniWhiteF :: (UniWhiteBase (l (LexicalUnicodeWhitespaceCharKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'UniWhiteF'
type UniWhite k z s l lexicalAnnotation annotation = Fixed.Fix (UniWhiteF k z s l lexicalAnnotation annotation)
-- | 'CommentBase' with fewer unresolved variables, with default linking.
newtype CommentF k z s l lexicalAnnotation annotation fixpoint = MkCommentF { _unCommentF :: (CommentBase (,) Prelude.Maybe [] (Dashes k z s l lexicalAnnotation lexicalAnnotation fixpoint) (AnySansSymbol k z s l lexicalAnnotation lexicalAnnotation fixpoint) (Any k z s l lexicalAnnotation lexicalAnnotation fixpoint) (Newline k z s l lexicalAnnotation lexicalAnnotation fixpoint) annotation fixpoint) }
-- | Fixpoint applied to 'CommentF'
type Comment k z s l lexicalAnnotation annotation = Fixed.Fix (CommentF k z s l lexicalAnnotation annotation)
-- | 'DashesBase' with fewer unresolved variables, with default linking.
newtype DashesF k z s l lexicalAnnotation annotation fixpoint = MkDashesF { _unDashesF :: (DashesBase [] (l (LexicalHyphenKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'DashesF'
type Dashes k z s l lexicalAnnotation annotation = Fixed.Fix (DashesF k z s l lexicalAnnotation annotation)
-- | 'OpenComBase' with fewer unresolved variables, with default linking.
newtype OpenComF k z s l lexicalAnnotation annotation fixpoint = MkOpenComF { _unOpenComF :: (OpenComBase (l (LexicalLeftBraceKeyBase k z s)) (l (LexicalHyphenKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'OpenComF'
type OpenCom k z s l lexicalAnnotation annotation = Fixed.Fix (OpenComF k z s l lexicalAnnotation annotation)
-- | 'CloseComBase' with fewer unresolved variables, with default linking.
newtype CloseComF k z s l lexicalAnnotation annotation fixpoint = MkCloseComF { _unCloseComF :: (CloseComBase (l (LexicalHyphenKeyBase k z s)) (l (LexicalRightBraceKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'CloseComF'
type CloseCom k z s l lexicalAnnotation annotation = Fixed.Fix (CloseComF k z s l lexicalAnnotation annotation)
-- | 'NcommentBase' with fewer unresolved variables, with default linking.
newtype NcommentF k z s l lexicalAnnotation annotation fixpoint = MkNcommentF { _unNcommentF :: (NcommentBase (,) [] (OpenCom k z s l lexicalAnnotation lexicalAnnotation fixpoint) (BigAnySeq k z s l lexicalAnnotation lexicalAnnotation fixpoint) (CloseCom k z s l lexicalAnnotation lexicalAnnotation fixpoint) annotation fixpoint) }
-- | Fixpoint applied to 'NcommentF'
type Ncomment k z s l lexicalAnnotation annotation = Fixed.Fix (NcommentF k z s l lexicalAnnotation annotation)
-- | 'BigAnySeqBase' with fewer unresolved variables, with default linking.
newtype BigAnySeqF k z s l lexicalAnnotation annotation fixpoint = MkBigAnySeqF { _unBigAnySeqF :: (BigAnySeqBase (BigAnySansNc k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalRightBraceKeyBase k z s)) (l (LexicalLeftBraceKeyBase k z s)) (BigAnySeqValidNcomChar1_0 k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalHyphenKeyBase k z s)) (BigAnySeqValidNcomChar1_1 k z s l lexicalAnnotation lexicalAnnotation fixpoint) annotation fixpoint) }
-- | Fixpoint applied to 'BigAnySeqF'
type BigAnySeq k z s l lexicalAnnotation annotation = Fixed.Fix (BigAnySeqF k z s l lexicalAnnotation annotation)
-- | 'BigAnySeqValidNcomChar1_0Base' with fewer unresolved variables, with default linking.
newtype BigAnySeqValidNcomChar1_0F k z s l lexicalAnnotation annotation fixpoint = MkBigAnySeqValidNcomChar1_0F { _unBigAnySeqValidNcomChar1_0F :: (BigAnySeqValidNcomChar1_0Base (BigAnySansNc k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalLeftBraceKeyBase k z s)) (l (LexicalRightBraceKeyBase k z s)) (l (LexicalEndOfParseKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'BigAnySeqValidNcomChar1_0F'
type BigAnySeqValidNcomChar1_0 k z s l lexicalAnnotation annotation = Fixed.Fix (BigAnySeqValidNcomChar1_0F k z s l lexicalAnnotation annotation)
-- | 'BigAnySeqValidNcomChar1_1Base' with fewer unresolved variables, with default linking.
newtype BigAnySeqValidNcomChar1_1F k z s l lexicalAnnotation annotation fixpoint = MkBigAnySeqValidNcomChar1_1F { _unBigAnySeqValidNcomChar1_1F :: (BigAnySeqValidNcomChar1_1Base (BigAnySansNc k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalLeftBraceKeyBase k z s)) (BigAnySeqValidNconmChar1_0Base k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalHyphenKeyBase k z s)) (l (LexicalEndOfParseKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'BigAnySeqValidNcomChar1_1F'
type BigAnySeqValidNcomChar1_1 k z s l lexicalAnnotation annotation = Fixed.Fix (BigAnySeqValidNcomChar1_1F k z s l lexicalAnnotation annotation)
-- | 'BigAnyBase' with fewer unresolved variables, with default linking.
newtype BigAnyF k z s l lexicalAnnotation annotation fixpoint = MkBigAnyF { _unBigAnyF :: (BigAnyBase (Graphic k z s l lexicalAnnotation lexicalAnnotation fixpoint) (Whitechar k z s l lexicalAnnotation lexicalAnnotation fixpoint) annotation fixpoint) }
-- | Fixpoint applied to 'BigAnyF'
type BigAny k z s l lexicalAnnotation annotation = Fixed.Fix (BigAnyF k z s l lexicalAnnotation annotation)
-- | 'BigAnySansNcBase' with fewer unresolved variables, with default linking.
newtype BigAnySansNcF k z s l lexicalAnnotation annotation fixpoint = MkBigAnySansNcF { _unBigAnySansNcF :: (BigAnySansNcBase (GraphicSansNc k z s l lexicalAnnotation lexicalAnnotation fixpoint) (Whitechar k z s l lexicalAnnotation lexicalAnnotation fixpoint) annotation fixpoint) }
-- | Fixpoint applied to 'BigAnySansNcF'
type BigAnySansNc k z s l lexicalAnnotation annotation = Fixed.Fix (BigAnySansNcF k z s l lexicalAnnotation annotation)
-- | 'AnyBase' with fewer unresolved variables, with default linking.
newtype AnyF k z s l lexicalAnnotation annotation fixpoint = MkAnyF { _unAnyF :: (AnyBase (Graphic k z s l lexicalAnnotation lexicalAnnotation fixpoint) (Space k z s l lexicalAnnotation lexicalAnnotation fixpoint) (Tab k z s l lexicalAnnotation lexicalAnnotation fixpoint) annotation fixpoint) }
-- | Fixpoint applied to 'AnyF'
type Any k z s l lexicalAnnotation annotation = Fixed.Fix (AnyF k z s l lexicalAnnotation annotation)
-- | 'GraphicBase' with fewer unresolved variables, with default linking.
newtype GraphicF k z s l lexicalAnnotation annotation fixpoint = MkGraphicF { _unGraphicF :: (GraphicBase (Small k z s l lexicalAnnotation lexicalAnnotation fixpoint) (Large k z s l lexicalAnnotation lexicalAnnotation fixpoint) (Symbol k z s l lexicalAnnotation lexicalAnnotation fixpoint) (Digit k z s l lexicalAnnotation lexicalAnnotation fixpoint) (Special k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalDoubleQuoteKeyBase k z s)) (l (LexicalSingleQuoteKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'GraphicF'
type Graphic k z s l lexicalAnnotation annotation = Fixed.Fix (GraphicF k z s l lexicalAnnotation annotation)
-- | 'GraphicSansNcBase' with fewer unresolved variables, with default linking.
newtype GraphicSansNcF k z s l lexicalAnnotation annotation fixpoint = MkGraphicSansNcF { _unGraphicSansNcF :: (GraphicSansNcBase (Small k z s l lexicalAnnotation lexicalAnnotation fixpoint) (Large k z s l lexicalAnnotation lexicalAnnotation fixpoint) (SymbolSansNc k z s l lexicalAnnotation lexicalAnnotation fixpoint) (Digit k z s l lexicalAnnotation lexicalAnnotation fixpoint) (SpecialSansNc k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalDoubleQuoteKeyBase k z s)) (l (LexicalSingleQuoteKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'GraphicSansNcF'
type GraphicSansNc k z s l lexicalAnnotation annotation = Fixed.Fix (GraphicSansNcF k z s l lexicalAnnotation annotation)
-- | 'SmallBase' with fewer unresolved variables, with default linking.
newtype SmallF k z s l lexicalAnnotation annotation fixpoint = MkSmallF { _unSmallF :: (SmallBase (AscSmall k z s l lexicalAnnotation lexicalAnnotation fixpoint) (UniSmall k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalUnderscoreKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'SmallF'
type Small k z s l lexicalAnnotation annotation = Fixed.Fix (SmallF k z s l lexicalAnnotation annotation)
-- | 'AscSmallBase' with fewer unresolved variables, with default linking.
newtype AscSmallF k z s l lexicalAnnotation annotation fixpoint = MkAscSmallF { _unAscSmallF :: (AscSmallBase (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'AscSmallF'
type AscSmall k z s l lexicalAnnotation annotation = Fixed.Fix (AscSmallF k z s l lexicalAnnotation annotation)
-- | 'UniSmallBase' with fewer unresolved variables, with default linking.
newtype UniSmallF k z s l lexicalAnnotation annotation fixpoint = MkUniSmallF { _unUniSmallF :: (UniSmallBase (l (LexicalUnicodeSmallKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'UniSmallF'
type UniSmall k z s l lexicalAnnotation annotation = Fixed.Fix (UniSmallF k z s l lexicalAnnotation annotation)
-- | 'LargeBase' with fewer unresolved variables, with default linking.
newtype LargeF k z s l lexicalAnnotation annotation fixpoint = MkLargeF { _unLargeF :: (LargeBase (AscLarge k z s l lexicalAnnotation lexicalAnnotation fixpoint) (UniLarge k z s l lexicalAnnotation lexicalAnnotation fixpoint) annotation fixpoint) }
-- | Fixpoint applied to 'LargeF'
type Large k z s l lexicalAnnotation annotation = Fixed.Fix (LargeF k z s l lexicalAnnotation annotation)
-- | 'AscLargeBase' with fewer unresolved variables, with default linking.
newtype AscLargeF k z s l lexicalAnnotation annotation fixpoint = MkAscLargeF { _unAscLargeF :: (AscLargeBase (l (LexicalAKeyBase k z s)) (l (LexicalBKeyBase k z s)) (l (LexicalCKeyBase k z s)) (l (LexicalDKeyBase k z s)) (l (LexicalEKeyBase k z s)) (l (LexicalFKeyBase k z s)) (l (LexicalGKeyBase k z s)) (l (LexicalHKeyBase k z s)) (l (LexicalIKeyBase k z s)) (l (LexicalJKeyBase k z s)) (l (LexicalKKeyBase k z s)) (l (LexicalLKeyBase k z s)) (l (LexicalMKeyBase k z s)) (l (LexicalNKeyBase k z s)) (l (LexicalOKeyBase k z s)) (l (LexicalPKeyBase k z s)) (l (LexicalQKeyBase k z s)) (l (LexicalRKeyBase k z s)) (l (LexicalSKeyBase k z s)) (l (LexicalTKeyBase k z s)) (l (LexicalUKeyBase k z s)) (l (LexicalVKeyBase k z s)) (l (LexicalWKeyBase k z s)) (l (LexicalXKeyBase k z s)) (l (LexicalYKeyBase k z s)) (l (LexicalZKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'AscLargeF'
type AscLarge k z s l lexicalAnnotation annotation = Fixed.Fix (AscLargeF k z s l lexicalAnnotation annotation)
-- | 'UniLargeBase' with fewer unresolved variables, with default linking.
newtype UniLargeF k z s l lexicalAnnotation annotation fixpoint = MkUniLargeF { _unUniLargeF :: (UniLargeBase (l (LexicalUnicodeLargeKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'UniLargeF'
type UniLarge k z s l lexicalAnnotation annotation = Fixed.Fix (UniLargeF k z s l lexicalAnnotation annotation)
-- | 'SymbolBase' with fewer unresolved variables, with default linking.
newtype SymbolF k z s l lexicalAnnotation annotation fixpoint = MkSymbolF { _unSymbolF :: (SymbolBase (AscSymbol k z s l lexicalAnnotation lexicalAnnotation fixpoint) (UniSymbolSansSpecialish k z s l lexicalAnnotation lexicalAnnotation fixpoint) annotation fixpoint) }
-- | Fixpoint applied to 'SymbolF'
type Symbol k z s l lexicalAnnotation annotation = Fixed.Fix (SymbolF k z s l lexicalAnnotation annotation)
-- | 'SymbolSansNcBase' with fewer unresolved variables, with default linking.
newtype SymbolSansNcF k z s l lexicalAnnotation annotation fixpoint = MkSymbolSansNcF { _unSymbolSansNcF :: (SymbolSansNcBase (AscSymbolSansNc k z s l lexicalAnnotation lexicalAnnotation fixpoint) (UniSymbolSansSpecialishSansNc k z s l lexicalAnnotation lexicalAnnotation fixpoint) annotation fixpoint) }
-- | Fixpoint applied to 'SymbolSansNcF'
type SymbolSansNc k z s l lexicalAnnotation annotation = Fixed.Fix (SymbolSansNcF k z s l lexicalAnnotation annotation)
-- | 'AscSymbolBase' with fewer unresolved variables, with default linking.
newtype AscSymbolF k z s l lexicalAnnotation annotation fixpoint = MkAscSymbolF { _unAscSymbolF :: (AscSymbolBase (l (LexicalExclamationKeyBase k z s)) (l (LexicalHashKeyBase k z s)) (l (LexicalDollarKeyBase k z s)) (l (LexicalPercentKeyBase k z s)) (l (LexicalAmpersandKeyBase k z s)) (l (LexicalAsteriskKeyBase k z s)) (l (LexicalPlusKeyBase k z s)) (l (LexicalDotKeyBase k z s)) (l (LexicalSlashKeyBase k z s)) (l (LexicalLeftAngleBracketKeyBase k z s)) (l (LexicalEqualsKeyBase k z s)) (l (LexicalRightAngleBracketKeyBase k z s)) (l (LexicalQuestionMarkKeyBase k z s)) (l (LexicalAtKeyBase k z s)) (l (LexicalBackslashKeyBase k z s)) (l (LexicalCaretKeyBase k z s)) (l (LexicalPipeKeyBase k z s)) (l (LexicalHyphenKeyBase k z s)) (l (LexicalTildeKeyBase k z s)) (l (LexicalColonKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'AscSymbolF'
type AscSymbol k z s l lexicalAnnotation annotation = Fixed.Fix (AscSymbolF k z s l lexicalAnnotation annotation)
-- | 'AscSymbolSansNcBase' with fewer unresolved variables, with default linking.
newtype AscSymbolSansNcF k z s l lexicalAnnotation annotation fixpoint = MkAscSymbolSansNcF { _unAscSymbolSansNcF :: (AscSymbolSansNcBase (l (LexicalExclamationKeyBase k z s)) (l (LexicalHashKeyBase k z s)) (l (LexicalDollarKeyBase k z s)) (l (LexicalPercentKeyBase k z s)) (l (LexicalAmpersandKeyBase k z s)) (l (LexicalAsteriskKeyBase k z s)) (l (LexicalPlusKeyBase k z s)) (l (LexicalDotKeyBase k z s)) (l (LexicalSlashKeyBase k z s)) (l (LexicalLeftAngleBracketKeyBase k z s)) (l (LexicalEqualsKeyBase k z s)) (l (LexicalRightAngleBracketKeyBase k z s)) (l (LexicalQuestionMarkKeyBase k z s)) (l (LexicalAtKeyBase k z s)) (l (LexicalBackslashKeyBase k z s)) (l (LexicalCaretKeyBase k z s)) (l (LexicalPipeKeyBase k z s)) (l (LexicalTildeKeyBase k z s)) (l (LexicalColonKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'AscSymbolSansNcF'
type AscSymbolSansNc k z s l lexicalAnnotation annotation = Fixed.Fix (AscSymbolSansNcF k z s l lexicalAnnotation annotation)
-- | 'UniSymbolBase' with fewer unresolved variables, with default linking.
newtype UniSymbolF k z s l lexicalAnnotation annotation fixpoint = MkUniSymbolF { _unUniSymbolF :: (UniSymbolBase (l (LexicalUnicodeSymbolKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'UniSymbolF'
type UniSymbol k z s l lexicalAnnotation annotation = Fixed.Fix (UniSymbolF k z s l lexicalAnnotation annotation)
-- | 'UniSymbolSansSpecialishBase' with fewer unresolved variables, with default linking.
newtype UniSymbolSansSpecialishF k z s l lexicalAnnotation annotation fixpoint = MkUniSymbolSansSpecialishF { _unUniSymbolSansSpecialishF :: (UniSymbolSansSpecialishBase (l (LexicalUnicodeSymbolSansSpecialUnderscoreDoubleQuoteSingleQuoteKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'UniSymbolSansSpecialishF'
type UniSymbolSansSpecialish k z s l lexicalAnnotation annotation = Fixed.Fix (UniSymbolSansSpecialishF k z s l lexicalAnnotation annotation)
-- | 'UniSymbolSansSpecialishSansNcBase' with fewer unresolved variables, with default linking.
newtype UniSymbolSansSpecialishSansNcF k z s l lexicalAnnotation annotation fixpoint = MkUniSymbolSansSpecialishSansNcF { _unUniSymbolSansSpecialishSansNcF :: (UniSymbolSansSpecialishSansNcBase (l (LexicalUnicodeSymbolSansSpecialUnderscoreDoubleQuoteSingleQuoteKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'UniSymbolSansSpecialishSansNcF'
type UniSymbolSansSpecialishSansNc k z s l lexicalAnnotation annotation = Fixed.Fix (UniSymbolSansSpecialishSansNcF k z s l lexicalAnnotation annotation)
-- | 'DigitBase' with fewer unresolved variables, with default linking.
newtype DigitF k z s l lexicalAnnotation annotation fixpoint = MkDigitF { _unDigitF :: (DigitBase (AscDigit k z s l lexicalAnnotation lexicalAnnotation fixpoint) (UniDigit k z s l lexicalAnnotation lexicalAnnotation fixpoint) annotation fixpoint) }
-- | Fixpoint applied to 'DigitF'
type Digit k z s l lexicalAnnotation annotation = Fixed.Fix (DigitF k z s l lexicalAnnotation annotation)
-- | 'AscDigitBase' with fewer unresolved variables, with default linking.
newtype AscDigitF k z s l lexicalAnnotation annotation fixpoint = MkAscDigitF { _unAscDigitF :: (AscDigitBase (l (Lexical0KeyBase k z s)) (l (Lexical1KeyBase k z s)) (l (Lexical2KeyBase k z s)) (l (Lexical3KeyBase k z s)) (l (Lexical4KeyBase k z s)) (l (Lexical5KeyBase k z s)) (l (Lexical6KeyBase k z s)) (l (Lexical7KeyBase k z s)) (l (Lexical8KeyBase k z s)) (l (Lexical9KeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'AscDigitF'
type AscDigit k z s l lexicalAnnotation annotation = Fixed.Fix (AscDigitF k z s l lexicalAnnotation annotation)
-- | 'UniDigitBase' with fewer unresolved variables, with default linking.
newtype UniDigitF k z s l lexicalAnnotation annotation fixpoint = MkUniDigitF { _unUniDigitF :: (UniDigitBase (l (LexicalUnicodeDigitKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'UniDigitF'
type UniDigit k z s l lexicalAnnotation annotation = Fixed.Fix (UniDigitF k z s l lexicalAnnotation annotation)
-- | 'OctitBase' with fewer unresolved variables, with default linking.
newtype OctitF k z s l lexicalAnnotation annotation fixpoint = MkOctitF { _unOctitF :: (OctitBase (l (Lexical0KeyBase k z s)) (l (Lexical1KeyBase k z s)) (l (Lexical2KeyBase k z s)) (l (Lexical3KeyBase k z s)) (l (Lexical4KeyBase k z s)) (l (Lexical5KeyBase k z s)) (l (Lexical6KeyBase k z s)) (l (Lexical7KeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'OctitF'
type Octit k z s l lexicalAnnotation annotation = Fixed.Fix (OctitF k z s l lexicalAnnotation annotation)
-- | 'HexitBase' with fewer unresolved variables, with default linking.
newtype HexitF k z s l lexicalAnnotation annotation fixpoint = MkHexitF { _unHexitF :: (HexitBase (Digit k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalAKeyBase k z s)) (l (LexicalBKeyBase k z s)) (l (LexicalCKeyBase k z s)) (l (LexicalDKeyBase k z s)) (l (LexicalEKeyBase k z s)) (l (LexicalFKeyBase k z s)) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'HexitF'
type Hexit k z s l lexicalAnnotation annotation = Fixed.Fix (HexitF k z s l lexicalAnnotation annotation)

-- § 2.4 Identifiers and Operators types.

-- | 'UnqualifiedNameBase' with fewer unresolved variables, with default linking.
newtype UnqualifiedNameF k z s l lexicalAnnotation annotation fixpoint = MkUnqualifiedNameF { _unUnqualifiedNameF :: (UnqualifiedNameBase (Varid k z s l lexicalAnnotation lexicalAnnotation fixpoint) (Conid k z s l lexicalAnnotation lexicalAnnotation fixpoint) (Tyvar k z s l lexicalAnnotation lexicalAnnotation fixpoint) (Tycon k z s l lexicalAnnotation lexicalAnnotation fixpoint) (Tycls k z s l lexicalAnnotation lexicalAnnotation fixpoint) (Modid k z s l lexicalAnnotation lexicalAnnotation fixpoint) annotation fixpoint) }
-- | Fixpoint applied to 'UnqualifiedNameF'
type UnqualifiedName k z s l lexicalAnnotation annotation = Fixed.Fix (UnqualifiedNameF k z s l lexicalAnnotation annotation)
-- | 'VaridNoExclusionsBase' with fewer unresolved variables, with default linking.
newtype VaridNoExclusionsF k z s l lexicalAnnotation annotation fixpoint = MkVaridNoExclusionsF { _unVaridNoExclusionsF :: (VaridNoExclusionsBase [] (VaridStart k z s l lexicalAnnotation lexicalAnnotation fixpoint) (VaridInner k z s l lexicalAnnotation lexicalAnnotation fixpoint) annotation fixpoint) }
-- | Fixpoint applied to 'VaridNoExclusionsF'
type VaridNoExclusions k z s l lexicalAnnotation annotation = Fixed.Fix (VaridNoExclusionsF k z s l lexicalAnnotation annotation)
-- | 'VaridStartBase' with fewer unresolved variables, with default linking.
newtype VaridStartF k z s l lexicalAnnotation annotation fixpoint = MkVaridStartF { _unVaridStartF :: (VaridStartBase (Small k z s l lexicalAnnotation lexicalAnnotation fixpoint) annotation fixpoint) }
-- | Fixpoint applied to 'VaridStartF'
type VaridStart k z s l lexicalAnnotation annotation = Fixed.Fix (VaridStartF k z s l lexicalAnnotation annotation)
-- | 'IdentifierInnerBase' with fewer unresolved variables, with default linking.
newtype IdentifierInnerF k z s l lexicalAnnotation annotation fixpoint = MkIdentifierInnerF { _unIdentifierInnerF :: (IdentifierInnerBase (Small k z s l lexicalAnnotation lexicalAnnotation fixpoint) (Large k z s l lexicalAnnotation lexicalAnnotation fixpoint) (Digit k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalSingleQuoteKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'IdentifierInnerF'
type IdentifierInner k z s l lexicalAnnotation annotation = Fixed.Fix (IdentifierInnerF k z s l lexicalAnnotation annotation)
-- | 'VaridInnerBase' with fewer unresolved variables, with default linking.
newtype VaridInnerF k z s l lexicalAnnotation annotation fixpoint = MkVaridInnerF { _unVaridInnerF :: (VaridInnerBase (IdentifierInner k z s l lexicalAnnotation lexicalAnnotation fixpoint) annotation fixpoint) }
-- | Fixpoint applied to 'VaridInnerF'
type VaridInner k z s l lexicalAnnotation annotation = Fixed.Fix (VaridInnerF k z s l lexicalAnnotation annotation)
-- | 'ConidBase' with fewer unresolved variables, with default linking.
newtype ConidF k z s l lexicalAnnotation annotation fixpoint = MkConidF { _unConidF :: (ConidBase [] (ConidStart k z s l lexicalAnnotation lexicalAnnotation fixpoint) (ConidInner k z s l lexicalAnnotation lexicalAnnotation fixpoint) annotation fixpoint) }
-- | Fixpoint applied to 'ConidF'
type Conid k z s l lexicalAnnotation annotation = Fixed.Fix (ConidF k z s l lexicalAnnotation annotation)
-- | 'ConidStartBase' with fewer unresolved variables, with default linking.
newtype ConidStartF k z s l lexicalAnnotation annotation fixpoint = MkConidStartF { _unConidStartF :: (ConidStartBase (Large k z s l lexicalAnnotation lexicalAnnotation fixpoint) annotation fixpoint) }
-- | Fixpoint applied to 'ConidStartF'
type ConidStart k z s l lexicalAnnotation annotation = Fixed.Fix (ConidStartF k z s l lexicalAnnotation annotation)
-- | 'ConidInnerBase' with fewer unresolved variables, with default linking.
newtype ConidInnerF k z s l lexicalAnnotation annotation fixpoint = MkConidInnerF { _unConidInnerF :: (ConidInnerBase (IdentifierInner k z s l lexicalAnnotation lexicalAnnotation fixpoint) annotation fixpoint) }
-- | Fixpoint applied to 'ConidInnerF'
type ConidInner k z s l lexicalAnnotation annotation = Fixed.Fix (ConidInnerF k z s l lexicalAnnotation annotation)
-- | 'ReservedidBase' with fewer unresolved variables, with default linking.
newtype ReservedidF k z s l lexicalAnnotation annotation fixpoint = MkReservedidF { _unReservedidF :: (ReservedidBase (LexicalCase k z s l lexicalAnnotation lexicalAnnotation fixpoint) (LexicalClass k z s l lexicalAnnotation lexicalAnnotation fixpoint) (LexicalData k z s l lexicalAnnotation lexicalAnnotation fixpoint) (LexicalDefault k z s l lexicalAnnotation lexicalAnnotation fixpoint) (LexicalDeriving k z s l lexicalAnnotation lexicalAnnotation fixpoint) (LexicalDo k z s l lexicalAnnotation lexicalAnnotation fixpoint) (LexicalElse k z s l lexicalAnnotation lexicalAnnotation fixpoint) (LexicalForeign k z s l lexicalAnnotation lexicalAnnotation fixpoint) (LexicalIf k z s l lexicalAnnotation lexicalAnnotation fixpoint) (LexicalImport k z s l lexicalAnnotation lexicalAnnotation fixpoint) (LexicalIn k z s l lexicalAnnotation lexicalAnnotation fixpoint) (LexicalInfix k z s l lexicalAnnotation lexicalAnnotation fixpoint) (LexicalInfixl k z s l lexicalAnnotation lexicalAnnotation fixpoint) (LexicalInfixr k z s l lexicalAnnotation lexicalAnnotation fixpoint) (LexicalInstance k z s l lexicalAnnotation lexicalAnnotation fixpoint) (LexicalLet k z s l lexicalAnnotation lexicalAnnotation fixpoint) (LexicalModule k z s l lexicalAnnotation lexicalAnnotation fixpoint) (LexicalNewtype k z s l lexicalAnnotation lexicalAnnotation fixpoint) (LexicalOf k z s l lexicalAnnotation lexicalAnnotation fixpoint) (LexicalThen k z s l lexicalAnnotation lexicalAnnotation fixpoint) (LexicalType k z s l lexicalAnnotation lexicalAnnotation fixpoint) (LexicalWhere k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalUnderscoreKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'ReservedidF'
type Reservedid k z s l lexicalAnnotation annotation = Fixed.Fix (ReservedidF k z s l lexicalAnnotation annotation)
-- | 'VarSymNoExtraExclusionsBase' with fewer unresolved variables, with default linking.
newtype VarSymNoExtraExclusionsF k z s l lexicalAnnotation annotation fixpoint = MkVarSymNoExtraExclusionsF { _unVarSymNoExtraExclusionsF :: (VarSymNoExtraExclusionsBase [] (VarSymStart k z s l lexicalAnnotation lexicalAnnotation fixpoint) (Symbol k z s l lexicalAnnotation lexicalAnnotation fixpoint) annotation fixpoint) }
-- | Fixpoint applied to 'VarSymNoExtraExclusionsF'
type VarSymNoExtraExclusions k z s l lexicalAnnotation annotation = Fixed.Fix (VarSymNoExtraExclusionsF k z s l lexicalAnnotation annotation)
-- | 'VarSymStartBase' with fewer unresolved variables, with default linking.
newtype VarSymStartF k z s l lexicalAnnotation annotation fixpoint = MkVarSymStartF { _unVarSymStartF :: (VarSymStartBase (SymbolSansColon k z s l lexicalAnnotation lexicalAnnotation fixpoint) annotation fixpoint) }
-- | Fixpoint applied to 'VarSymStartF'
type VarSymStart k z s l lexicalAnnotation annotation = Fixed.Fix (VarSymStartF k z s l lexicalAnnotation annotation)
-- | 'ConSymNoExtraExclusionsBase' with fewer unresolved variables, with default linking.
newtype ConSymNoExtraExclusionsF k z s l lexicalAnnotation annotation fixpoint = MkConSymNoExtraExclusionsF { _unConSymNoExtraExclusionsF :: (ConSymNoExtraExclusionsBase [] (ConSymStart k z s l lexicalAnnotation lexicalAnnotation fixpoint) (Symbol k z s l lexicalAnnotation lexicalAnnotation fixpoint) annotation fixpoint) }
-- | Fixpoint applied to 'ConSymNoExtraExclusionsF'
type ConSymNoExtraExclusions k z s l lexicalAnnotation annotation = Fixed.Fix (ConSymNoExtraExclusionsF k z s l lexicalAnnotation annotation)
-- | 'ConSymStartBase' with fewer unresolved variables, with default linking.
newtype ConSymStartF k z s l lexicalAnnotation annotation fixpoint = MkConSymStartF { _unConSymStartF :: (ConSymStartBase (l (LexicalColonKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'ConSymStartF'
type ConSymStart k z s l lexicalAnnotation annotation = Fixed.Fix (ConSymStartF k z s l lexicalAnnotation annotation)
-- | 'ReservedOpBase' with fewer unresolved variables, with default linking.
newtype ReservedOpF k z s l lexicalAnnotation annotation fixpoint = MkReservedOpF { _unReservedOpF :: (ReservedOpBase (LexicalDotDot k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalColonKeyBase k z s)) (LexicalDoubleColon k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalEqualsKeyBase k z s)) (l (LexicalBackslashKeyBase k z s)) (l (LexicalPipeKeyBase k z s)) (LexicalLeftArrow k z s l lexicalAnnotation lexicalAnnotation fixpoint) (LexicalRightArrow k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalAtKeyBase k z s)) (l (LexicalTildeKeyBase k z s)) (LexicalDoubleRightArrow k z s l lexicalAnnotation lexicalAnnotation fixpoint) annotation fixpoint) }
-- | Fixpoint applied to 'ReservedOpF'
type ReservedOp k z s l lexicalAnnotation annotation = Fixed.Fix (ReservedOpF k z s l lexicalAnnotation annotation)
-- | 'TyvarBase' with fewer unresolved variables, with default linking.
newtype TyvarF k z s l lexicalAnnotation annotation fixpoint = MkTyvarF { _unTyvarF :: (TyvarBase (Conid k z s l lexicalAnnotation lexicalAnnotation fixpoint) annotation fixpoint) }
-- | Fixpoint applied to 'TyvarF'
type Tyvar k z s l lexicalAnnotation annotation = Fixed.Fix (TyvarF k z s l lexicalAnnotation annotation)
-- | 'TyconBase' with fewer unresolved variables, with default linking.
newtype TyconF k z s l lexicalAnnotation annotation fixpoint = MkTyconF { _unTyconF :: (TyconBase (Varid k z s l lexicalAnnotation lexicalAnnotation fixpoint) annotation fixpoint) }
-- | Fixpoint applied to 'TyconF'
type Tycon k z s l lexicalAnnotation annotation = Fixed.Fix (TyconF k z s l lexicalAnnotation annotation)
-- | 'TyclsBase' with fewer unresolved variables, with default linking.
newtype TyclsF k z s l lexicalAnnotation annotation fixpoint = MkTyclsF { _unTyclsF :: (TyclsBase (Conid k z s l lexicalAnnotation lexicalAnnotation fixpoint) annotation fixpoint) }
-- | Fixpoint applied to 'TyclsF'
type Tycls k z s l lexicalAnnotation annotation = Fixed.Fix (TyclsF k z s l lexicalAnnotation annotation)
-- | 'ModidBase' with fewer unresolved variables, with default linking.
newtype ModidF k z s l lexicalAnnotation annotation fixpoint = MkModidF { _unModidF :: (ModidBase (,) [] (Conid k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalDotKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'ModidF'
type Modid k z s l lexicalAnnotation annotation = Fixed.Fix (ModidF k z s l lexicalAnnotation annotation)
-- | 'NameBase' with fewer unresolved variables, with default linking.
newtype NameF k z s l lexicalAnnotation annotation fixpoint = MkNameF { _unNameF :: (NameBase (Qvarid k z s l lexicalAnnotation lexicalAnnotation fixpoint) (Qconid k z s l lexicalAnnotation lexicalAnnotation fixpoint) (Qtycon k z s l lexicalAnnotation lexicalAnnotation fixpoint) (Qtycls k z s l lexicalAnnotation lexicalAnnotation fixpoint) (QvarSym k z s l lexicalAnnotation lexicalAnnotation fixpoint) (QconSym k z s l lexicalAnnotation lexicalAnnotation fixpoint) annotation fixpoint) }
-- | Fixpoint applied to 'NameF'
type Name k z s l lexicalAnnotation annotation = Fixed.Fix (NameF k z s l lexicalAnnotation annotation)
-- | 'QvaridBase' with fewer unresolved variables, with default linking.
newtype QvaridF k z s l lexicalAnnotation annotation fixpoint = MkQvaridF { _unQvaridF :: (QvaridBase (,) Prelude.Maybe (Modid k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalDotKeyBase k z s)) (Varid k z s l lexicalAnnotation lexicalAnnotation fixpoint) annotation fixpoint) }
-- | Fixpoint applied to 'QvaridF'
type Qvarid k z s l lexicalAnnotation annotation = Fixed.Fix (QvaridF k z s l lexicalAnnotation annotation)
-- | 'QconidBase' with fewer unresolved variables, with default linking.
newtype QconidF k z s l lexicalAnnotation annotation fixpoint = MkQconidF { _unQconidF :: (QconidBase (,) Prelude.Maybe (Modid k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalDotKeyBase k z s)) (Conid k z s l lexicalAnnotation lexicalAnnotation fixpoint) annotation fixpoint) }
-- | Fixpoint applied to 'QconidF'
type Qconid k z s l lexicalAnnotation annotation = Fixed.Fix (QconidF k z s l lexicalAnnotation annotation)
-- | 'QtyconBase' with fewer unresolved variables, with default linking.
newtype QtyconF k z s l lexicalAnnotation annotation fixpoint = MkQtyconF { _unQtyconF :: (QtyconBase (,) Prelude.Maybe (Modid k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalDotKeyBase k z s)) (Tycon k z s l lexicalAnnotation lexicalAnnotation fixpoint) annotation fixpoint) }
-- | Fixpoint applied to 'QtyconF'
type Qtycon k z s l lexicalAnnotation annotation = Fixed.Fix (QtyconF k z s l lexicalAnnotation annotation)
-- | 'QtyclsBase' with fewer unresolved variables, with default linking.
newtype QtyclsF k z s l lexicalAnnotation annotation fixpoint = MkQtyclsF { _unQtyclsF :: (QtyclsBase (,) Prelude.Maybe (Modid k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalDotKeyBase k z s)) (Tycls k z s l lexicalAnnotation lexicalAnnotation fixpoint) annotation fixpoint) }
-- | Fixpoint applied to 'QtyclsF'
type Qtycls k z s l lexicalAnnotation annotation = Fixed.Fix (QtyclsF k z s l lexicalAnnotation annotation)
-- | 'QvarSymBase' with fewer unresolved variables, with default linking.
newtype QvarSymF k z s l lexicalAnnotation annotation fixpoint = MkQvarSymF { _unQvarSymF :: (QvarSymBase (,) Prelude.Maybe (Modid k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalDotKeyBase k z s)) (VarSym k z s l lexicalAnnotation lexicalAnnotation fixpoint) annotation fixpoint) }
-- | Fixpoint applied to 'QvarSymF'
type QvarSym k z s l lexicalAnnotation annotation = Fixed.Fix (QvarSymF k z s l lexicalAnnotation annotation)
-- | 'QconSymBase' with fewer unresolved variables, with default linking.
newtype QconSymF k z s l lexicalAnnotation annotation fixpoint = MkQconSymF { _unQconSymF :: (QconSymBase (,) Prelude.Maybe (Modid k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalDotKeyBase k z s)) (ConSym k z s l lexicalAnnotation lexicalAnnotation fixpoint) annotation fixpoint) }
-- | Fixpoint applied to 'QconSymF'
type QconSym k z s l lexicalAnnotation annotation = Fixed.Fix (QconSymF k z s l lexicalAnnotation annotation)

-- § 2.5 Numeric Literals types.

-- | 'DecimalBase' with fewer unresolved variables, with default linking.
newtype DecimalF k z s l lexicalAnnotation annotation fixpoint = MkDecimalF { _unDecimalF :: (DecimalBase [] (Digit k z s l lexicalAnnotation lexicalAnnotation fixpoint) annotation fixpoint) }
-- | Fixpoint applied to 'DecimalF'
type Decimal k z s l lexicalAnnotation annotation = Fixed.Fix (DecimalF k z s l lexicalAnnotation annotation)
-- | 'OctalBase' with fewer unresolved variables, with default linking.
newtype OctalF k z s l lexicalAnnotation annotation fixpoint = MkOctalF { _unOctalF :: (OctalBase [] (Octit k z s l lexicalAnnotation lexicalAnnotation fixpoint) annotation fixpoint) }
-- | Fixpoint applied to 'OctalF'
type Octal k z s l lexicalAnnotation annotation = Fixed.Fix (OctalF k z s l lexicalAnnotation annotation)
-- | 'HexadecimalBase' with fewer unresolved variables, with default linking.
newtype HexadecimalF k z s l lexicalAnnotation annotation fixpoint = MkHexadecimalF { _unHexadecimalF :: (HexadecimalBase [] (Hexit k z s l lexicalAnnotation lexicalAnnotation fixpoint) annotation fixpoint) }
-- | Fixpoint applied to 'HexadecimalF'
type Hexadecimal k z s l lexicalAnnotation annotation = Fixed.Fix (HexadecimalF k z s l lexicalAnnotation annotation)
-- | 'IntegerBase' with fewer unresolved variables, with default linking.
newtype IntegerF k z s l lexicalAnnotation annotation fixpoint = MkIntegerF { _unIntegerF :: (IntegerBase (Decimal k z s l lexicalAnnotation lexicalAnnotation fixpoint) (Lexical0o k z s l lexicalAnnotation lexicalAnnotation fixpoint) (Octal k z s l lexicalAnnotation lexicalAnnotation fixpoint) (Lexical0O k z s l lexicalAnnotation lexicalAnnotation fixpoint) (Lexical0x k z s l lexicalAnnotation lexicalAnnotation fixpoint) (Hexadecimal k z s l lexicalAnnotation lexicalAnnotation fixpoint) (Lexical0X k z s l lexicalAnnotation lexicalAnnotation fixpoint) annotation fixpoint) }
-- | Fixpoint applied to 'IntegerF'
type Integer k z s l lexicalAnnotation annotation = Fixed.Fix (IntegerF k z s l lexicalAnnotation annotation)
-- | 'FloatBase' with fewer unresolved variables, with default linking.
newtype FloatF k z s l lexicalAnnotation annotation fixpoint = MkFloatF { _unFloatF :: (FloatBase Prelude.Maybe (Decimal k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalDotKeyBase k z s)) (Exponent k z s l lexicalAnnotation lexicalAnnotation fixpoint) annotation fixpoint) }
-- | Fixpoint applied to 'FloatF'
type Float k z s l lexicalAnnotation annotation = Fixed.Fix (FloatF k z s l lexicalAnnotation annotation)
-- | 'ExponentBase' with fewer unresolved variables, with default linking.
newtype ExponentF k z s l lexicalAnnotation annotation fixpoint = MkExponentF { _unExponentF :: (ExponentBase Prelude.Either Prelude.Maybe (l (LexicalELowerKeyBase k z s)) (l (LexicalEKeyBase k z s)) (l (LexicalPlusKeyBase k z s)) (LexicalMinus k z s l lexicalAnnotation lexicalAnnotation fixpoint) (Decimal k z s l lexicalAnnotation lexicalAnnotation fixpoint) annotation fixpoint) }
-- | Fixpoint applied to 'ExponentF'
type Exponent k z s l lexicalAnnotation annotation = Fixed.Fix (ExponentF k z s l lexicalAnnotation annotation)

-- § 2.6 Character and String Literals types.

-- | 'CharBase' with fewer unresolved variables, with default linking.
newtype CharF k z s l lexicalAnnotation annotation fixpoint = MkCharF { _unCharF :: (CharBase (l (LexicalSingleQuoteKeyBase k z s)) (CharLiteralInner k z s l lexicalAnnotation lexicalAnnotation fixpoint) annotation fixpoint) }
-- | Fixpoint applied to 'CharF'
type Char k z s l lexicalAnnotation annotation = Fixed.Fix (CharF k z s l lexicalAnnotation annotation)
-- | 'CharLiteralInnerBase' with fewer unresolved variables, with default linking.
newtype CharLiteralInnerF k z s l lexicalAnnotation annotation fixpoint = MkCharLiteralInnerF { _unCharLiteralInnerF :: (CharLiteralInnerBase (GraphicSansSingleQuoteOrBackslash k z s l lexicalAnnotation lexicalAnnotation fixpoint) (Space k z s l lexicalAnnotation lexicalAnnotation fixpoint) (EscapeSansBackslashAndAmpersand k z s l lexicalAnnotation lexicalAnnotation fixpoint) annotation fixpoint) }
-- | Fixpoint applied to 'CharLiteralInnerF'
type CharLiteralInner k z s l lexicalAnnotation annotation = Fixed.Fix (CharLiteralInnerF k z s l lexicalAnnotation annotation)
-- | 'StringBase' with fewer unresolved variables, with default linking.
newtype StringF k z s l lexicalAnnotation annotation fixpoint = MkStringF { _unStringF :: (StringBase [] (l (LexicalDoubleQuoteKeyBase k z s)) (StringLiteralInnerUnit k z s l lexicalAnnotation lexicalAnnotation fixpoint) annotation fixpoint) }
-- | Fixpoint applied to 'StringF'
type String k z s l lexicalAnnotation annotation = Fixed.Fix (StringF k z s l lexicalAnnotation annotation)
-- | 'StringLiteralInnerUnitBase' with fewer unresolved variables, with default linking.
newtype StringLiteralInnerUnitF k z s l lexicalAnnotation annotation fixpoint = MkStringLiteralInnerUnitF { _unStringLiteralInnerUnitF :: (StringLiteralInnerUnitBase (GraphicSansDoubleQuoteOrBackslash k z s l lexicalAnnotation lexicalAnnotation fixpoint) (Space k z s l lexicalAnnotation lexicalAnnotation fixpoint) (Escape k z s l lexicalAnnotation lexicalAnnotation fixpoint) (Gap k z s l lexicalAnnotation lexicalAnnotation fixpoint) annotation fixpoint) }
-- | Fixpoint applied to 'StringLiteralInnerUnitF'
type StringLiteralInnerUnit k z s l lexicalAnnotation annotation = Fixed.Fix (StringLiteralInnerUnitF k z s l lexicalAnnotation annotation)
-- | 'EscapeBase' with fewer unresolved variables, with default linking.
newtype EscapeF k z s l lexicalAnnotation annotation fixpoint = MkEscapeF { _unEscapeF :: (EscapeBase (l (LexicalBackslashKeyBase k z s)) (EscapeInner k z s l lexicalAnnotation lexicalAnnotation fixpoint) annotation fixpoint) }
-- | Fixpoint applied to 'EscapeF'
type Escape k z s l lexicalAnnotation annotation = Fixed.Fix (EscapeF k z s l lexicalAnnotation annotation)
-- | 'EscapeInnerBase' with fewer unresolved variables, with default linking.
newtype EscapeInnerF k z s l lexicalAnnotation annotation fixpoint = MkEscapeInnerF { _unEscapeInnerF :: (EscapeInnerBase (CharEsc k z s l lexicalAnnotation lexicalAnnotation fixpoint) (Ascii k z s l lexicalAnnotation lexicalAnnotation fixpoint) (Decimal k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalOLowerKeyBase k z s)) (Octal k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalXLowerKeyBase k z s)) (Hexadecimal k z s l lexicalAnnotation lexicalAnnotation fixpoint) annotation fixpoint) }
-- | Fixpoint applied to 'EscapeInnerF'
type EscapeInner k z s l lexicalAnnotation annotation = Fixed.Fix (EscapeInnerF k z s l lexicalAnnotation annotation)
-- | 'CharEscBase' with fewer unresolved variables, with default linking.
newtype CharEscF k z s l lexicalAnnotation annotation fixpoint = MkCharEscF { _unCharEscF :: (CharEscBase (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalBackslashKeyBase k z s)) (l (LexicalDoubleQuoteKeyBase k z s)) (l (LexicalSingleQuoteKeyBase k z s)) (l (LexicalAmpersandKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'CharEscF'
type CharEsc k z s l lexicalAnnotation annotation = Fixed.Fix (CharEscF k z s l lexicalAnnotation annotation)
-- | 'AsciiBase' with fewer unresolved variables, with default linking.
newtype AsciiF k z s l lexicalAnnotation annotation fixpoint = MkAsciiF { _unAsciiF :: (AsciiBase (l (LexicalCaretKeyBase k z s)) (Cntrl k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalNULKeyBase k z s)) (l (LexicalSOHKeyBase k z s)) (l (LexicalSTXKeyBase k z s)) (l (LexicalETXKeyBase k z s)) (l (LexicalEOTKeyBase k z s)) (l (LexicalENQKeyBase k z s)) (l (LexicalACKKeyBase k z s)) (l (LexicalBELKeyBase k z s)) (l (LexicalBSKeyBase k z s)) (l (LexicalHTKeyBase k z s)) (l (LexicalLFKeyBase k z s)) (l (LexicalVTKeyBase k z s)) (l (LexicalFFKeyBase k z s)) (l (LexicalCRKeyBase k z s)) (l (LexicalSOKeyBase k z s)) (l (LexicalSIKeyBase k z s)) (l (LexicalDLEKeyBase k z s)) (l (LexicalDC1KeyBase k z s)) (l (LexicalDC2KeyBase k z s)) (l (LexicalDC3KeyBase k z s)) (l (LexicalDC4KeyBase k z s)) (l (LexicalNAKKeyBase k z s)) (l (LexicalSYNKeyBase k z s)) (l (LexicalETBKeyBase k z s)) (l (LexicalCANKeyBase k z s)) (l (LexicalEMKeyBase k z s)) (l (LexicalSUBKeyBase k z s)) (l (LexicalESCKeyBase k z s)) (l (LexicalFSKeyBase k z s)) (l (LexicalGSKeyBase k z s)) (l (LexicalRSKeyBase k z s)) (l (LexicalUSKeyBase k z s)) (LexicalSpace k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalDELKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'AsciiF'
type Ascii k z s l lexicalAnnotation annotation = Fixed.Fix (AsciiF k z s l lexicalAnnotation annotation)
-- | 'CntrlBase' with fewer unresolved variables, with default linking.
newtype CntrlF k z s l lexicalAnnotation annotation fixpoint = MkCntrlF { _unCntrlF :: (CntrlBase (AscLarge k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalAtKeyBase k z s)) (l (LexicalLeftBracketKeyBase k z s)) (l (LexicalBackslashKeyBase k z s)) (l (LexicalRightBracketKeyBase k z s)) (l (LexicalCaretKeyBase k z s)) (l (LexicalUnderscoreKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'CntrlF'
type Cntrl k z s l lexicalAnnotation annotation = Fixed.Fix (CntrlF k z s l lexicalAnnotation annotation)
-- | 'GapBase' with fewer unresolved variables, with default linking.
newtype GapF k z s l lexicalAnnotation annotation fixpoint = MkGapF { _unGapF :: (GapBase [] (l (LexicalBackslashKeyBase k z s)) (Whitechar k z s l lexicalAnnotation lexicalAnnotation fixpoint) annotation fixpoint) }
-- | Fixpoint applied to 'GapF'
type Gap k z s l lexicalAnnotation annotation = Fixed.Fix (GapF k z s l lexicalAnnotation annotation)

-- Base lexical structures.

-- Pseudo-foundational lexical structures.

-- | 'LexicalPseudoBase' with fewer unresolved variables, with default linking.
newtype LexicalPseudoF k z s l lexicalAnnotation annotation fixpoint = MkLexicalPseudoF { _unLexicalPseudoF :: (LexicalPseudoBase (LexicalNonsymKeyword k z s l lexicalAnnotation lexicalAnnotation fixpoint) (LexicalNonsymNonkeyword k z s l lexicalAnnotation lexicalAnnotation fixpoint) (LexicalSymAlias k z s l lexicalAnnotation lexicalAnnotation fixpoint) (LexicalAlias k z s l lexicalAnnotation lexicalAnnotation fixpoint) (LexicalNumPrefix k z s l lexicalAnnotation lexicalAnnotation fixpoint) annotation fixpoint) }
-- | Fixpoint applied to 'LexicalPseudoF'
type LexicalPseudo k z s l lexicalAnnotation annotation = Fixed.Fix (LexicalPseudoF k z s l lexicalAnnotation annotation)

-- Non-symbolic keyword pseudo-lexical structures.

-- | 'LexicalNonsymKeywordBase' with fewer unresolved variables, with default linking.
newtype LexicalNonsymKeywordF k z s l lexicalAnnotation annotation fixpoint = MkLexicalNonsymKeywordF { _unLexicalNonsymKeywordF :: (LexicalNonsymKeywordBase (LexicalCase k z s l lexicalAnnotation lexicalAnnotation fixpoint) (LexicalClass k z s l lexicalAnnotation lexicalAnnotation fixpoint) (LexicalData k z s l lexicalAnnotation lexicalAnnotation fixpoint) (LexicalDefault k z s l lexicalAnnotation lexicalAnnotation fixpoint) (LexicalDeriving k z s l lexicalAnnotation lexicalAnnotation fixpoint) (LexicalDo k z s l lexicalAnnotation lexicalAnnotation fixpoint) (LexicalElse k z s l lexicalAnnotation lexicalAnnotation fixpoint) (LexicalForeign k z s l lexicalAnnotation lexicalAnnotation fixpoint) (LexicalIf k z s l lexicalAnnotation lexicalAnnotation fixpoint) (LexicalImport k z s l lexicalAnnotation lexicalAnnotation fixpoint) (LexicalIn k z s l lexicalAnnotation lexicalAnnotation fixpoint) (LexicalInfix k z s l lexicalAnnotation lexicalAnnotation fixpoint) (LexicalInfixl k z s l lexicalAnnotation lexicalAnnotation fixpoint) (LexicalInfixr k z s l lexicalAnnotation lexicalAnnotation fixpoint) (LexicalInstance k z s l lexicalAnnotation lexicalAnnotation fixpoint) (LexicalLet k z s l lexicalAnnotation lexicalAnnotation fixpoint) (LexicalModule k z s l lexicalAnnotation lexicalAnnotation fixpoint) (LexicalNewtype k z s l lexicalAnnotation lexicalAnnotation fixpoint) (LexicalOf k z s l lexicalAnnotation lexicalAnnotation fixpoint) (LexicalThen k z s l lexicalAnnotation lexicalAnnotation fixpoint) (LexicalType k z s l lexicalAnnotation lexicalAnnotation fixpoint) (LexicalWhere k z s l lexicalAnnotation lexicalAnnotation fixpoint) annotation fixpoint) }
-- | Fixpoint applied to 'LexicalNonsymKeywordF'
type LexicalNonsymKeyword k z s l lexicalAnnotation annotation = Fixed.Fix (LexicalNonsymKeywordF k z s l lexicalAnnotation annotation)
-- | 'LexicalCaseBase' with fewer unresolved variables, with default linking.
newtype LexicalCaseF k z s l lexicalAnnotation annotation fixpoint = MkLexicalCaseF { _unLexicalCaseF :: (LexicalCaseBase (l (LexicalCLowerKeyBase k z s)) (l (LexicalALowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'LexicalCaseF'
type LexicalCase k z s l lexicalAnnotation annotation = Fixed.Fix (LexicalCaseF k z s l lexicalAnnotation annotation)
-- | 'LexicalClassBase' with fewer unresolved variables, with default linking.
newtype LexicalClassF k z s l lexicalAnnotation annotation fixpoint = MkLexicalClassF { _unLexicalClassF :: (LexicalClassBase (l (LexicalCLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalALowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'LexicalClassF'
type LexicalClass k z s l lexicalAnnotation annotation = Fixed.Fix (LexicalClassF k z s l lexicalAnnotation annotation)
-- | 'LexicalDataBase' with fewer unresolved variables, with default linking.
newtype LexicalDataF k z s l lexicalAnnotation annotation fixpoint = MkLexicalDataF { _unLexicalDataF :: (LexicalDataBase (l (LexicalDLowerKeyBase k z s)) (l (LexicalALowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'LexicalDataF'
type LexicalData k z s l lexicalAnnotation annotation = Fixed.Fix (LexicalDataF k z s l lexicalAnnotation annotation)
-- | 'LexicalDefaultBase' with fewer unresolved variables, with default linking.
newtype LexicalDefaultF k z s l lexicalAnnotation annotation fixpoint = MkLexicalDefaultF { _unLexicalDefaultF :: (LexicalDefaultBase (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalALowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'LexicalDefaultF'
type LexicalDefault k z s l lexicalAnnotation annotation = Fixed.Fix (LexicalDefaultF k z s l lexicalAnnotation annotation)
-- | 'LexicalDerivingBase' with fewer unresolved variables, with default linking.
newtype LexicalDerivingF k z s l lexicalAnnotation annotation fixpoint = MkLexicalDerivingF { _unLexicalDerivingF :: (LexicalDerivingBase (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'LexicalDerivingF'
type LexicalDeriving k z s l lexicalAnnotation annotation = Fixed.Fix (LexicalDerivingF k z s l lexicalAnnotation annotation)
-- | 'LexicalDoBase' with fewer unresolved variables, with default linking.
newtype LexicalDoF k z s l lexicalAnnotation annotation fixpoint = MkLexicalDoF { _unLexicalDoF :: (LexicalDoBase (l (LexicalDLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'LexicalDoF'
type LexicalDo k z s l lexicalAnnotation annotation = Fixed.Fix (LexicalDoF k z s l lexicalAnnotation annotation)
-- | 'LexicalElseBase' with fewer unresolved variables, with default linking.
newtype LexicalElseF k z s l lexicalAnnotation annotation fixpoint = MkLexicalElseF { _unLexicalElseF :: (LexicalElseBase (l (LexicalELowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'LexicalElseF'
type LexicalElse k z s l lexicalAnnotation annotation = Fixed.Fix (LexicalElseF k z s l lexicalAnnotation annotation)
-- | 'LexicalForeignBase' with fewer unresolved variables, with default linking.
newtype LexicalForeignF k z s l lexicalAnnotation annotation fixpoint = MkLexicalForeignF { _unLexicalForeignF :: (LexicalForeignBase (l (LexicalFLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'LexicalForeignF'
type LexicalForeign k z s l lexicalAnnotation annotation = Fixed.Fix (LexicalForeignF k z s l lexicalAnnotation annotation)
-- | 'LexicalIfBase' with fewer unresolved variables, with default linking.
newtype LexicalIfF k z s l lexicalAnnotation annotation fixpoint = MkLexicalIfF { _unLexicalIfF :: (LexicalIfBase (l (LexicalILowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'LexicalIfF'
type LexicalIf k z s l lexicalAnnotation annotation = Fixed.Fix (LexicalIfF k z s l lexicalAnnotation annotation)
-- | 'LexicalImportBase' with fewer unresolved variables, with default linking.
newtype LexicalImportF k z s l lexicalAnnotation annotation fixpoint = MkLexicalImportF { _unLexicalImportF :: (LexicalImportBase (l (LexicalILowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'LexicalImportF'
type LexicalImport k z s l lexicalAnnotation annotation = Fixed.Fix (LexicalImportF k z s l lexicalAnnotation annotation)
-- | 'LexicalInBase' with fewer unresolved variables, with default linking.
newtype LexicalInF k z s l lexicalAnnotation annotation fixpoint = MkLexicalInF { _unLexicalInF :: (LexicalInBase (l (LexicalILowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'LexicalInF'
type LexicalIn k z s l lexicalAnnotation annotation = Fixed.Fix (LexicalInF k z s l lexicalAnnotation annotation)
-- | 'LexicalInfixBase' with fewer unresolved variables, with default linking.
newtype LexicalInfixF k z s l lexicalAnnotation annotation fixpoint = MkLexicalInfixF { _unLexicalInfixF :: (LexicalInfixBase (l (LexicalILowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'LexicalInfixF'
type LexicalInfix k z s l lexicalAnnotation annotation = Fixed.Fix (LexicalInfixF k z s l lexicalAnnotation annotation)
-- | 'LexicalInfixlBase' with fewer unresolved variables, with default linking.
newtype LexicalInfixlF k z s l lexicalAnnotation annotation fixpoint = MkLexicalInfixlF { _unLexicalInfixlF :: (LexicalInfixlBase (l (LexicalILowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'LexicalInfixlF'
type LexicalInfixl k z s l lexicalAnnotation annotation = Fixed.Fix (LexicalInfixlF k z s l lexicalAnnotation annotation)
-- | 'LexicalInfixrBase' with fewer unresolved variables, with default linking.
newtype LexicalInfixrF k z s l lexicalAnnotation annotation fixpoint = MkLexicalInfixrF { _unLexicalInfixrF :: (LexicalInfixrBase (l (LexicalILowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'LexicalInfixrF'
type LexicalInfixr k z s l lexicalAnnotation annotation = Fixed.Fix (LexicalInfixrF k z s l lexicalAnnotation annotation)
-- | 'LexicalInstanceBase' with fewer unresolved variables, with default linking.
newtype LexicalInstanceF k z s l lexicalAnnotation annotation fixpoint = MkLexicalInstanceF { _unLexicalInstanceF :: (LexicalInstanceBase (l (LexicalILowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalALowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'LexicalInstanceF'
type LexicalInstance k z s l lexicalAnnotation annotation = Fixed.Fix (LexicalInstanceF k z s l lexicalAnnotation annotation)
-- | 'LexicalLetBase' with fewer unresolved variables, with default linking.
newtype LexicalLetF k z s l lexicalAnnotation annotation fixpoint = MkLexicalLetF { _unLexicalLetF :: (LexicalLetBase (l (LexicalLLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'LexicalLetF'
type LexicalLet k z s l lexicalAnnotation annotation = Fixed.Fix (LexicalLetF k z s l lexicalAnnotation annotation)
-- | 'LexicalModuleBase' with fewer unresolved variables, with default linking.
newtype LexicalModuleF k z s l lexicalAnnotation annotation fixpoint = MkLexicalModuleF { _unLexicalModuleF :: (LexicalModuleBase (l (LexicalMLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'LexicalModuleF'
type LexicalModule k z s l lexicalAnnotation annotation = Fixed.Fix (LexicalModuleF k z s l lexicalAnnotation annotation)
-- | 'LexicalNewtypeBase' with fewer unresolved variables, with default linking.
newtype LexicalNewtypeF k z s l lexicalAnnotation annotation fixpoint = MkLexicalNewtypeF { _unLexicalNewtypeF :: (LexicalNewtypeBase (l (LexicalNLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'LexicalNewtypeF'
type LexicalNewtype k z s l lexicalAnnotation annotation = Fixed.Fix (LexicalNewtypeF k z s l lexicalAnnotation annotation)
-- | 'LexicalOfBase' with fewer unresolved variables, with default linking.
newtype LexicalOfF k z s l lexicalAnnotation annotation fixpoint = MkLexicalOfF { _unLexicalOfF :: (LexicalOfBase (l (LexicalOLowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'LexicalOfF'
type LexicalOf k z s l lexicalAnnotation annotation = Fixed.Fix (LexicalOfF k z s l lexicalAnnotation annotation)
-- | 'LexicalThenBase' with fewer unresolved variables, with default linking.
newtype LexicalThenF k z s l lexicalAnnotation annotation fixpoint = MkLexicalThenF { _unLexicalThenF :: (LexicalThenBase (l (LexicalTLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'LexicalThenF'
type LexicalThen k z s l lexicalAnnotation annotation = Fixed.Fix (LexicalThenF k z s l lexicalAnnotation annotation)
-- | 'LexicalTypeBase' with fewer unresolved variables, with default linking.
newtype LexicalTypeF k z s l lexicalAnnotation annotation fixpoint = MkLexicalTypeF { _unLexicalTypeF :: (LexicalTypeBase (l (LexicalTLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'LexicalTypeF'
type LexicalType k z s l lexicalAnnotation annotation = Fixed.Fix (LexicalTypeF k z s l lexicalAnnotation annotation)
-- | 'LexicalWhereBase' with fewer unresolved variables, with default linking.
newtype LexicalWhereF k z s l lexicalAnnotation annotation fixpoint = MkLexicalWhereF { _unLexicalWhereF :: (LexicalWhereBase (l (LexicalWLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'LexicalWhereF'
type LexicalWhere k z s l lexicalAnnotation annotation = Fixed.Fix (LexicalWhereF k z s l lexicalAnnotation annotation)

-- Non-symbolic non-keyword pseudo-lexical structures.

-- | 'LexicalNonsymNonkeywordBase' with fewer unresolved variables, with default linking.
newtype LexicalNonsymNonkeywordF k z s l lexicalAnnotation annotation fixpoint = MkLexicalNonsymNonkeywordF { _unLexicalNonsymNonkeywordF :: (LexicalNonsymNonkeywordBase (LexicalAs k z s l lexicalAnnotation lexicalAnnotation fixpoint) (LexicalHiding k z s l lexicalAnnotation lexicalAnnotation fixpoint) (LexicalQualified k z s l lexicalAnnotation lexicalAnnotation fixpoint) annotation fixpoint) }
-- | Fixpoint applied to 'LexicalNonsymNonkeywordF'
type LexicalNonsymNonkeyword k z s l lexicalAnnotation annotation = Fixed.Fix (LexicalNonsymNonkeywordF k z s l lexicalAnnotation annotation)
-- | 'LexicalAsBase' with fewer unresolved variables, with default linking.
newtype LexicalAsF k z s l lexicalAnnotation annotation fixpoint = MkLexicalAsF { _unLexicalAsF :: (LexicalAsBase (l (LexicalALowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'LexicalAsF'
type LexicalAs k z s l lexicalAnnotation annotation = Fixed.Fix (LexicalAsF k z s l lexicalAnnotation annotation)
-- | 'LexicalHidingBase' with fewer unresolved variables, with default linking.
newtype LexicalHidingF k z s l lexicalAnnotation annotation fixpoint = MkLexicalHidingF { _unLexicalHidingF :: (LexicalHidingBase (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'LexicalHidingF'
type LexicalHiding k z s l lexicalAnnotation annotation = Fixed.Fix (LexicalHidingF k z s l lexicalAnnotation annotation)
-- | 'LexicalQualifiedBase' with fewer unresolved variables, with default linking.
newtype LexicalQualifiedF k z s l lexicalAnnotation annotation fixpoint = MkLexicalQualifiedF { _unLexicalQualifiedF :: (LexicalQualifiedBase (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'LexicalQualifiedF'
type LexicalQualified k z s l lexicalAnnotation annotation = Fixed.Fix (LexicalQualifiedF k z s l lexicalAnnotation annotation)

-- Symbolic alias pseudo-lexical structures.

-- | 'LexicalSymAliasBase' with fewer unresolved variables, with default linking.
newtype LexicalSymAliasF k z s l lexicalAnnotation annotation fixpoint = MkLexicalSymAliasF { _unLexicalSymAliasF :: (LexicalSymAliasBase (LexicalDotDot k z s l lexicalAnnotation lexicalAnnotation) (LexicalDoubleColon k z s l lexicalAnnotation lexicalAnnotation) (LexicalDoubleRightArrow k z s l lexicalAnnotation lexicalAnnotation) (LexicalLeftArrow k z s l lexicalAnnotation lexicalAnnotation) (LexicalRightArrow k z s l lexicalAnnotation lexicalAnnotation) annotation fixpoint) }
-- | Fixpoint applied to 'LexicalSymAliasF'.
type LexicalSymAlias k z s l lexicalAnnotation annotation = Fixed.Fix (LexicalSymAliasF k z s l lexicalAnnotation annotation)

-- | 'LexicalDotDotArrowBase' with fewer unresolved variables, with default linking.
newtype LexicalDotDotF k z s l lexicalAnnotation annotation fixpoint = MkLexicalDotDotF { _unLexicalDotDotF :: (LexicalDotDotBase (l (LexicalDotKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'LexicalDotDotArrowF'.
type LexicalDotDot k z s l lexicalAnnotation annotation = Fixed.Fix (LexicalDotDotF k z s l lexicalAnnotation annotation)

-- | 'LexicalDoubleColonArrowBase' with fewer unresolved variables, with default linking.
newtype LexicalDoubleColonF k z s l lexicalAnnotation annotation fixpoint = MkLexicalDoubleColonF { _unLexicalDoubleColonF :: (LexicalDoubleColonBase (l (LexicalColonKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'LexicalDoubleColonArrowF'.
type LexicalDoubleColon k z s l lexicalAnnotation annotation = Fixed.Fix (LexicalDoubleColonF k z s l lexicalAnnotation annotation)

-- | 'LexicalDoubleRightArrowBase' with fewer unresolved variables, with default linking.
newtype LexicalDoubleRightArrowF k z s l lexicalAnnotation annotation fixpoint = MkLexicalDoubleRightArrowF { _unLexicalDoubleRightArrowF :: (LexicalDoubleRightArrowBase (l (LexicalEqualsKeyBase k z s)) (l (LexicalRightAngleBracketKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'LexicalDoubleRightArrowF'.
type LexicalDoubleRightArrow k z s l lexicalAnnotation annotation = Fixed.Fix (LexicalDoubleRightArrowF k z s l lexicalAnnotation annotation)

-- | 'LexicalLeftArrowBase' with fewer unresolved variables, with default linking.
newtype LexicalLeftArrowF k z s l lexicalAnnotation annotation fixpoint = MkLexicalLeftArrowF { _unLexicalLeftArrowF :: (LexicalLeftArrowBase (l (LexicalLeftAngleBracketKeyBase k z s)) (l (LexicalHyphenKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'LexicalLeftArrowF'.
type LexicalLeftArrow k z s l lexicalAnnotation annotation = Fixed.Fix (LexicalLeftArrowF k z s l lexicalAnnotation annotation)

-- | 'LexicalRightArrowBase' with fewer unresolved variables, with default linking.
newtype LexicalRightArrowF k z s l lexicalAnnotation annotation fixpoint = MkLexicalRightArrowF { _unLexicalRightArrowF :: (LexicalRightArrowBase (l (LexicalHyphenKeyBase k z s)) (l (LexicalRightAngleBracketKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'LexicalRightArrowF'.
type LexicalRightArrow k z s l lexicalAnnotation annotation = Fixed.Fix (LexicalRightArrowF k z s l lexicalAnnotation annotation)

-- Alias pseudo-lexical structures.

-- | 'LexicalAliasBase' with fewer unresolved variables, with default linking.
newtype LexicalAliasF k z s l lexicalAnnotation annotation fixpoint = MkLexicalAliasF { _unLexicalAliasF :: (LexicalAliasBase (LexicalSpace k z s l lexicalAnnotation) (LexicalMinus k z s l lexicalAnnotation) (LexicalAsciiLambda k z s l lexicalAnnotation) annotation fixpoint) }
-- | Fixpoint applied to 'LexicalAliasF'.
type LexicalAlias k z s l lexicalAnnotation annotation = Fixed.Fix (LexicalAliasF k z s l lexicalAnnotation annotation)

-- | 'LexicalSpaceBase' with fewer unresolved variables, with default linking.
newtype LexicalSpaceF k z s l annotation fixpoint = MkLexicalSpaceF { _unLexicalSpaceF :: (LexicalSpaceBase (l (LexicalSPKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'LexicalSpaceF'.
type LexicalSpace k z s l annotation = Fixed.Fix (LexicalSpaceF k z s l annotation)

-- | 'LexicalMinusBase' with fewer unresolved variables, with default linking.
newtype LexicalMinusF k z s l annotation fixpoint = MkLexicalMinusF { _unLexicalMinusF :: (LexicalMinusBase (l (LexicalHyphenKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'LexicalMinusF'.
type LexicalMinus k z s l annotation = Fixed.Fix (LexicalMinusF k z s l annotation)

-- | 'LexicalAsciiLambdaBase' with fewer unresolved variables, with default linking.
newtype LexicalAsciiLambdaF k z s l annotation fixpoint = MkLexicalAsciiLambdaF { _unLexicalAsciiLambdaF :: (LexicalAsciiLambdaBase (l (LexicalBackslashKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'LexicalAsciiLambdaF'.
type LexicalAsciiLambda k z s l annotation = Fixed.Fix (LexicalAsciiLambdaF k z s l annotation)

-- Non-symbolic numeric literal prefix pseudo-lexical structures.

-- | 'LexicalNumPrefixBase' with fewer unresolved variables, with default linking.
newtype LexicalNumPrefixF k z s l lexicalAnnotation annotation fixpoint = MkLexicalNumPrefixF { _unLexicalNumPrefixF :: (LexicalNumPrefixBase (Lexical0o k z s l lexicalAnnotation) (Lexical0O k z s l lexicalAnnotation) (Lexical0x k z s l lexicalAnnotation) (Lexical0X k z s l lexicalAnnotation) annotation fixpoint) }
-- | Fixpoint applied to 'LexicalNumPrefixF'.
type LexicalNumPrefix k z s l lexicalAnnotation annotation = Fixed.Fix (LexicalNumPrefixF k z s l lexicalAnnotation annotation)

-- | 'Lexical0oBase' with fewer unresolved variables, with default linking.
newtype Lexical0oF k z s l annotation fixpoint = MkLexical0oF { _unLexical0oF :: (Lexical0oBase (l (Lexical0KeyBase k z s)) (l (LexicalXKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'Lexical0oF'.
type Lexical0o k z s l annotation = Fixed.Fix (Lexical0oF k z s l annotation)

-- | 'Lexical0OBase' with fewer unresolved variables, with default linking.
newtype Lexical0OF k z s l annotation fixpoint = MkLexical0OF { _unLexical0OF :: (Lexical0OBase (l (Lexical0KeyBase k z s)) (l (LexicalXKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'Lexical0OF'.
type Lexical0O k z s l annotation = Fixed.Fix (Lexical0OF k z s l annotation)

-- | 'Lexical0xBase' with fewer unresolved variables, with default linking.
newtype Lexical0xF k z s l annotation fixpoint = MkLexical0xF { _unLexical0xF :: (Lexical0xBase (l (Lexical0KeyBase k z s)) (l (LexicalXKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'Lexical0xF'.
type Lexical0x k z s l annotation = Fixed.Fix (Lexical0xF k z s l annotation)

-- | 'Lexical0XBase' with fewer unresolved variables, with default linking.
newtype Lexical0XF k z s l annotation fixpoint = MkLexical0XF { _unLexical0XF :: (Lexical0XBase (l (Lexical0KeyBase k z s)) (l (LexicalXKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'Lexical0XF'.
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

-- | 'VaridInnerSansAscSmallUnderscoreBase' with fewer unresolved variables, with default linking.
newtype VaridInnerSansAscSmallUnderscoreF k z s l lexicalAnnotation annotation fixpoint = MkVaridInnerSansAscSmallUnderscoreF { _unVaridInnerSansAscSmallUnderscoreF :: (VaridInnerSansAscSmallUnderscoreBase (SmallSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation fixpoint) (Large k z s l lexicalAnnotation lexicalAnnotation fixpoint) (Digit k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalSingleQuoteKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'VaridInnerSansAscSmallUnderscoreF'
type VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation annotation = Fixed.Fix (VaridInnerSansAscSmallUnderscoreF k z s l lexicalAnnotation annotation)
-- | 'SmallSansAscSmallUnderscoreBase' with fewer unresolved variables, with default linking.
newtype SmallSansAscSmallUnderscoreF k z s l lexicalAnnotation annotation fixpoint = MkSmallSansAscSmallUnderscoreF { _unSmallSansAscSmallUnderscoreF :: (SmallSansAscSmallUnderscoreBase (AscSmall k z s l lexicalAnnotation lexicalAnnotation fixpoint) (UniSmallSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalUnderscoreKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'SmallSansAscSmallUnderscoreF'
type SmallSansAscSmallUnderscore k z s l lexicalAnnotation annotation = Fixed.Fix (SmallSansAscSmallUnderscoreF k z s l lexicalAnnotation annotation)
-- | 'UniSmallSansAscBase' with fewer unresolved variables, with default linking.
newtype UniSmallSansAscF k z s l lexicalAnnotation annotation fixpoint = MkUniSmallSansAscF { _unUniSmallSansAscF :: (UniSmallSansAscBase (l (LexicalUnicodeSmallSansAscUnderscoreKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'UniSmallSansAscF'
type UniSmallSansAsc k z s l lexicalAnnotation annotation = Fixed.Fix (UniSmallSansAscF k z s l lexicalAnnotation annotation)
-- | 'VaridBase' with fewer unresolved variables, with default linking.
newtype VaridF k z s l lexicalAnnotation annotation fixpoint = MkVaridF { _unVaridF :: (VaridBase [] (SmallSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation fixpoint) (VaridInner k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (VaridC k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalDLowerKeyBase k z s)) (VaridD k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalELowerKeyBase k z s)) (VaridE k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalFLowerKeyBase k z s)) (VaridF_ k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (VaridI k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (VaridL k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalMLowerKeyBase k z s)) (VaridM k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalNLowerKeyBase k z s)) (VaridN k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalOLowerKeyBase k z s)) (VaridO k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (VaridT k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (VaridW k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'VaridF'
type Varid k z s l lexicalAnnotation annotation = Fixed.Fix (VaridF k z s l lexicalAnnotation annotation)
-- | 'VaridCBase' with fewer unresolved variables, with default linking.
newtype VaridCF k z s l lexicalAnnotation annotation fixpoint = MkVaridCF { _unVaridCF :: (VaridCBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation fixpoint) (VaridInner k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalALowerKeyBase k z s)) (VaridCa k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (VaridCl k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'VaridCF'
type VaridC k z s l lexicalAnnotation annotation = Fixed.Fix (VaridCF k z s l lexicalAnnotation annotation)
-- | 'VaridDBase' with fewer unresolved variables, with default linking.
newtype VaridDF k z s l lexicalAnnotation annotation fixpoint = MkVaridDF { _unVaridDF :: (VaridDBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation fixpoint) (VaridInner k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalALowerKeyBase k z s)) (VaridDa k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (VaridDe k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'VaridDF'
type VaridD k z s l lexicalAnnotation annotation = Fixed.Fix (VaridDF k z s l lexicalAnnotation annotation)
-- | 'VaridEBase' with fewer unresolved variables, with default linking.
newtype VaridEF k z s l lexicalAnnotation annotation fixpoint = MkVaridEF { _unVaridEF :: (VaridEBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation fixpoint) (VaridInner k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (VaridEl k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'VaridEF'
type VaridE k z s l lexicalAnnotation annotation = Fixed.Fix (VaridEF k z s l lexicalAnnotation annotation)
-- | 'VaridFBase' with fewer unresolved variables, with default linking.
newtype VaridFF k z s l lexicalAnnotation annotation fixpoint = MkVaridFF { _unVaridFF :: (VaridFBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation fixpoint) (VaridInner k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (VaridFo k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'VaridFF'
type VaridF_ k z s l lexicalAnnotation annotation = Fixed.Fix (VaridFF k z s l lexicalAnnotation annotation)
-- | 'VaridIBase' with fewer unresolved variables, with default linking.
newtype VaridIF k z s l lexicalAnnotation annotation fixpoint = MkVaridIF { _unVaridIF :: (VaridIBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation fixpoint) (VaridInner k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (VaridIm k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalNLowerKeyBase k z s)) (VaridIn k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'VaridIF'
type VaridI k z s l lexicalAnnotation annotation = Fixed.Fix (VaridIF k z s l lexicalAnnotation annotation)
-- | 'VaridLBase' with fewer unresolved variables, with default linking.
newtype VaridLF k z s l lexicalAnnotation annotation fixpoint = MkVaridLF { _unVaridLF :: (VaridLBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation fixpoint) (VaridInner k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (VaridLe k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'VaridLF'
type VaridL k z s l lexicalAnnotation annotation = Fixed.Fix (VaridLF k z s l lexicalAnnotation annotation)
-- | 'VaridMBase' with fewer unresolved variables, with default linking.
newtype VaridMF k z s l lexicalAnnotation annotation fixpoint = MkVaridMF { _unVaridMF :: (VaridMBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation fixpoint) (VaridInner k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (VaridMo k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'VaridMF'
type VaridM k z s l lexicalAnnotation annotation = Fixed.Fix (VaridMF k z s l lexicalAnnotation annotation)
-- | 'VaridNBase' with fewer unresolved variables, with default linking.
newtype VaridNF k z s l lexicalAnnotation annotation fixpoint = MkVaridNF { _unVaridNF :: (VaridNBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation fixpoint) (VaridInner k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (VaridNe k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'VaridNF'
type VaridN k z s l lexicalAnnotation annotation = Fixed.Fix (VaridNF k z s l lexicalAnnotation annotation)
-- | 'VaridOBase' with fewer unresolved variables, with default linking.
newtype VaridOF k z s l lexicalAnnotation annotation fixpoint = MkVaridOF { _unVaridOF :: (VaridOBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation fixpoint) (VaridInner k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'VaridOF'
type VaridO k z s l lexicalAnnotation annotation = Fixed.Fix (VaridOF k z s l lexicalAnnotation annotation)
-- | 'VaridTBase' with fewer unresolved variables, with default linking.
newtype VaridTF k z s l lexicalAnnotation annotation fixpoint = MkVaridTF { _unVaridTF :: (VaridTBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation fixpoint) (VaridInner k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (VaridTh k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (VaridTy k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalZLowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'VaridTF'
type VaridT k z s l lexicalAnnotation annotation = Fixed.Fix (VaridTF k z s l lexicalAnnotation annotation)
-- | 'VaridWBase' with fewer unresolved variables, with default linking.
newtype VaridWF k z s l lexicalAnnotation annotation fixpoint = MkVaridWF { _unVaridWF :: (VaridWBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation fixpoint) (VaridInner k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (VaridWh k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'VaridWF'
type VaridW k z s l lexicalAnnotation annotation = Fixed.Fix (VaridWF k z s l lexicalAnnotation annotation)
-- | 'VaridCaBase' with fewer unresolved variables, with default linking.
newtype VaridCaF k z s l lexicalAnnotation annotation fixpoint = MkVaridCaF { _unVaridCaF :: (VaridCaBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation fixpoint) (VaridInner k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (VaridCas k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'VaridCaF'
type VaridCa k z s l lexicalAnnotation annotation = Fixed.Fix (VaridCaF k z s l lexicalAnnotation annotation)
-- | 'VaridClBase' with fewer unresolved variables, with default linking.
newtype VaridClF k z s l lexicalAnnotation annotation fixpoint = MkVaridClF { _unVaridClF :: (VaridClBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation fixpoint) (VaridInner k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalALowerKeyBase k z s)) (VaridCla k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'VaridClF'
type VaridCl k z s l lexicalAnnotation annotation = Fixed.Fix (VaridClF k z s l lexicalAnnotation annotation)
-- | 'VaridDaBase' with fewer unresolved variables, with default linking.
newtype VaridDaF k z s l lexicalAnnotation annotation fixpoint = MkVaridDaF { _unVaridDaF :: (VaridDaBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation fixpoint) (VaridInner k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (VaridDat k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'VaridDaF'
type VaridDa k z s l lexicalAnnotation annotation = Fixed.Fix (VaridDaF k z s l lexicalAnnotation annotation)
-- | 'VaridDeBase' with fewer unresolved variables, with default linking.
newtype VaridDeF k z s l lexicalAnnotation annotation fixpoint = MkVaridDeF { _unVaridDeF :: (VaridDeBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation fixpoint) (VaridInner k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (VaridDef k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (VaridDer k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'VaridDeF'
type VaridDe k z s l lexicalAnnotation annotation = Fixed.Fix (VaridDeF k z s l lexicalAnnotation annotation)
-- | 'VaridElBase' with fewer unresolved variables, with default linking.
newtype VaridElF k z s l lexicalAnnotation annotation fixpoint = MkVaridElF { _unVaridElF :: (VaridElBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation fixpoint) (VaridInner k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (VaridEls k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'VaridElF'
type VaridEl k z s l lexicalAnnotation annotation = Fixed.Fix (VaridElF k z s l lexicalAnnotation annotation)
-- | 'VaridFoBase' with fewer unresolved variables, with default linking.
newtype VaridFoF k z s l lexicalAnnotation annotation fixpoint = MkVaridFoF { _unVaridFoF :: (VaridFoBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation fixpoint) (VaridInner k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (VaridFor k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'VaridFoF'
type VaridFo k z s l lexicalAnnotation annotation = Fixed.Fix (VaridFoF k z s l lexicalAnnotation annotation)
-- | 'VaridImBase' with fewer unresolved variables, with default linking.
newtype VaridImF k z s l lexicalAnnotation annotation fixpoint = MkVaridImF { _unVaridImF :: (VaridImBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation fixpoint) (VaridInner k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (VaridImp k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'VaridImF'
type VaridIm k z s l lexicalAnnotation annotation = Fixed.Fix (VaridImF k z s l lexicalAnnotation annotation)
-- | 'VaridInBase' with fewer unresolved variables, with default linking.
newtype VaridInF k z s l lexicalAnnotation annotation fixpoint = MkVaridInF { _unVaridInF :: (VaridInBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation fixpoint) (VaridInner k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (VaridInf k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (VaridIns k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'VaridInF'
type VaridIn k z s l lexicalAnnotation annotation = Fixed.Fix (VaridInF k z s l lexicalAnnotation annotation)
-- | 'VaridLeBase' with fewer unresolved variables, with default linking.
newtype VaridLeF k z s l lexicalAnnotation annotation fixpoint = MkVaridLeF { _unVaridLeF :: (VaridLeBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation fixpoint) (VaridInner k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'VaridLeF'
type VaridLe k z s l lexicalAnnotation annotation = Fixed.Fix (VaridLeF k z s l lexicalAnnotation annotation)
-- | 'VaridMoBase' with fewer unresolved variables, with default linking.
newtype VaridMoF k z s l lexicalAnnotation annotation fixpoint = MkVaridMoF { _unVaridMoF :: (VaridMoBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation fixpoint) (VaridInner k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (VaridMod k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'VaridMoF'
type VaridMo k z s l lexicalAnnotation annotation = Fixed.Fix (VaridMoF k z s l lexicalAnnotation annotation)
-- | 'VaridNeBase' with fewer unresolved variables, with default linking.
newtype VaridNeF k z s l lexicalAnnotation annotation fixpoint = MkVaridNeF { _unVaridNeF :: (VaridNeBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation fixpoint) (VaridInner k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (VaridNew k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'VaridNeF'
type VaridNe k z s l lexicalAnnotation annotation = Fixed.Fix (VaridNeF k z s l lexicalAnnotation annotation)
-- | 'VaridThBase' with fewer unresolved variables, with default linking.
newtype VaridThF k z s l lexicalAnnotation annotation fixpoint = MkVaridThF { _unVaridThF :: (VaridThBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation fixpoint) (VaridInner k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (VaridThe k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'VaridThF'
type VaridTh k z s l lexicalAnnotation annotation = Fixed.Fix (VaridThF k z s l lexicalAnnotation annotation)
-- | 'VaridTyBase' with fewer unresolved variables, with default linking.
newtype VaridTyF k z s l lexicalAnnotation annotation fixpoint = MkVaridTyF { _unVaridTyF :: (VaridTyBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation fixpoint) (VaridInner k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (VaridTyp k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalZLowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'VaridTyF'
type VaridTy k z s l lexicalAnnotation annotation = Fixed.Fix (VaridTyF k z s l lexicalAnnotation annotation)
-- | 'VaridCasBase' with fewer unresolved variables, with default linking.
newtype VaridCasF k z s l lexicalAnnotation annotation fixpoint = MkVaridCasF { _unVaridCasF :: (VaridCasBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation fixpoint) (VaridInner k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'VaridCasF'
type VaridCas k z s l lexicalAnnotation annotation = Fixed.Fix (VaridCasF k z s l lexicalAnnotation annotation)
-- | 'VaridClaBase' with fewer unresolved variables, with default linking.
newtype VaridClaF k z s l lexicalAnnotation annotation fixpoint = MkVaridClaF { _unVaridClaF :: (VaridClaBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation fixpoint) (VaridInner k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (VaridClas k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'VaridClaF'
type VaridCla k z s l lexicalAnnotation annotation = Fixed.Fix (VaridClaF k z s l lexicalAnnotation annotation)
-- | 'VaridDatBase' with fewer unresolved variables, with default linking.
newtype VaridDatF k z s l lexicalAnnotation annotation fixpoint = MkVaridDatF { _unVaridDatF :: (VaridDatBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation fixpoint) (VaridInner k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'VaridDatF'
type VaridDat k z s l lexicalAnnotation annotation = Fixed.Fix (VaridDatF k z s l lexicalAnnotation annotation)
-- | 'VaridDefBase' with fewer unresolved variables, with default linking.
newtype VaridDefF k z s l lexicalAnnotation annotation fixpoint = MkVaridDefF { _unVaridDefF :: (VaridDefBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation fixpoint) (VaridInner k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalALowerKeyBase k z s)) (VaridDefa k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'VaridDefF'
type VaridDef k z s l lexicalAnnotation annotation = Fixed.Fix (VaridDefF k z s l lexicalAnnotation annotation)
-- | 'VaridDerBase' with fewer unresolved variables, with default linking.
newtype VaridDerF k z s l lexicalAnnotation annotation fixpoint = MkVaridDerF { _unVaridDerF :: (VaridDerBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation fixpoint) (VaridInner k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (VaridDeri k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'VaridDerF'
type VaridDer k z s l lexicalAnnotation annotation = Fixed.Fix (VaridDerF k z s l lexicalAnnotation annotation)
-- | 'VaridElsBase' with fewer unresolved variables, with default linking.
newtype VaridElsF k z s l lexicalAnnotation annotation fixpoint = MkVaridElsF { _unVaridElsF :: (VaridElsBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation fixpoint) (VaridInner k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'VaridElsF'
type VaridEls k z s l lexicalAnnotation annotation = Fixed.Fix (VaridElsF k z s l lexicalAnnotation annotation)
-- | 'VaridForBase' with fewer unresolved variables, with default linking.
newtype VaridForF k z s l lexicalAnnotation annotation fixpoint = MkVaridForF { _unVaridForF :: (VaridForBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation fixpoint) (VaridInner k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (VaridFore k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'VaridForF'
type VaridFor k z s l lexicalAnnotation annotation = Fixed.Fix (VaridForF k z s l lexicalAnnotation annotation)
-- | 'VaridImpBase' with fewer unresolved variables, with default linking.
newtype VaridImpF k z s l lexicalAnnotation annotation fixpoint = MkVaridImpF { _unVaridImpF :: (VaridImpBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation fixpoint) (VaridInner k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (VaridImpo k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'VaridImpF'
type VaridImp k z s l lexicalAnnotation annotation = Fixed.Fix (VaridImpF k z s l lexicalAnnotation annotation)
-- | 'VaridInfBase' with fewer unresolved variables, with default linking.
newtype VaridInfF k z s l lexicalAnnotation annotation fixpoint = MkVaridInfF { _unVaridInfF :: (VaridInfBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation fixpoint) (VaridInner k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (VaridInfi k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'VaridInfF'
type VaridInf k z s l lexicalAnnotation annotation = Fixed.Fix (VaridInfF k z s l lexicalAnnotation annotation)
-- | 'VaridInsBase' with fewer unresolved variables, with default linking.
newtype VaridInsF k z s l lexicalAnnotation annotation fixpoint = MkVaridInsF { _unVaridInsF :: (VaridInsBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation fixpoint) (VaridInner k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (VaridInst k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'VaridInsF'
type VaridIns k z s l lexicalAnnotation annotation = Fixed.Fix (VaridInsF k z s l lexicalAnnotation annotation)
-- | 'VaridModBase' with fewer unresolved variables, with default linking.
newtype VaridModF k z s l lexicalAnnotation annotation fixpoint = MkVaridModF { _unVaridModF :: (VaridModBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation fixpoint) (VaridInner k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (VaridModu k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'VaridModF'
type VaridMod k z s l lexicalAnnotation annotation = Fixed.Fix (VaridModF k z s l lexicalAnnotation annotation)
-- | 'VaridNewBase' with fewer unresolved variables, with default linking.
newtype VaridNewF k z s l lexicalAnnotation annotation fixpoint = MkVaridNewF { _unVaridNewF :: (VaridNewBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation fixpoint) (VaridInner k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (VaridNewt k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'VaridNewF'
type VaridNew k z s l lexicalAnnotation annotation = Fixed.Fix (VaridNewF k z s l lexicalAnnotation annotation)
-- | 'VaridTheBase' with fewer unresolved variables, with default linking.
newtype VaridTheF k z s l lexicalAnnotation annotation fixpoint = MkVaridTheF { _unVaridTheF :: (VaridTheBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation fixpoint) (VaridInner k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'VaridTheF'
type VaridThe k z s l lexicalAnnotation annotation = Fixed.Fix (VaridTheF k z s l lexicalAnnotation annotation)
-- | 'VaridTypBase' with fewer unresolved variables, with default linking.
newtype VaridTypF k z s l lexicalAnnotation annotation fixpoint = MkVaridTypF { _unVaridTypF :: (VaridTypBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation fixpoint) (VaridInner k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'VaridTypF'
type VaridTyp k z s l lexicalAnnotation annotation = Fixed.Fix (VaridTypF k z s l lexicalAnnotation annotation)
-- | 'VaridWheBase' with fewer unresolved variables, with default linking.
newtype VaridWheF k z s l lexicalAnnotation annotation fixpoint = MkVaridWheF { _unVaridWheF :: (VaridWheBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation fixpoint) (VaridInner k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (VaridWher k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'VaridWheF'
type VaridWhe k z s l lexicalAnnotation annotation = Fixed.Fix (VaridWheF k z s l lexicalAnnotation annotation)
-- | 'VaridClasBase' with fewer unresolved variables, with default linking.
newtype VaridClasF k z s l lexicalAnnotation annotation fixpoint = MkVaridClasF { _unVaridClasF :: (VaridClasBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation fixpoint) (VaridInner k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'VaridClasF'
type VaridClas k z s l lexicalAnnotation annotation = Fixed.Fix (VaridClasF k z s l lexicalAnnotation annotation)
-- | 'VaridDefaBase' with fewer unresolved variables, with default linking.
newtype VaridDefaF k z s l lexicalAnnotation annotation fixpoint = MkVaridDefaF { _unVaridDefaF :: (VaridDefaBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation fixpoint) (VaridInner k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (VaridDefau k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'VaridDefaF'
type VaridDefa k z s l lexicalAnnotation annotation = Fixed.Fix (VaridDefaF k z s l lexicalAnnotation annotation)
-- | 'VaridDeriBase' with fewer unresolved variables, with default linking.
newtype VaridDeriF k z s l lexicalAnnotation annotation fixpoint = MkVaridDeriF { _unVaridDeriF :: (VaridDeriBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation fixpoint) (VaridInner k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (VaridDeriv k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'VaridDeriF'
type VaridDeri k z s l lexicalAnnotation annotation = Fixed.Fix (VaridDeriF k z s l lexicalAnnotation annotation)
-- | 'VaridForeBase' with fewer unresolved variables, with default linking.
newtype VaridForeF k z s l lexicalAnnotation annotation fixpoint = MkVaridForeF { _unVaridForeF :: (VaridForeBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation fixpoint) (VaridInner k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (VaridForei k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'VaridForeF'
type VaridFore k z s l lexicalAnnotation annotation = Fixed.Fix (VaridForeF k z s l lexicalAnnotation annotation)
-- | 'VaridImpoBase' with fewer unresolved variables, with default linking.
newtype VaridImpoF k z s l lexicalAnnotation annotation fixpoint = MkVaridImpoF { _unVaridImpoF :: (VaridImpoBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation fixpoint) (VaridInner k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (VaridImpor k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'VaridImpoF'
type VaridImpo k z s l lexicalAnnotation annotation = Fixed.Fix (VaridImpoF k z s l lexicalAnnotation annotation)
-- | 'VaridInfiBase' with fewer unresolved variables, with default linking.
newtype VaridInfiF k z s l lexicalAnnotation annotation fixpoint = MkVaridInfiF { _unVaridInfiF :: (VaridInfiBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation fixpoint) (VaridInner k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (VaridInfix k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'VaridInfiF'
type VaridInfi k z s l lexicalAnnotation annotation = Fixed.Fix (VaridInfiF k z s l lexicalAnnotation annotation)
-- | 'VaridInstBase' with fewer unresolved variables, with default linking.
newtype VaridInstF k z s l lexicalAnnotation annotation fixpoint = MkVaridInstF { _unVaridInstF :: (VaridInstBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation fixpoint) (VaridInner k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalALowerKeyBase k z s)) (VaridInsta k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'VaridInstF'
type VaridInst k z s l lexicalAnnotation annotation = Fixed.Fix (VaridInstF k z s l lexicalAnnotation annotation)
-- | 'VaridModuBase' with fewer unresolved variables, with default linking.
newtype VaridModuF k z s l lexicalAnnotation annotation fixpoint = MkVaridModuF { _unVaridModuF :: (VaridModuBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation fixpoint) (VaridInner k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (VaridModul k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'VaridModuF'
type VaridModu k z s l lexicalAnnotation annotation = Fixed.Fix (VaridModuF k z s l lexicalAnnotation annotation)
-- | 'VaridNewtBase' with fewer unresolved variables, with default linking.
newtype VaridNewtF k z s l lexicalAnnotation annotation fixpoint = MkVaridNewtF { _unVaridNewtF :: (VaridNewtBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation fixpoint) (VaridInner k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (VaridNewty k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalZLowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'VaridNewtF'
type VaridNewt k z s l lexicalAnnotation annotation = Fixed.Fix (VaridNewtF k z s l lexicalAnnotation annotation)
-- | 'VaridWherBase' with fewer unresolved variables, with default linking.
newtype VaridWherF k z s l lexicalAnnotation annotation fixpoint = MkVaridWherF { _unVaridWherF :: (VaridWherBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation fixpoint) (VaridInner k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'VaridWherF'
type VaridWher k z s l lexicalAnnotation annotation = Fixed.Fix (VaridWherF k z s l lexicalAnnotation annotation)
-- | 'VaridDefauBase' with fewer unresolved variables, with default linking.
newtype VaridDefauF k z s l lexicalAnnotation annotation fixpoint = MkVaridDefauF { _unVaridDefauF :: (VaridDefauBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation fixpoint) (VaridInner k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (VaridDefaul k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'VaridDefauF'
type VaridDefau k z s l lexicalAnnotation annotation = Fixed.Fix (VaridDefauF k z s l lexicalAnnotation annotation)
-- | 'VaridDerivBase' with fewer unresolved variables, with default linking.
newtype VaridDerivF k z s l lexicalAnnotation annotation fixpoint = MkVaridDerivF { _unVaridDerivF :: (VaridDerivBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation fixpoint) (VaridInner k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (VaridDerivi k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'VaridDerivF'
type VaridDeriv k z s l lexicalAnnotation annotation = Fixed.Fix (VaridDerivF k z s l lexicalAnnotation annotation)
-- | 'VaridForeiBase' with fewer unresolved variables, with default linking.
newtype VaridForeiF k z s l lexicalAnnotation annotation fixpoint = MkVaridForeiF { _unVaridForeiF :: (VaridForeiBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation fixpoint) (VaridInner k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (VaridForeig k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'VaridForeiF'
type VaridForei k z s l lexicalAnnotation annotation = Fixed.Fix (VaridForeiF k z s l lexicalAnnotation annotation)
-- | 'VaridImporBase' with fewer unresolved variables, with default linking.
newtype VaridImporF k z s l lexicalAnnotation annotation fixpoint = MkVaridImporF { _unVaridImporF :: (VaridImporBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation fixpoint) (VaridInner k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'VaridImporF'
type VaridImpor k z s l lexicalAnnotation annotation = Fixed.Fix (VaridImporF k z s l lexicalAnnotation annotation)
-- | 'VaridInfixBase' with fewer unresolved variables, with default linking.
newtype VaridInfixF k z s l lexicalAnnotation annotation fixpoint = MkVaridInfixF { _unVaridInfixF :: (VaridInfixBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation fixpoint) (VaridInner k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'VaridInfixF'
type VaridInfix k z s l lexicalAnnotation annotation = Fixed.Fix (VaridInfixF k z s l lexicalAnnotation annotation)
-- | 'VaridInstaBase' with fewer unresolved variables, with default linking.
newtype VaridInstaF k z s l lexicalAnnotation annotation fixpoint = MkVaridInstaF { _unVaridInstaF :: (VaridInstaBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation fixpoint) (VaridInner k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (VaridInstan k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'VaridInstaF'
type VaridInsta k z s l lexicalAnnotation annotation = Fixed.Fix (VaridInstaF k z s l lexicalAnnotation annotation)
-- | 'VaridModulBase' with fewer unresolved variables, with default linking.
newtype VaridModulF k z s l lexicalAnnotation annotation fixpoint = MkVaridModulF { _unVaridModulF :: (VaridModulBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation fixpoint) (VaridInner k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'VaridModulF'
type VaridModul k z s l lexicalAnnotation annotation = Fixed.Fix (VaridModulF k z s l lexicalAnnotation annotation)
-- | 'VaridNewtyBase' with fewer unresolved variables, with default linking.
newtype VaridNewtyF k z s l lexicalAnnotation annotation fixpoint = MkVaridNewtyF { _unVaridNewtyF :: (VaridNewtyBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation fixpoint) (VaridInner k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (VaridNewtyp k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'VaridNewtyF'
type VaridNewty k z s l lexicalAnnotation annotation = Fixed.Fix (VaridNewtyF k z s l lexicalAnnotation annotation)
-- | 'VaridDefaulBase' with fewer unresolved variables, with default linking.
newtype VaridDefaulF k z s l lexicalAnnotation annotation fixpoint = MkVaridDefaulF { _unVaridDefaulF :: (VaridDefaulBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation fixpoint) (VaridInner k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'VaridDefaulF'
type VaridDefaul k z s l lexicalAnnotation annotation = Fixed.Fix (VaridDefaulF k z s l lexicalAnnotation annotation)
-- | 'VaridDeriviBase' with fewer unresolved variables, with default linking.
newtype VaridDeriviF k z s l lexicalAnnotation annotation fixpoint = MkVaridDeriviF { _unVaridDeriviF :: (VaridDeriviBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation fixpoint) (VaridInner k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (VaridDerivin k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'VaridDeriviF'
type VaridDerivi k z s l lexicalAnnotation annotation = Fixed.Fix (VaridDeriviF k z s l lexicalAnnotation annotation)
-- | 'VaridForeigBase' with fewer unresolved variables, with default linking.
newtype VaridForeigF k z s l lexicalAnnotation annotation fixpoint = MkVaridForeigF { _unVaridForeigF :: (VaridForeigBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation fixpoint) (VaridInner k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'VaridForeigF'
type VaridForeig k z s l lexicalAnnotation annotation = Fixed.Fix (VaridForeigF k z s l lexicalAnnotation annotation)
-- | 'VaridInstanBase' with fewer unresolved variables, with default linking.
newtype VaridInstanF k z s l lexicalAnnotation annotation fixpoint = MkVaridInstanF { _unVaridInstanF :: (VaridInstanBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation fixpoint) (VaridInner k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (VaridInstanc k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'VaridInstanF'
type VaridInstan k z s l lexicalAnnotation annotation = Fixed.Fix (VaridInstanF k z s l lexicalAnnotation annotation)
-- | 'VaridNewtypBase' with fewer unresolved variables, with default linking.
newtype VaridNewtypF k z s l lexicalAnnotation annotation fixpoint = MkVaridNewtypF { _unVaridNewtypF :: (VaridNewtypBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation fixpoint) (VaridInner k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'VaridNewtypF'
type VaridNewtyp k z s l lexicalAnnotation annotation = Fixed.Fix (VaridNewtypF k z s l lexicalAnnotation annotation)
-- | 'VaridDerivinBase' with fewer unresolved variables, with default linking.
newtype VaridDerivinF k z s l lexicalAnnotation annotation fixpoint = MkVaridDerivinF { _unVaridDerivinF :: (VaridDerivinBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation fixpoint) (VaridInner k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'VaridDerivinF'
type VaridDerivin k z s l lexicalAnnotation annotation = Fixed.Fix (VaridDerivinF k z s l lexicalAnnotation annotation)
-- | 'VaridInstancBase' with fewer unresolved variables, with default linking.
newtype VaridInstancF k z s l lexicalAnnotation annotation fixpoint = MkVaridInstancF { _unVaridInstancF :: (VaridInstancBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation fixpoint) (VaridInner k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'VaridInstancF'
type VaridInstanc k z s l lexicalAnnotation annotation = Fixed.Fix (VaridInstancF k z s l lexicalAnnotation annotation)
-- | 'SymbolSansAscBase' with fewer unresolved variables, with default linking.
newtype SymbolSansAscF k z s l lexicalAnnotation annotation fixpoint = MkSymbolSansAscF { _unSymbolSansAscF :: (SymbolSansAscBase (UniSymbolSansSpecialishAsc k z s l lexicalAnnotation lexicalAnnotation fixpoint) annotation fixpoint) }
-- | Fixpoint applied to 'SymbolSansAscF'
type SymbolSansAsc k z s l lexicalAnnotation annotation = Fixed.Fix (SymbolSansAscF k z s l lexicalAnnotation annotation)
-- | 'UniSymbolSansSpecialishAscBase' with fewer unresolved variables, with default linking.
newtype UniSymbolSansSpecialishAscF k z s l lexicalAnnotation annotation fixpoint = MkUniSymbolSansSpecialishAscF { _unUniSymbolSansSpecialishAscF :: (UniSymbolSansSpecialishAscBase (l (LexicalUnicodeSymbolSansSpecialUnderscoreDoubleQuoteSingleQuoteAsciiKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'UniSymbolSansSpecialishAscF'
type UniSymbolSansSpecialishAsc k z s l lexicalAnnotation annotation = Fixed.Fix (UniSymbolSansSpecialishAscF k z s l lexicalAnnotation annotation)
-- | 'SymbolSansHyphenBase' with fewer unresolved variables, with default linking.
newtype SymbolSansHyphenF k z s l lexicalAnnotation annotation fixpoint = MkSymbolSansHyphenF { _unSymbolSansHyphenF :: (SymbolSansHyphenBase (AscSymbolSansHyphen k z s l lexicalAnnotation lexicalAnnotation fixpoint) (UniSymbolSansSpecialishHyphen k z s l lexicalAnnotation lexicalAnnotation fixpoint) annotation fixpoint) }
-- | Fixpoint applied to 'SymbolSansHyphenF'
type SymbolSansHyphen k z s l lexicalAnnotation annotation = Fixed.Fix (SymbolSansHyphenF k z s l lexicalAnnotation annotation)
-- | 'AscSymbolSansHyphenBase' with fewer unresolved variables, with default linking.
newtype AscSymbolSansHyphenF k z s l lexicalAnnotation annotation fixpoint = MkAscSymbolSansHyphenF { _unAscSymbolSansHyphenF :: (AscSymbolSansHyphenBase (l (LexicalExclamationKeyBase k z s)) (l (LexicalHashKeyBase k z s)) (l (LexicalDollarKeyBase k z s)) (l (LexicalPercentKeyBase k z s)) (l (LexicalAmpersandKeyBase k z s)) (l (LexicalAsteriskKeyBase k z s)) (l (LexicalPlusKeyBase k z s)) (l (LexicalDotKeyBase k z s)) (l (LexicalSlashKeyBase k z s)) (l (LexicalLeftAngleBracketKeyBase k z s)) (l (LexicalEqualsKeyBase k z s)) (l (LexicalRightAngleBracketKeyBase k z s)) (l (LexicalQuestionMarkKeyBase k z s)) (l (LexicalAtKeyBase k z s)) (l (LexicalBackslashKeyBase k z s)) (l (LexicalCaretKeyBase k z s)) (l (LexicalPipeKeyBase k z s)) (l (LexicalTildeKeyBase k z s)) (l (LexicalColonKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'AscSymbolSansHyphenF'
type AscSymbolSansHyphen k z s l lexicalAnnotation annotation = Fixed.Fix (AscSymbolSansHyphenF k z s l lexicalAnnotation annotation)
-- | 'UniSymbolSansSpecialishHyphenBase' with fewer unresolved variables, with default linking.
newtype UniSymbolSansSpecialishHyphenF k z s l lexicalAnnotation annotation fixpoint = MkUniSymbolSansSpecialishHyphenF { _unUniSymbolSansSpecialishHyphenF :: (UniSymbolSansSpecialishHyphenBase (l (LexicalUnicodeSymbolSansSpecialUnderscoreDoubleQuoteSingleQuoteHyphenKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'UniSymbolSansSpecialishHyphenF'
type UniSymbolSansSpecialishHyphen k z s l lexicalAnnotation annotation = Fixed.Fix (UniSymbolSansSpecialishHyphenF k z s l lexicalAnnotation annotation)
-- | 'VarSymBase' with fewer unresolved variables, with default linking.
newtype VarSymF k z s l lexicalAnnotation annotation fixpoint = MkVarSymF { _unVarSymF :: (VarSymBase [] (SymbolSansAsc k z s l lexicalAnnotation lexicalAnnotation fixpoint) (Symbol k z s l lexicalAnnotation lexicalAnnotation fixpoint) (l (LexicalExclamationKeyBase k z s)) (l (LexicalHashKeyBase k z s)) (l (LexicalDollarKeyBase k z s)) (l (LexicalPercentKeyBase k z s)) (l (LexicalAmpersandKeyBase k z s)) (l (LexicalAsteriskKeyBase k z s)) (l (LexicalPlusKeyBase k z s)) (l (LexicalDotKeyBase k z s)) (l (LexicalSlashKeyBase k z s)) (l (LexicalLeftAngleBracketKeyBase k z s)) (l (LexicalRightAngleBracketKeyBase k z s)) (l (LexicalQuestionMarkKeyBase k z s)) (l (LexicalCaretKeyBase k z s)) (l (LexicalHyphenKeyBase k z s)) (l (LexicalEqualsKeyBase k z s)) (l (LexicalAtKeyBase k z s)) (l (LexicalBackslashKeyBase k z s)) (l (LexicalPipeKeyBase k z s)) (l (LexicalTildeKeyBase k z s)) (l (LexicalColonKeyBase k z s)) (SymbolSansHyphen k z s l lexicalAnnotation lexicalAnnotation fixpoint) annotation fixpoint) }
-- | Fixpoint applied to 'VarSymF'
type VarSym k z s l lexicalAnnotation annotation = Fixed.Fix (VarSymF k z s l lexicalAnnotation annotation)
-- | 'SymbolSansColonBase' with fewer unresolved variables, with default linking.
newtype SymbolSansColonF k z s l lexicalAnnotation annotation fixpoint = MkSymbolSansColonF { _unSymbolSansColonF :: (SymbolSansColonBase (AscSymbolSansColon k z s l lexicalAnnotation lexicalAnnotation fixpoint) (UniSymbolSansSpecialishColon k z s l lexicalAnnotation lexicalAnnotation fixpoint) annotation fixpoint) }
-- | Fixpoint applied to 'SymbolSansColonF'
type SymbolSansColon k z s l lexicalAnnotation annotation = Fixed.Fix (SymbolSansColonF k z s l lexicalAnnotation annotation)
-- | 'AscSymbolSansColonBase' with fewer unresolved variables, with default linking.
newtype AscSymbolSansColonF k z s l lexicalAnnotation annotation fixpoint = MkAscSymbolSansColonF { _unAscSymbolSansColonF :: (AscSymbolSansColonBase (l (LexicalExclamationKeyBase k z s)) (l (LexicalHashKeyBase k z s)) (l (LexicalDollarKeyBase k z s)) (l (LexicalPercentKeyBase k z s)) (l (LexicalAmpersandKeyBase k z s)) (l (LexicalAsteriskKeyBase k z s)) (l (LexicalPlusKeyBase k z s)) (l (LexicalDotKeyBase k z s)) (l (LexicalSlashKeyBase k z s)) (l (LexicalLeftAngleBracketKeyBase k z s)) (l (LexicalEqualsKeyBase k z s)) (l (LexicalRightAngleBracketKeyBase k z s)) (l (LexicalQuestionMarkKeyBase k z s)) (l (LexicalAtKeyBase k z s)) (l (LexicalBackslashKeyBase k z s)) (l (LexicalCaretKeyBase k z s)) (l (LexicalPipeKeyBase k z s)) (l (LexicalHyphenKeyBase k z s)) (l (LexicalTildeKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'AscSymbolSansColonF'
type AscSymbolSansColon k z s l lexicalAnnotation annotation = Fixed.Fix (AscSymbolSansColonF k z s l lexicalAnnotation annotation)
-- | 'UniSymbolSansSpecialishColonBase' with fewer unresolved variables, with default linking.
newtype UniSymbolSansSpecialishColonF k z s l lexicalAnnotation annotation fixpoint = MkUniSymbolSansSpecialishColonF { _unUniSymbolSansSpecialishColonF :: (UniSymbolSansSpecialishColonBase (l (LexicalUnicodeSymbolSansSpecialUnderscoreDoubleQuoteSingleQuoteColonKeyBase k z s)) annotation fixpoint) }
-- | Fixpoint applied to 'UniSymbolSansSpecialishColonF'
type UniSymbolSansSpecialishColon k z s l lexicalAnnotation annotation = Fixed.Fix (UniSymbolSansSpecialishColonF k z s l lexicalAnnotation annotation)
-- | 'ConSymBase' with fewer unresolved variables, with default linking.
newtype ConSymF k z s l lexicalAnnotation annotation fixpoint = MkConSymF { _unConSymF :: (ConSymBase [] (l (LexicalColonKeyBase k z s)) (Symbol k z s l lexicalAnnotation lexicalAnnotation fixpoint) (SymbolSansColon k z s l lexicalAnnotation lexicalAnnotation fixpoint) annotation fixpoint) }
-- | Fixpoint applied to 'ConSymF'
type ConSym k z s l lexicalAnnotation annotation = Fixed.Fix (ConSymF k z s l lexicalAnnotation annotation)
