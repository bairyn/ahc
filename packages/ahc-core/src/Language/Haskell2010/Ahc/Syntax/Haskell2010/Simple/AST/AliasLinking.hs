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
-- Provide further instantiated type aliases with 'StandardLinking' structures
-- as the fixpoint.
--
-- That is, these structures are base structures with variables instantiated to
-- other base structures with our linking if available, or with
-- 'StandardLinking' if not (that is, for fixpoints).
--
-- These structures' variables are instantiated to other type aliases within
-- this module except for mutually recursive references.  In this case, the
-- variables, where what they refer to could refer back to us (possibly
-- indirectly), triggering a cyclic reference in mutal recursion, are instead
-- instantiated using their fixpoint-based 'StandardLinking' aliases, not the
-- intra-module 'AliasLinking' aliases.  This is analogous to the safety level
-- (‘safe’ vs.  ‘unsafe’) in ‘foregn import’ declarations, as we refer to
-- another intra-module variable that can refer back to us in the case of
-- mutual recursion.
--
-- Additionally, choose Prelude definitions to provide for common data
-- structure idioms like ‘maybe’, falling back to our own definitions where
-- they are not available, like 'Fixed'.
--
-- Here are mutually recursive references, where the 'StandardLinking' rather
-- than 'AliasLinking' reference is used to instantiate (like an necessarily
-- ‘safe’ ‘foreign import’ declaration):
--   - QconBase         -> GconSymBase
--   - GconSymBase      -> QconBase
--   - TypeBase         -> BtypeBase
--   - BtypeBase        -> AtypeBase
--   - AtypeBase        -> TypeBase
--   - PatBase          -> LpatBase
--   - LpatBase         -> ApatBase
--   - ApatBase         -> FpatBase
--     - ApatBase         -> PatBase
--   - FpatBase         -> PatBase
--   - ExpBase          -> InfixExpBase
--   - InfixExpBase     -> LexpBase
--   - LexpBase         -> ExpBase
--     - LexpBase         -> AltsBase
--     - LexpBase         -> StmtsBase
--     - LexpBase         -> FexpBase
--   - FexpBase         -> AexpBase
--   - AexpBase         -> ExpBase
--     - AexpBase         -> QualBase
--     - AexpBase         -> InfixExpBase
--     - AexpBase         -> FbindBase
--     - AexpBase         -> AexpSansQconBase
--   - AexpSansQconBase -> ExpBase
--     - AexpSansQconBase -> QualBase
--     - AexpSansQconBase -> InfixExpBase
--     - AexpSansQconBase -> FbindBase
--   - AltsBase           -> AltBase
--   - AltBase            -> ExpBase
--     - AltBase            -> GdpatBase
--     - AltBase            -> DeclsBase
--   - GdpatBase          -> ExpBase
--   - GuardsBase         -> GuardBase
--   - GuardBase          -> InfixExpBase
--     - GuardBase          -> DeclsBase
--   - StmtsBase          -> StmtBase
--   - StmtBase           -> ExpBase
--     - StmtBase           -> DeclsBase
--   - QualBase           -> ExpBase
--     - QualBase           -> DeclsBase
--   - FbindBase          -> ExpBase
--   - DeclsBase          -> DeclBase
--   - DeclBase           -> RhsBase
--   - RhsBase            -> ExpBase
--     - RhsBase            -> GdrhsBase
--   - GdrhsBase          -> GuardsBase
--     - GdrhsBase          -> ExpBase
--   - GuardsBase         -> GuardBase
--   - GuardBase          -> InfixExpBase
--     - GuardBase          -> DeclsBase
--
-- Here are non-mutually recursive references (this involves ‘fixpoint’):
--   - TypeBase                      -> TypeBase
--   - BtypeBase                     -> BtypeBase
--   - GdrhsBase                     -> GdrhsBase
--   - FtypeBase                     -> FtypeBase
--   - InfixExpBase                  -> InfixExpBase
--   - FexpBase                      -> FexpBase
--   - AexpSansQconBase              -> AexpSansQconBase
--   - GdpatBase                     -> GdpatBase
--   - PatBase                       -> PatBase
--   - ApatBase                      -> ApatBase
--   - NcommentBase                  -> NcommentBase
--   - BigAnySeqBase                 -> BigAnySeqBase
--   - BigAnySeqValidNcomChar1_0Base -> BigAnySeqValidNcomChar1_0Base
--   - BigAnySeqValidNcomChar1_1Base -> BigAnySeqValidNcomChar1_1Base
--
-- The following types have both mutually- and non-mutually- recursive
-- references:
--   - TypeBase
--   - BtypeBase
--   - GdrhsBase
--   - InfixExpBase
--   - FexpBase
--   - AexpSansQconBase
--   - GdpatBase
--   - PatBase
--   - ApatBase
--
-- (Additionally just alias to 'StandardLinking's LexicalFoundation.)
module Language.Haskell2010.Ahc.Syntax.Haskell2010.Simple.AST.AliasLinking (
	-- Export everything, explicitly.

	-- * Structures with alias linking.

	-- ** Lexical foundation structures.

	LexicalFoundation,

	-- ** Lexical structures.

	-- *** § 2.2 Lexical Program Structure types.
	Program,
	Lexeme,
	Literal,
	Special,
	SpecialSansNc,

	Whitespace,
	Whitestuff,
	Whitechar,
	Newline,
	Return,
	Linefeed,
	Vertab,
	Formfeed,
	Space,
	Tab,
	UniWhite,

	Comment,
	Dashes,
	OpenCom,
	CloseCom,
	Ncomment,
	BigAnySeq,
	BigAnySeqValidNcomChar1_0,
	BigAnySeqValidNcomChar1_1,
	BigAny,
	BigAnySansNc,
	Any,
	AnySansSymbol,
	Graphic,
	GraphicSansSingleQuoteOrBackslash,
	GraphicSansDoubleQuoteOrBackslash,
	GraphicSansSymbol,
	GraphicSansNc,

	Small,
	AscSmall,
	UniSmall,

	Large,
	AscLarge,
	UniLarge,
	Symbol,
	SymbolSansBackslash,
	SymbolSansNc,

	AscSymbol,
	AscSymbolSansBackslash,
	AscSymbolSansNc,
	UniSymbol,
	UniSymbolSansSpecialish,
	UniSymbolSansSpecialishSansNc,
	Digit,
	AscDigit,
	UniDigit,
	Octit,
	Hexit,

	-- *** § 2.4 Identifiers and Operators types.
	UnqualifiedName,
	VaridNoExclusions,
	VaridStart,
	IdentifierInner,
	VaridInner,
	Conid,
	ConidStart,
	ConidInner,
	Reservedid,
	VarSymNoExtraExclusions,
	VarSymStart,
	ConSymNoExtraExclusions,
	ConSymStart,
	ReservedOp,
	Tyvar,
	Tycon,
	Tycls,
	Modid,
	Name,
	Qvarid,
	Qconid,
	Qtycon,
	Qtycls,
	QvarSym,
	QvarSymSansMinus,
	QconSym,

	-- *** § 2.5 Numeric Literals types.
	Decimal,
	Octal,
	Hexadecimal,
	Integer,
	Float,
	Exponent,

	-- *** § 2.6 Character and String Literals types.

	Char,
	CharLiteralInner,
	String,
	StringLiteralInnerUnit,
	Escape,
	EscapeSansBackslashAndAmpersand,
	EscapeInner,
	EscapeInnerSansAmpersand,
	CharEsc,
	CharEscSansAmpersand,
	Ascii,
	Cntrl,
	Gap,

	-- *** § 8.3 (FFI) Lexical Structure types.
	Chname,
	Cid,
	Chchar,
	Letter,
	AscSymbolSansAmpersand,

	-- *** Base lexical structures.

	-- **** Pseudo-foundational lexical structures.

	LexicalPseudo,

	-- ***** Non-symbolic keyword pseudo-lexical structures.

	LexicalNonsymKeyword,
	LexicalCase,
	LexicalClass,
	LexicalData,
	LexicalDefault,
	LexicalDeriving,
	LexicalDo,
	LexicalElse,
	LexicalForeign,
	LexicalIf,
	LexicalImport,
	LexicalIn,
	LexicalInfix,
	LexicalInfixl,
	LexicalInfixr,
	LexicalInstance,
	LexicalLet,
	LexicalModule,
	LexicalNewtype,
	LexicalOf,
	LexicalThen,
	LexicalType,
	LexicalWhere,

	-- ***** Non-symbolic non-keyword pseudo-lexical structures.

	LexicalNonsymNonkeyword,
	LexicalAs,
	LexicalHiding,
	LexicalQualified,

	-- ***** Symbolic alias pseudo-lexical structures.

	LexicalSymAlias,
	LexicalDotDot,
	LexicalDoubleColon,
	LexicalDoubleRightArrow,
	LexicalLeftArrow,
	LexicalRightArrow,

	-- ***** Alias pseudo-lexical structures.

	LexicalAlias,
	LexicalSpace,
	LexicalMinus,
	LexicalAsciiLambda,

	-- ***** Non-symbolic numeric literal prefix pseudo-lexical structures.

	LexicalNumPrefix,
	Lexical0o,
	Lexical0O,
	Lexical0x,
	Lexical0X,

	-- ***** FFI pseudo-lexical structures.

	{-
	LexicalImport,
	-}
	LexicalExport,
	LexicalCcall,
	LexicalStdcall,
	LexicalCplusplus,
	LexicalJvm,
	LexicalDotnet,
	LexicalUnsafe,
	LexicalSafe,
	LexicalStatic,
	LexicalDynamic,
	LexicalWrapper,

	-- ** Grammatical structures.

	-- *** § 5.1 Module Structure types.
	Module,
	Body,
	ImpDecls,
	TopDecls,

	-- *** § 5.2 Export Lists types.
	Exports,
	Export,
	Cname,

	-- *** § 5.3 Import Declarations types.
	ImpDecl,
	ImpSpec,
	Import,

	-- *** § 4 Declarations and Bindings types.
	TopDecl,
	Decls,
	Decl,
	Cdecls,
	Cdecl,
	Idecls,
	Idecl,
	GenDecl,
	Ops,
	Vars,
	Fixity,

	-- *** § 4.1.2 Syntax of Types types.
	Type,
	Btype,
	Atype,
	Gtycon,

	-- *** § 4.1.3 Syntax of Class Assertions and Contexts types.
	Context,
	Class,
	{-
	ClassQtycls,
	ClassTycls,
	ClassTyvar,
	-}

	-- *** § 4.2.1 Algebraic Datatype Declarations types.
	SimpleType,
	Constrs,
	Constr,
	EvalAtype,
	FieldDecl,
	Deriving,
	Dclass,

	-- *** § 4.2.3 Datatype Renamings types.
	NewConstr,

	-- *** § 4.3.1 Type Classes and Overloading types.
	Scontext,
	SimpleClass,

	-- *** § 4.3.2 Instance Declarations types.
	Inst,

	{-
	-- *** § 4.4.2 Fixity Declarations types.
	FixityOp,
	-}

	-- *** § 4.4.3 Function and Pattern Bindings types.
	Funlhs,
	Rhs,
	Gdrhs,
	{-
	Guards,
	Guard,
	-}

	-- *** § 8.4 Foreign Declarations types.
	Fdecl,
	CallConv,
	Impent,
	Expent,
	Safety,

	-- ** § 8.4.2 Foreign Types types.
	Ftype,
	Frtype,
	Fatype,

	-- ** § 8.5.1 Standard C Calls types.
	ImpentCcall,
	ExpentCcall,

	-- *** § 3 Expressions types.
	Exp,
	InfixExp,
	Lexp,
	Fexp,
	Aexp,
	AexpSansQcon,

	-- *** § 3.2 Variables, Constructors, Operators, and Literals types.
	Gcon,
	GconSansQcon,
	Var,
	Qvar,
	Con,
	Qcon,
	Varop,
	Qvarop,
	QvaropSansMinus,
	Conop,
	Qconop,
	Op,
	Qop,
	QopSansMinus,
	GconSym,

	-- *** § 3.11 List Comprehensions types.
	Qual,

	-- *** § 3.13 Case Expressions types.
	Alts,
	Alt,
	Gdpat,
	Guards,
	Guard,

	-- *** § 3.14 Do Expressions types.
	Stmts,
	Stmt,

	-- *** § 3.15.2 Construction Using Field Labels types.
	Fbind,

	-- *** § 3.17.1 Patterns types.
	Pat,
	Lpat,
	Apat,
	Fpat,

	-- ** Exclusion structures.

	-- *** § 2.4 Identifiers and Operators types.

	-- **** Exclusion structures types.

	VaridInnerSansAscSmallUnderscore,
	SmallSansAscSmallUnderscore,
	UniSmallSansAsc,
	Varid,
	VaridC,
	VaridD,
	VaridE,
	VaridF_,
	VaridI,
	VaridL,
	VaridM,
	VaridN,
	VaridO,
	VaridT,
	VaridW,
	VaridCa,
	VaridCl,
	VaridDa,
	VaridDe,
	VaridEl,
	VaridFo,
	VaridIm,
	VaridIn,
	VaridLe,
	VaridMo,
	VaridNe,
	VaridTh,
	VaridTy,
	VaridWh,
	VaridCas,
	VaridCla,
	VaridDat,
	VaridDef,
	VaridDer,
	VaridEls,
	VaridFor,
	VaridImp,
	VaridInf,
	VaridIns,
	VaridMod,
	VaridNew,
	VaridThe,
	VaridTyp,
	VaridWhe,
	VaridClas,
	VaridDefa,
	VaridDeri,
	VaridFore,
	VaridImpo,
	VaridInfi,
	VaridInst,
	VaridModu,
	VaridNewt,
	VaridWher,
	VaridDefau,
	VaridDeriv,
	VaridForei,
	VaridImpor,
	VaridInfix,
	VaridInsta,
	VaridModul,
	VaridNewty,
	VaridDefaul,
	VaridDerivi,
	VaridForeig,
	VaridInstan,
	VaridNewtyp,
	VaridDerivin,
	VaridInstanc,

	SymbolSansAsc,
	UniSymbolSansSpecialishAsc,
	SymbolSansHyphen,
	AscSymbolSansHyphen,
	UniSymbolSansSpecialishHyphen,
	VarSym,
	VarSymSansMinus,

	SymbolSansColon,
	AscSymbolSansColon,
	UniSymbolSansSpecialishColon,
	ConSym,
) where

import           Prelude ()

import           Language.Haskell2010.Ahc.Syntax.Haskell2010.Simple.AST.Base
import qualified Language.Haskell2010.Ahc.Syntax.Haskell2010.Simple.AST.StandardLinking as StandardLinking

import qualified Prelude

-- (Tip: the bulk of this module can be generated by copying StandardLinking
-- definitions, filtering out the newtypes, and using the same structure for
-- your ‘type’ definitions here, with the fixpoint applied as documented.)

{-
 - ----------------------------------------------------------------
 - Structures with alias linking.
 - ----------------------------------------------------------------
 -}

-- Lexical Foundation structures.

-- | ‘LexicalFoundationBase’ with alias linking.
--
-- - k: ‘lexicalTypeIndexerKeyCons’
-- - z: ‘zero’
-- - s: ‘succ’
-- - l: ‘lexicalTypeIndexer’
--
-- (This is an alias for 'StandardLinking's 'LexicalFoundation' instantiation
-- linking.)
type LexicalFoundation k z s l typeValue = StandardLinking.LexicalFoundation k z s l typeValue

-- Lexical structures.

-- § 2.2 Lexical Program Structure types.

-- | 'ProgramBase' with fewer unresolved variables, with alias linking.
type Program k z s l lexicalAnnotation annotation = ProgramBase [] Prelude.Either (Lexeme k z s l lexicalAnnotation lexicalAnnotation) (Whitespace k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.Program k z s l lexicalAnnotation annotation)
-- | 'LexemeBase' with fewer unresolved variables, with alias linking.
type Lexeme k z s l lexicalAnnotation annotation = LexemeBase (Qvarid k z s l lexicalAnnotation lexicalAnnotation) (Qconid k z s l lexicalAnnotation lexicalAnnotation) (QvarSym k z s l lexicalAnnotation lexicalAnnotation) (QconSym k z s l lexicalAnnotation lexicalAnnotation) (Literal k z s l lexicalAnnotation lexicalAnnotation) (Special k z s l lexicalAnnotation lexicalAnnotation) (ReservedOp k z s l lexicalAnnotation lexicalAnnotation) (Reservedid k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.Lexeme k z s l lexicalAnnotation annotation)
-- | 'LiteralBase' with fewer unresolved variables, with alias linking.
type Literal k z s l lexicalAnnotation annotation = LiteralBase (Integer k z s l lexicalAnnotation lexicalAnnotation) (Float k z s l lexicalAnnotation lexicalAnnotation) (Char k z s l lexicalAnnotation lexicalAnnotation) (String k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.Literal k z s l lexicalAnnotation annotation)
-- | 'SpecialBase' with fewer unresolved variables, with alias linking.
type Special k z s l lexicalAnnotation annotation = SpecialBase (l (LexicalLeftParenthesisKeyBase k z s)) (l (LexicalRightParenthesisKeyBase k z s)) (l (LexicalCommaKeyBase k z s)) (l (LexicalSemicolonKeyBase k z s)) (l (LexicalLeftBracketKeyBase k z s)) (l (LexicalRightBracketKeyBase k z s)) (l (LexicalBacktickKeyBase k z s)) (l (LexicalLeftBraceKeyBase k z s)) (l (LexicalRightBraceKeyBase k z s)) annotation (StandardLinking.Special k z s l lexicalAnnotation annotation)
-- | 'SpecialSansNcBase' with fewer unresolved variables, with alias linking.
type SpecialSansNc k z s l lexicalAnnotation annotation = SpecialSansNcBase (l (LexicalLeftParenthesisKeyBase k z s)) (l (LexicalRightParenthesisKeyBase k z s)) (l (LexicalCommaKeyBase k z s)) (l (LexicalSemicolonKeyBase k z s)) (l (LexicalLeftBracketKeyBase k z s)) (l (LexicalRightBracketKeyBase k z s)) (l (LexicalBacktickKeyBase k z s)) annotation (StandardLinking.SpecialSansNc k z s l lexicalAnnotation annotation)
-- | 'WhitespaceBase' with fewer unresolved variables, with alias linking.
type Whitespace k z s l lexicalAnnotation annotation = WhitespaceBase [] (Whitestuff k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.Whitespace k z s l lexicalAnnotation annotation)
-- | 'WhitestuffBase' with fewer unresolved variables, with alias linking.
type Whitestuff k z s l lexicalAnnotation annotation = WhitestuffBase (Whitechar k z s l lexicalAnnotation lexicalAnnotation) (Comment k z s l lexicalAnnotation lexicalAnnotation) (Ncomment k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.Whitestuff k z s l lexicalAnnotation annotation)
-- | 'WhitecharBase' with fewer unresolved variables, with alias linking.
type Whitechar k z s l lexicalAnnotation annotation = WhitecharBase (Newline k z s l lexicalAnnotation lexicalAnnotation) (Vertab k z s l lexicalAnnotation lexicalAnnotation) (Space k z s l lexicalAnnotation lexicalAnnotation) (Tab k z s l lexicalAnnotation lexicalAnnotation) (UniWhite k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.Whitechar k z s l lexicalAnnotation annotation)
-- | 'NewlineBase' with fewer unresolved variables, with alias linking.
type Newline k z s l lexicalAnnotation annotation = NewlineBase (Return k z s l lexicalAnnotation lexicalAnnotation) (Linefeed k z s l lexicalAnnotation lexicalAnnotation) (Formfeed k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.Newline k z s l lexicalAnnotation annotation)
-- | 'ReturnBase' with fewer unresolved variables, with alias linking.
type Return k z s l lexicalAnnotation annotation = ReturnBase (l (LexicalCRKeyBase k z s)) annotation (StandardLinking.Return k z s l lexicalAnnotation annotation)
-- | 'LinefeedBase' with fewer unresolved variables, with alias linking.
type Linefeed k z s l lexicalAnnotation annotation = LinefeedBase (l (LexicalLFKeyBase k z s)) annotation (StandardLinking.Linefeed k z s l lexicalAnnotation annotation)
-- | 'VertabBase' with fewer unresolved variables, with alias linking.
type Vertab k z s l lexicalAnnotation annotation = VertabBase (l (LexicalVTKeyBase k z s)) annotation (StandardLinking.Vertab k z s l lexicalAnnotation annotation)
-- | 'FormfeedBase' with fewer unresolved variables, with alias linking.
type Formfeed k z s l lexicalAnnotation annotation = FormfeedBase (l (LexicalFFKeyBase k z s)) annotation (StandardLinking.Formfeed k z s l lexicalAnnotation annotation)
-- | 'SpaceBase' with fewer unresolved variables, with alias linking.
type Space k z s l lexicalAnnotation annotation = SpaceBase (l (LexicalSPKeyBase k z s)) annotation (StandardLinking.Space k z s l lexicalAnnotation annotation)
-- | 'TabBase' with fewer unresolved variables, with alias linking.
type Tab k z s l lexicalAnnotation annotation = TabBase (l (LexicalHTKeyBase k z s)) annotation (StandardLinking.Tab k z s l lexicalAnnotation annotation)
-- | 'UniWhiteBase' with fewer unresolved variables, with alias linking.
type UniWhite k z s l lexicalAnnotation annotation = UniWhiteBase (l (LexicalUnicodeWhitespaceCharKeyBase k z s)) annotation (StandardLinking.UniWhite k z s l lexicalAnnotation annotation)
-- | 'CommentBase' with fewer unresolved variables, with alias linking.
type Comment k z s l lexicalAnnotation annotation = CommentBase (,) Prelude.Maybe [] (Dashes k z s l lexicalAnnotation lexicalAnnotation) (AnySansSymbol k z s l lexicalAnnotation lexicalAnnotation) (Any k z s l lexicalAnnotation lexicalAnnotation) (Newline k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.Comment k z s l lexicalAnnotation annotation)
-- | 'DashesBase' with fewer unresolved variables, with alias linking.
type Dashes k z s l lexicalAnnotation annotation = DashesBase [] (l (LexicalHyphenKeyBase k z s)) annotation (StandardLinking.Dashes k z s l lexicalAnnotation annotation)
-- | 'OpenComBase' with fewer unresolved variables, with alias linking.
type OpenCom k z s l lexicalAnnotation annotation = OpenComBase (l (LexicalLeftBraceKeyBase k z s)) (l (LexicalHyphenKeyBase k z s)) annotation (StandardLinking.OpenCom k z s l lexicalAnnotation annotation)
-- | 'CloseComBase' with fewer unresolved variables, with alias linking.
type CloseCom k z s l lexicalAnnotation annotation = CloseComBase (l (LexicalHyphenKeyBase k z s)) (l (LexicalRightBraceKeyBase k z s)) annotation (StandardLinking.CloseCom k z s l lexicalAnnotation annotation)
-- | 'NcommentBase' with fewer unresolved variables, with alias linking.
type Ncomment k z s l lexicalAnnotation annotation = NcommentBase (,) [] (OpenCom k z s l lexicalAnnotation lexicalAnnotation) (BigAnySeq k z s l lexicalAnnotation lexicalAnnotation) (CloseCom k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.Ncomment k z s l lexicalAnnotation annotation)
-- | 'BigAnySeqBase' with fewer unresolved variables, with alias linking.
type BigAnySeq k z s l lexicalAnnotation annotation = BigAnySeqBase (BigAnySansNc k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalRightBraceKeyBase k z s)) (l (LexicalLeftBraceKeyBase k z s)) (BigAnySeqValidNcomChar1_0 k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalHyphenKeyBase k z s)) (BigAnySeqValidNcomChar1_1 k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.BigAnySeq k z s l lexicalAnnotation annotation)
-- | 'BigAnySeqValidNcomChar1_0Base' with fewer unresolved variables, with alias linking.
type BigAnySeqValidNcomChar1_0 k z s l lexicalAnnotation annotation = BigAnySeqValidNcomChar1_0Base (BigAnySansNc k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalLeftBraceKeyBase k z s)) (l (LexicalRightBraceKeyBase k z s)) (l (LexicalEndOfParseKeyBase k z s)) annotation (StandardLinking.BigAnySeqValidNcomChar1_0 k z s l lexicalAnnotation annotation)
-- | 'BigAnySeqValidNcomChar1_1Base' with fewer unresolved variables, with alias linking.
type BigAnySeqValidNcomChar1_1 k z s l lexicalAnnotation annotation = BigAnySeqValidNcomChar1_1Base (BigAnySansNc k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalLeftBraceKeyBase k z s)) (BigAnySeqValidNcomChar1_0 k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalHyphenKeyBase k z s)) (l (LexicalEndOfParseKeyBase k z s)) annotation (StandardLinking.BigAnySeqValidNcomChar1_1 k z s l lexicalAnnotation annotation)
-- | 'BigAnyBase' with fewer unresolved variables, with alias linking.
type BigAny k z s l lexicalAnnotation annotation = BigAnyBase (Graphic k z s l lexicalAnnotation lexicalAnnotation) (Whitechar k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.BigAny k z s l lexicalAnnotation annotation)
-- | 'BigAnySansNcBase' with fewer unresolved variables, with alias linking.
type BigAnySansNc k z s l lexicalAnnotation annotation = BigAnySansNcBase (GraphicSansNc k z s l lexicalAnnotation lexicalAnnotation) (Whitechar k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.BigAnySansNc k z s l lexicalAnnotation annotation)
-- | 'AnyBase' with fewer unresolved variables, with alias linking.
type Any k z s l lexicalAnnotation annotation = AnyBase (Graphic k z s l lexicalAnnotation lexicalAnnotation) (Space k z s l lexicalAnnotation lexicalAnnotation) (Tab k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.Any k z s l lexicalAnnotation annotation)
-- | 'AnySansSymbolBase' with fewer unresolved variables, with alias linking.
type AnySansSymbol k z s l lexicalAnnotation annotation = AnySansSymbolBase (GraphicSansSymbol k z s l lexicalAnnotation lexicalAnnotation) (Space k z s l lexicalAnnotation lexicalAnnotation) (Tab k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.AnySansSymbol k z s l lexicalAnnotation annotation)
-- | 'GraphicBase' with fewer unresolved variables, with alias linking.
type Graphic k z s l lexicalAnnotation annotation = GraphicBase (Small k z s l lexicalAnnotation lexicalAnnotation) (Large k z s l lexicalAnnotation lexicalAnnotation) (Symbol k z s l lexicalAnnotation lexicalAnnotation) (Digit k z s l lexicalAnnotation lexicalAnnotation) (Special k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalDoubleQuoteKeyBase k z s)) (l (LexicalSingleQuoteKeyBase k z s)) annotation (StandardLinking.Graphic k z s l lexicalAnnotation annotation)
-- | 'GraphicSansSingleQuoteOrBackslashBase' with fewer unresolved variables, with alias linking.
type GraphicSansSingleQuoteOrBackslash k z s l lexicalAnnotation annotation = GraphicSansSingleQuoteOrBackslashBase (Small k z s l lexicalAnnotation lexicalAnnotation) (Large k z s l lexicalAnnotation lexicalAnnotation) (SymbolSansBackslash k z s l lexicalAnnotation lexicalAnnotation) (Digit k z s l lexicalAnnotation lexicalAnnotation) (Special k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalDoubleQuoteKeyBase k z s)) annotation (StandardLinking.GraphicSansSingleQuoteOrBackslash k z s l lexicalAnnotation annotation)
-- | 'GraphicSansDoubleQuoteOrBackslashBase' with fewer unresolved variables, with alias linking.
type GraphicSansDoubleQuoteOrBackslash k z s l lexicalAnnotation annotation = GraphicSansDoubleQuoteOrBackslashBase (Small k z s l lexicalAnnotation lexicalAnnotation) (Large k z s l lexicalAnnotation lexicalAnnotation) (SymbolSansBackslash k z s l lexicalAnnotation lexicalAnnotation) (Digit k z s l lexicalAnnotation lexicalAnnotation) (Special k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalSingleQuoteKeyBase k z s)) annotation (StandardLinking.GraphicSansDoubleQuoteOrBackslash k z s l lexicalAnnotation annotation)
-- | 'GraphicSansSymbolBase' with fewer unresolved variables, with alias linking.
type GraphicSansSymbol k z s l lexicalAnnotation annotation = GraphicSansSymbolBase (Small k z s l lexicalAnnotation lexicalAnnotation) (Large k z s l lexicalAnnotation lexicalAnnotation) (Digit k z s l lexicalAnnotation lexicalAnnotation) (Special k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalDoubleQuoteKeyBase k z s)) (l (LexicalSingleQuoteKeyBase k z s)) annotation (StandardLinking.GraphicSansSymbol k z s l lexicalAnnotation annotation)
-- | 'GraphicSansNcBase' with fewer unresolved variables, with alias linking.
type GraphicSansNc k z s l lexicalAnnotation annotation = GraphicSansNcBase (Small k z s l lexicalAnnotation lexicalAnnotation) (Large k z s l lexicalAnnotation lexicalAnnotation) (SymbolSansNc k z s l lexicalAnnotation lexicalAnnotation) (Digit k z s l lexicalAnnotation lexicalAnnotation) (SpecialSansNc k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalDoubleQuoteKeyBase k z s)) (l (LexicalSingleQuoteKeyBase k z s)) annotation (StandardLinking.GraphicSansNc k z s l lexicalAnnotation annotation)
-- | 'SmallBase' with fewer unresolved variables, with alias linking.
type Small k z s l lexicalAnnotation annotation = SmallBase (AscSmall k z s l lexicalAnnotation lexicalAnnotation) (UniSmall k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalUnderscoreKeyBase k z s)) annotation (StandardLinking.Small k z s l lexicalAnnotation annotation)
-- | 'AscSmallBase' with fewer unresolved variables, with alias linking.
type AscSmall k z s l lexicalAnnotation annotation = AscSmallBase (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation (StandardLinking.AscSmall k z s l lexicalAnnotation annotation)
-- | 'UniSmallBase' with fewer unresolved variables, with alias linking.
type UniSmall k z s l lexicalAnnotation annotation = UniSmallBase (l (LexicalUnicodeSmallKeyBase k z s)) annotation (StandardLinking.UniSmall k z s l lexicalAnnotation annotation)
-- | 'LargeBase' with fewer unresolved variables, with alias linking.
type Large k z s l lexicalAnnotation annotation = LargeBase (AscLarge k z s l lexicalAnnotation lexicalAnnotation) (UniLarge k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.Large k z s l lexicalAnnotation annotation)
-- | 'AscLargeBase' with fewer unresolved variables, with alias linking.
type AscLarge k z s l lexicalAnnotation annotation = AscLargeBase (l (LexicalAKeyBase k z s)) (l (LexicalBKeyBase k z s)) (l (LexicalCKeyBase k z s)) (l (LexicalDKeyBase k z s)) (l (LexicalEKeyBase k z s)) (l (LexicalFKeyBase k z s)) (l (LexicalGKeyBase k z s)) (l (LexicalHKeyBase k z s)) (l (LexicalIKeyBase k z s)) (l (LexicalJKeyBase k z s)) (l (LexicalKKeyBase k z s)) (l (LexicalLKeyBase k z s)) (l (LexicalMKeyBase k z s)) (l (LexicalNKeyBase k z s)) (l (LexicalOKeyBase k z s)) (l (LexicalPKeyBase k z s)) (l (LexicalQKeyBase k z s)) (l (LexicalRKeyBase k z s)) (l (LexicalSKeyBase k z s)) (l (LexicalTKeyBase k z s)) (l (LexicalUKeyBase k z s)) (l (LexicalVKeyBase k z s)) (l (LexicalWKeyBase k z s)) (l (LexicalXKeyBase k z s)) (l (LexicalYKeyBase k z s)) (l (LexicalZKeyBase k z s)) annotation (StandardLinking.AscLarge k z s l lexicalAnnotation annotation)
-- | 'UniLargeBase' with fewer unresolved variables, with alias linking.
type UniLarge k z s l lexicalAnnotation annotation = UniLargeBase (l (LexicalUnicodeLargeKeyBase k z s)) annotation (StandardLinking.UniLarge k z s l lexicalAnnotation annotation)
-- | 'SymbolBase' with fewer unresolved variables, with alias linking.
type Symbol k z s l lexicalAnnotation annotation = SymbolBase (AscSymbol k z s l lexicalAnnotation lexicalAnnotation) (UniSymbolSansSpecialish k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.Symbol k z s l lexicalAnnotation annotation)
-- | 'SymbolSansBackslashBase' with fewer unresolved variables, with alias linking.
type SymbolSansBackslash k z s l lexicalAnnotation annotation = SymbolSansBackslashBase (AscSymbolSansBackslash k z s l lexicalAnnotation lexicalAnnotation) (UniSymbolSansSpecialish k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.SymbolSansBackslash k z s l lexicalAnnotation annotation)
-- | 'SymbolSansNcBase' with fewer unresolved variables, with alias linking.
type SymbolSansNc k z s l lexicalAnnotation annotation = SymbolSansNcBase (AscSymbolSansNc k z s l lexicalAnnotation lexicalAnnotation) (UniSymbolSansSpecialishSansNc k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.SymbolSansNc k z s l lexicalAnnotation annotation)
-- | 'AscSymbolBase' with fewer unresolved variables, with alias linking.
type AscSymbol k z s l lexicalAnnotation annotation = AscSymbolBase (l (LexicalExclamationKeyBase k z s)) (l (LexicalHashKeyBase k z s)) (l (LexicalDollarKeyBase k z s)) (l (LexicalPercentKeyBase k z s)) (l (LexicalAmpersandKeyBase k z s)) (l (LexicalAsteriskKeyBase k z s)) (l (LexicalPlusKeyBase k z s)) (l (LexicalDotKeyBase k z s)) (l (LexicalSlashKeyBase k z s)) (l (LexicalLeftAngleBracketKeyBase k z s)) (l (LexicalEqualsKeyBase k z s)) (l (LexicalRightAngleBracketKeyBase k z s)) (l (LexicalQuestionMarkKeyBase k z s)) (l (LexicalAtKeyBase k z s)) (l (LexicalBackslashKeyBase k z s)) (l (LexicalCaretKeyBase k z s)) (l (LexicalPipeKeyBase k z s)) (l (LexicalHyphenKeyBase k z s)) (l (LexicalTildeKeyBase k z s)) (l (LexicalColonKeyBase k z s)) annotation (StandardLinking.AscSymbol k z s l lexicalAnnotation annotation)
-- | 'AscSymbolSansBackslashBase' with fewer unresolved variables, with alias linking.
type AscSymbolSansBackslash k z s l lexicalAnnotation annotation = AscSymbolSansBackslashBase (l (LexicalExclamationKeyBase k z s)) (l (LexicalHashKeyBase k z s)) (l (LexicalDollarKeyBase k z s)) (l (LexicalPercentKeyBase k z s)) (l (LexicalAmpersandKeyBase k z s)) (l (LexicalAsteriskKeyBase k z s)) (l (LexicalPlusKeyBase k z s)) (l (LexicalDotKeyBase k z s)) (l (LexicalSlashKeyBase k z s)) (l (LexicalLeftAngleBracketKeyBase k z s)) (l (LexicalEqualsKeyBase k z s)) (l (LexicalRightAngleBracketKeyBase k z s)) (l (LexicalQuestionMarkKeyBase k z s)) (l (LexicalAtKeyBase k z s)) (l (LexicalCaretKeyBase k z s)) (l (LexicalPipeKeyBase k z s)) (l (LexicalHyphenKeyBase k z s)) (l (LexicalTildeKeyBase k z s)) (l (LexicalColonKeyBase k z s)) annotation (StandardLinking.AscSymbolSansBackslash k z s l lexicalAnnotation annotation)
-- | 'AscSymbolSansNcBase' with fewer unresolved variables, with alias linking.
type AscSymbolSansNc k z s l lexicalAnnotation annotation = AscSymbolSansNcBase (l (LexicalExclamationKeyBase k z s)) (l (LexicalHashKeyBase k z s)) (l (LexicalDollarKeyBase k z s)) (l (LexicalPercentKeyBase k z s)) (l (LexicalAmpersandKeyBase k z s)) (l (LexicalAsteriskKeyBase k z s)) (l (LexicalPlusKeyBase k z s)) (l (LexicalDotKeyBase k z s)) (l (LexicalSlashKeyBase k z s)) (l (LexicalLeftAngleBracketKeyBase k z s)) (l (LexicalEqualsKeyBase k z s)) (l (LexicalRightAngleBracketKeyBase k z s)) (l (LexicalQuestionMarkKeyBase k z s)) (l (LexicalAtKeyBase k z s)) (l (LexicalBackslashKeyBase k z s)) (l (LexicalCaretKeyBase k z s)) (l (LexicalPipeKeyBase k z s)) (l (LexicalTildeKeyBase k z s)) (l (LexicalColonKeyBase k z s)) annotation (StandardLinking.AscSymbolSansNc k z s l lexicalAnnotation annotation)
-- | 'UniSymbolBase' with fewer unresolved variables, with alias linking.
type UniSymbol k z s l lexicalAnnotation annotation = UniSymbolBase (l (LexicalUnicodeSymbolKeyBase k z s)) annotation (StandardLinking.UniSymbol k z s l lexicalAnnotation annotation)
-- | 'UniSymbolSansSpecialishBase' with fewer unresolved variables, with alias linking.
type UniSymbolSansSpecialish k z s l lexicalAnnotation annotation = UniSymbolSansSpecialishBase (l (LexicalUnicodeSymbolSansSpecialUnderscoreDoubleQuoteSingleQuoteKeyBase k z s)) annotation (StandardLinking.UniSymbolSansSpecialish k z s l lexicalAnnotation annotation)
-- | 'UniSymbolSansSpecialishSansNcBase' with fewer unresolved variables, with alias linking.
type UniSymbolSansSpecialishSansNc k z s l lexicalAnnotation annotation = UniSymbolSansSpecialishSansNcBase (l (LexicalUnicodeSymbolSansSpecialUnderscoreDoubleQuoteSingleQuoteKeyBase k z s)) annotation (StandardLinking.UniSymbolSansSpecialishSansNc k z s l lexicalAnnotation annotation)
-- | 'DigitBase' with fewer unresolved variables, with alias linking.
type Digit k z s l lexicalAnnotation annotation = DigitBase (AscDigit k z s l lexicalAnnotation lexicalAnnotation) (UniDigit k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.Digit k z s l lexicalAnnotation annotation)
-- | 'AscDigitBase' with fewer unresolved variables, with alias linking.
type AscDigit k z s l lexicalAnnotation annotation = AscDigitBase (l (Lexical0KeyBase k z s)) (l (Lexical1KeyBase k z s)) (l (Lexical2KeyBase k z s)) (l (Lexical3KeyBase k z s)) (l (Lexical4KeyBase k z s)) (l (Lexical5KeyBase k z s)) (l (Lexical6KeyBase k z s)) (l (Lexical7KeyBase k z s)) (l (Lexical8KeyBase k z s)) (l (Lexical9KeyBase k z s)) annotation (StandardLinking.AscDigit k z s l lexicalAnnotation annotation)
-- | 'UniDigitBase' with fewer unresolved variables, with alias linking.
type UniDigit k z s l lexicalAnnotation annotation = UniDigitBase (l (LexicalUnicodeDigitKeyBase k z s)) annotation (StandardLinking.UniDigit k z s l lexicalAnnotation annotation)
-- | 'OctitBase' with fewer unresolved variables, with alias linking.
type Octit k z s l lexicalAnnotation annotation = OctitBase (l (Lexical0KeyBase k z s)) (l (Lexical1KeyBase k z s)) (l (Lexical2KeyBase k z s)) (l (Lexical3KeyBase k z s)) (l (Lexical4KeyBase k z s)) (l (Lexical5KeyBase k z s)) (l (Lexical6KeyBase k z s)) (l (Lexical7KeyBase k z s)) annotation (StandardLinking.Octit k z s l lexicalAnnotation annotation)
-- | 'HexitBase' with fewer unresolved variables, with alias linking.
type Hexit k z s l lexicalAnnotation annotation = HexitBase (Digit k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalAKeyBase k z s)) (l (LexicalBKeyBase k z s)) (l (LexicalCKeyBase k z s)) (l (LexicalDKeyBase k z s)) (l (LexicalEKeyBase k z s)) (l (LexicalFKeyBase k z s)) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) annotation (StandardLinking.Hexit k z s l lexicalAnnotation annotation)

-- § 2.4 Identifiers and Operators types.

-- | 'UnqualifiedNameBase' with fewer unresolved variables, with alias linking.
type UnqualifiedName k z s l lexicalAnnotation annotation = UnqualifiedNameBase (Varid k z s l lexicalAnnotation lexicalAnnotation) (Conid k z s l lexicalAnnotation lexicalAnnotation) (Tyvar k z s l lexicalAnnotation lexicalAnnotation) (Tycon k z s l lexicalAnnotation lexicalAnnotation) (Tycls k z s l lexicalAnnotation lexicalAnnotation) (Modid k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.UnqualifiedName k z s l lexicalAnnotation annotation)
-- | 'VaridNoExclusionsBase' with fewer unresolved variables, with alias linking.
type VaridNoExclusions k z s l lexicalAnnotation annotation = VaridNoExclusionsBase [] (VaridStart k z s l lexicalAnnotation lexicalAnnotation) (VaridInner k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.VaridNoExclusions k z s l lexicalAnnotation annotation)
-- | 'VaridStartBase' with fewer unresolved variables, with alias linking.
type VaridStart k z s l lexicalAnnotation annotation = VaridStartBase (Small k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.VaridStart k z s l lexicalAnnotation annotation)
-- | 'IdentifierInnerBase' with fewer unresolved variables, with alias linking.
type IdentifierInner k z s l lexicalAnnotation annotation = IdentifierInnerBase (Small k z s l lexicalAnnotation lexicalAnnotation) (Large k z s l lexicalAnnotation lexicalAnnotation) (Digit k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalSingleQuoteKeyBase k z s)) annotation (StandardLinking.IdentifierInner k z s l lexicalAnnotation annotation)
-- | 'VaridInnerBase' with fewer unresolved variables, with alias linking.
type VaridInner k z s l lexicalAnnotation annotation = VaridInnerBase (IdentifierInner k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.VaridInner k z s l lexicalAnnotation annotation)
-- | 'ConidBase' with fewer unresolved variables, with alias linking.
type Conid k z s l lexicalAnnotation annotation = ConidBase [] (ConidStart k z s l lexicalAnnotation lexicalAnnotation) (ConidInner k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.Conid k z s l lexicalAnnotation annotation)
-- | 'ConidStartBase' with fewer unresolved variables, with alias linking.
type ConidStart k z s l lexicalAnnotation annotation = ConidStartBase (Large k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.ConidStart k z s l lexicalAnnotation annotation)
-- | 'ConidInnerBase' with fewer unresolved variables, with alias linking.
type ConidInner k z s l lexicalAnnotation annotation = ConidInnerBase (IdentifierInner k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.ConidInner k z s l lexicalAnnotation annotation)
-- | 'ReservedidBase' with fewer unresolved variables, with alias linking.
type Reservedid k z s l lexicalAnnotation annotation = ReservedidBase (LexicalCase k z s l lexicalAnnotation lexicalAnnotation) (LexicalClass k z s l lexicalAnnotation lexicalAnnotation) (LexicalData k z s l lexicalAnnotation lexicalAnnotation) (LexicalDefault k z s l lexicalAnnotation lexicalAnnotation) (LexicalDeriving k z s l lexicalAnnotation lexicalAnnotation) (LexicalDo k z s l lexicalAnnotation lexicalAnnotation) (LexicalElse k z s l lexicalAnnotation lexicalAnnotation) (LexicalForeign k z s l lexicalAnnotation lexicalAnnotation) (LexicalIf k z s l lexicalAnnotation lexicalAnnotation) (LexicalImport k z s l lexicalAnnotation lexicalAnnotation) (LexicalIn k z s l lexicalAnnotation lexicalAnnotation) (LexicalInfix k z s l lexicalAnnotation lexicalAnnotation) (LexicalInfixl k z s l lexicalAnnotation lexicalAnnotation) (LexicalInfixr k z s l lexicalAnnotation lexicalAnnotation) (LexicalInstance k z s l lexicalAnnotation lexicalAnnotation) (LexicalLet k z s l lexicalAnnotation lexicalAnnotation) (LexicalModule k z s l lexicalAnnotation lexicalAnnotation) (LexicalNewtype k z s l lexicalAnnotation lexicalAnnotation) (LexicalOf k z s l lexicalAnnotation lexicalAnnotation) (LexicalThen k z s l lexicalAnnotation lexicalAnnotation) (LexicalType k z s l lexicalAnnotation lexicalAnnotation) (LexicalWhere k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalUnderscoreKeyBase k z s)) annotation (StandardLinking.Reservedid k z s l lexicalAnnotation annotation)
-- | 'VarSymNoExtraExclusionsBase' with fewer unresolved variables, with alias linking.
type VarSymNoExtraExclusions k z s l lexicalAnnotation annotation = VarSymNoExtraExclusionsBase [] (VarSymStart k z s l lexicalAnnotation lexicalAnnotation) (Symbol k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.VarSymNoExtraExclusions k z s l lexicalAnnotation annotation)
-- | 'VarSymStartBase' with fewer unresolved variables, with alias linking.
type VarSymStart k z s l lexicalAnnotation annotation = VarSymStartBase (SymbolSansColon k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.VarSymStart k z s l lexicalAnnotation annotation)
-- | 'ConSymNoExtraExclusionsBase' with fewer unresolved variables, with alias linking.
type ConSymNoExtraExclusions k z s l lexicalAnnotation annotation = ConSymNoExtraExclusionsBase [] (ConSymStart k z s l lexicalAnnotation lexicalAnnotation) (Symbol k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.ConSymNoExtraExclusions k z s l lexicalAnnotation annotation)
-- | 'ConSymStartBase' with fewer unresolved variables, with alias linking.
type ConSymStart k z s l lexicalAnnotation annotation = ConSymStartBase (l (LexicalColonKeyBase k z s)) annotation (StandardLinking.ConSymStart k z s l lexicalAnnotation annotation)
-- | 'ReservedOpBase' with fewer unresolved variables, with alias linking.
type ReservedOp k z s l lexicalAnnotation annotation = ReservedOpBase (LexicalDotDot k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalColonKeyBase k z s)) (LexicalDoubleColon k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalEqualsKeyBase k z s)) (l (LexicalBackslashKeyBase k z s)) (l (LexicalPipeKeyBase k z s)) (LexicalLeftArrow k z s l lexicalAnnotation lexicalAnnotation) (LexicalRightArrow k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalAtKeyBase k z s)) (l (LexicalTildeKeyBase k z s)) (LexicalDoubleRightArrow k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.ReservedOp k z s l lexicalAnnotation annotation)
-- | 'TyvarBase' with fewer unresolved variables, with alias linking.
type Tyvar k z s l lexicalAnnotation annotation = TyvarBase (Conid k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.Tyvar k z s l lexicalAnnotation annotation)
-- | 'TyconBase' with fewer unresolved variables, with alias linking.
type Tycon k z s l lexicalAnnotation annotation = TyconBase (Varid k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.Tycon k z s l lexicalAnnotation annotation)
-- | 'TyclsBase' with fewer unresolved variables, with alias linking.
type Tycls k z s l lexicalAnnotation annotation = TyclsBase (Conid k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.Tycls k z s l lexicalAnnotation annotation)
-- | 'ModidBase' with fewer unresolved variables, with alias linking.
type Modid k z s l lexicalAnnotation annotation = ModidBase (,) [] (Conid k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalDotKeyBase k z s)) annotation (StandardLinking.Modid k z s l lexicalAnnotation annotation)
-- | 'NameBase' with fewer unresolved variables, with alias linking.
type Name k z s l lexicalAnnotation annotation = NameBase (Qvarid k z s l lexicalAnnotation lexicalAnnotation) (Qconid k z s l lexicalAnnotation lexicalAnnotation) (Qtycon k z s l lexicalAnnotation lexicalAnnotation) (Qtycls k z s l lexicalAnnotation lexicalAnnotation) (QvarSym k z s l lexicalAnnotation lexicalAnnotation) (QconSym k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.Name k z s l lexicalAnnotation annotation)
-- | 'QvaridBase' with fewer unresolved variables, with alias linking.
type Qvarid k z s l lexicalAnnotation annotation = QvaridBase (,) Prelude.Maybe (Modid k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalDotKeyBase k z s)) (Varid k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.Qvarid k z s l lexicalAnnotation annotation)
-- | 'QconidBase' with fewer unresolved variables, with alias linking.
type Qconid k z s l lexicalAnnotation annotation = QconidBase (,) Prelude.Maybe (Modid k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalDotKeyBase k z s)) (Conid k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.Qconid k z s l lexicalAnnotation annotation)
-- | 'QtyconBase' with fewer unresolved variables, with alias linking.
type Qtycon k z s l lexicalAnnotation annotation = QtyconBase (,) Prelude.Maybe (Modid k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalDotKeyBase k z s)) (Tycon k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.Qtycon k z s l lexicalAnnotation annotation)
-- | 'QtyclsBase' with fewer unresolved variables, with alias linking.
type Qtycls k z s l lexicalAnnotation annotation = QtyclsBase (,) Prelude.Maybe (Modid k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalDotKeyBase k z s)) (Tycls k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.Qtycls k z s l lexicalAnnotation annotation)
-- | 'QvarSymBase' with fewer unresolved variables, with alias linking.
type QvarSym k z s l lexicalAnnotation annotation = QvarSymBase (,) Prelude.Maybe (Modid k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalDotKeyBase k z s)) (VarSym k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.QvarSym k z s l lexicalAnnotation annotation)
-- | 'QvarSymSansMinusBase' with fewer unresolved variables, with alias linking.
type QvarSymSansMinus k z s l lexicalAnnotation annotation = QvarSymSansMinusBase (,) Prelude.Maybe (Modid k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalDotKeyBase k z s)) (VarSymSansMinus k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.QvarSymSansMinus k z s l lexicalAnnotation annotation)
-- | 'QconSymBase' with fewer unresolved variables, with alias linking.
type QconSym k z s l lexicalAnnotation annotation = QconSymBase (,) Prelude.Maybe (Modid k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalDotKeyBase k z s)) (ConSym k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.QconSym k z s l lexicalAnnotation annotation)

-- § 2.5 Numeric Literals types.

-- | 'DecimalBase' with fewer unresolved variables, with alias linking.
type Decimal k z s l lexicalAnnotation annotation = DecimalBase [] (Digit k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.Decimal k z s l lexicalAnnotation annotation)
-- | 'OctalBase' with fewer unresolved variables, with alias linking.
type Octal k z s l lexicalAnnotation annotation = OctalBase [] (Octit k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.Octal k z s l lexicalAnnotation annotation)
-- | 'HexadecimalBase' with fewer unresolved variables, with alias linking.
type Hexadecimal k z s l lexicalAnnotation annotation = HexadecimalBase [] (Hexit k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.Hexadecimal k z s l lexicalAnnotation annotation)
-- | 'IntegerBase' with fewer unresolved variables, with alias linking.
type Integer k z s l lexicalAnnotation annotation = IntegerBase (Decimal k z s l lexicalAnnotation lexicalAnnotation) (Lexical0o k z s l lexicalAnnotation lexicalAnnotation) (Octal k z s l lexicalAnnotation lexicalAnnotation) (Lexical0O k z s l lexicalAnnotation lexicalAnnotation) (Lexical0x k z s l lexicalAnnotation lexicalAnnotation) (Hexadecimal k z s l lexicalAnnotation lexicalAnnotation) (Lexical0X k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.Integer k z s l lexicalAnnotation annotation)
-- | 'FloatBase' with fewer unresolved variables, with alias linking.
type Float k z s l lexicalAnnotation annotation = FloatBase Prelude.Maybe (Decimal k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalDotKeyBase k z s)) (Exponent k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.Float k z s l lexicalAnnotation annotation)
-- | 'ExponentBase' with fewer unresolved variables, with alias linking.
type Exponent k z s l lexicalAnnotation annotation = ExponentBase Prelude.Either Prelude.Maybe (l (LexicalELowerKeyBase k z s)) (l (LexicalEKeyBase k z s)) (l (LexicalPlusKeyBase k z s)) (LexicalMinus k z s l lexicalAnnotation lexicalAnnotation) (Decimal k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.Exponent k z s l lexicalAnnotation annotation)

-- § 2.6 Character and String Literals types.

-- | 'CharBase' with fewer unresolved variables, with alias linking.
type Char k z s l lexicalAnnotation annotation = CharBase (l (LexicalSingleQuoteKeyBase k z s)) (CharLiteralInner k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.Char k z s l lexicalAnnotation annotation)
-- | 'CharLiteralInnerBase' with fewer unresolved variables, with alias linking.
type CharLiteralInner k z s l lexicalAnnotation annotation = CharLiteralInnerBase (GraphicSansSingleQuoteOrBackslash k z s l lexicalAnnotation lexicalAnnotation) (Space k z s l lexicalAnnotation lexicalAnnotation) (EscapeSansBackslashAndAmpersand k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.CharLiteralInner k z s l lexicalAnnotation annotation)
-- | 'StringBase' with fewer unresolved variables, with alias linking.
type String k z s l lexicalAnnotation annotation = StringBase [] (l (LexicalDoubleQuoteKeyBase k z s)) (StringLiteralInnerUnit k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.String k z s l lexicalAnnotation annotation)
-- | 'StringLiteralInnerUnitBase' with fewer unresolved variables, with alias linking.
type StringLiteralInnerUnit k z s l lexicalAnnotation annotation = StringLiteralInnerUnitBase (GraphicSansDoubleQuoteOrBackslash k z s l lexicalAnnotation lexicalAnnotation) (Space k z s l lexicalAnnotation lexicalAnnotation) (Escape k z s l lexicalAnnotation lexicalAnnotation) (Gap k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.StringLiteralInnerUnit k z s l lexicalAnnotation annotation)
-- | 'EscapeBase' with fewer unresolved variables, with alias linking.
type Escape k z s l lexicalAnnotation annotation = EscapeBase (l (LexicalBackslashKeyBase k z s)) (EscapeInner k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.Escape k z s l lexicalAnnotation annotation)
-- | 'EscapeSansBackslashAndAmpersandBase' with fewer unresolved variables, with alias linking.
type EscapeSansBackslashAndAmpersand k z s l lexicalAnnotation annotation = EscapeSansBackslashAndAmpersandBase (l (LexicalBackslashKeyBase k z s)) (EscapeInnerSansAmpersand k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.EscapeSansBackslashAndAmpersand k z s l lexicalAnnotation annotation)
-- | 'EscapeInnerBase' with fewer unresolved variables, with alias linking.
type EscapeInner k z s l lexicalAnnotation annotation = EscapeInnerBase (CharEsc k z s l lexicalAnnotation lexicalAnnotation) (Ascii k z s l lexicalAnnotation lexicalAnnotation) (Decimal k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalOLowerKeyBase k z s)) (Octal k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalXLowerKeyBase k z s)) (Hexadecimal k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.EscapeInner k z s l lexicalAnnotation annotation)
-- | 'EscapeInnerSansAmpersandBase' with fewer unresolved variables, with alias linking.
type EscapeInnerSansAmpersand k z s l lexicalAnnotation annotation = EscapeInnerSansAmpersandBase (CharEscSansAmpersand k z s l lexicalAnnotation lexicalAnnotation) (Ascii k z s l lexicalAnnotation lexicalAnnotation) (Decimal k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalOLowerKeyBase k z s)) (Octal k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalXLowerKeyBase k z s)) (Hexadecimal k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.EscapeInnerSansAmpersand k z s l lexicalAnnotation annotation)
-- | 'CharEscBase' with fewer unresolved variables, with alias linking.
type CharEsc k z s l lexicalAnnotation annotation = CharEscBase (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalBackslashKeyBase k z s)) (l (LexicalDoubleQuoteKeyBase k z s)) (l (LexicalSingleQuoteKeyBase k z s)) (l (LexicalAmpersandKeyBase k z s)) annotation (StandardLinking.CharEsc k z s l lexicalAnnotation annotation)
-- | 'CharEscSansAmpersandBase' with fewer unresolved variables, with alias linking.
type CharEscSansAmpersand k z s l lexicalAnnotation annotation = CharEscSansAmpersandBase (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalBackslashKeyBase k z s)) (l (LexicalDoubleQuoteKeyBase k z s)) (l (LexicalSingleQuoteKeyBase k z s)) annotation (StandardLinking.CharEscSansAmpersand k z s l lexicalAnnotation annotation)
-- | 'AsciiBase' with fewer unresolved variables, with alias linking.
type Ascii k z s l lexicalAnnotation annotation = AsciiBase (l (LexicalCaretKeyBase k z s)) (Cntrl k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalNULKeyBase k z s)) (l (LexicalSOHKeyBase k z s)) (l (LexicalSTXKeyBase k z s)) (l (LexicalETXKeyBase k z s)) (l (LexicalEOTKeyBase k z s)) (l (LexicalENQKeyBase k z s)) (l (LexicalACKKeyBase k z s)) (l (LexicalBELKeyBase k z s)) (l (LexicalBSKeyBase k z s)) (l (LexicalHTKeyBase k z s)) (l (LexicalLFKeyBase k z s)) (l (LexicalVTKeyBase k z s)) (l (LexicalFFKeyBase k z s)) (l (LexicalCRKeyBase k z s)) (l (LexicalSOKeyBase k z s)) (l (LexicalSIKeyBase k z s)) (l (LexicalDLEKeyBase k z s)) (l (LexicalDC1KeyBase k z s)) (l (LexicalDC2KeyBase k z s)) (l (LexicalDC3KeyBase k z s)) (l (LexicalDC4KeyBase k z s)) (l (LexicalNAKKeyBase k z s)) (l (LexicalSYNKeyBase k z s)) (l (LexicalETBKeyBase k z s)) (l (LexicalCANKeyBase k z s)) (l (LexicalEMKeyBase k z s)) (l (LexicalSUBKeyBase k z s)) (l (LexicalESCKeyBase k z s)) (l (LexicalFSKeyBase k z s)) (l (LexicalGSKeyBase k z s)) (l (LexicalRSKeyBase k z s)) (l (LexicalUSKeyBase k z s)) (LexicalSpace k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalDELKeyBase k z s)) annotation (StandardLinking.Ascii k z s l lexicalAnnotation annotation)
-- | 'CntrlBase' with fewer unresolved variables, with alias linking.
type Cntrl k z s l lexicalAnnotation annotation = CntrlBase (AscLarge k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalAtKeyBase k z s)) (l (LexicalLeftBracketKeyBase k z s)) (l (LexicalBackslashKeyBase k z s)) (l (LexicalRightBracketKeyBase k z s)) (l (LexicalCaretKeyBase k z s)) (l (LexicalUnderscoreKeyBase k z s)) annotation (StandardLinking.Cntrl k z s l lexicalAnnotation annotation)
-- | 'GapBase' with fewer unresolved variables, with alias linking.
type Gap k z s l lexicalAnnotation annotation = GapBase [] (l (LexicalBackslashKeyBase k z s)) (Whitechar k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.Gap k z s l lexicalAnnotation annotation)

-- § 8.3 (FFI) Lexical Structure types.

-- | 'ChnameBase' with fewer unresolved variables, with alias linking.
type Chname k z s l lexicalAnnotation annotation = ChnameBase [] (Chchar k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalDotKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) annotation (StandardLinking.Chname k z s l lexicalAnnotation annotation)
-- | 'CidBase' with fewer unresolved variables, with alias linking.
type Cid k z s l lexicalAnnotation annotation = CidBase [] Prelude.Either (Letter k z s l lexicalAnnotation lexicalAnnotation) (AscDigit k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.Cid k z s l lexicalAnnotation annotation)
-- | 'ChcharBase' with fewer unresolved variables, with alias linking.
type Chchar k z s l lexicalAnnotation annotation = ChcharBase Prelude.Either (Letter k z s l lexicalAnnotation lexicalAnnotation) (AscSymbolSansAmpersand k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.Chchar k z s l lexicalAnnotation annotation)
-- | 'LetterBase' with fewer unresolved variables, with alias linking.
type Letter k z s l lexicalAnnotation annotation = LetterBase (AscSmall k z s l lexicalAnnotation lexicalAnnotation) (AscLarge k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalUnderscoreKeyBase k z s)) annotation (StandardLinking.Letter k z s l lexicalAnnotation annotation)
-- | 'AscSymbolSansAmpersandBase' with fewer unresolved variables, with alias linking.
type AscSymbolSansAmpersand k z s l lexicalAnnotation annotation = AscSymbolSansAmpersandBase (l (LexicalExclamationKeyBase k z s)) (l (LexicalHashKeyBase k z s)) (l (LexicalDollarKeyBase k z s)) (l (LexicalPercentKeyBase k z s)) (l (LexicalAsteriskKeyBase k z s)) (l (LexicalPlusKeyBase k z s)) (l (LexicalDotKeyBase k z s)) (l (LexicalSlashKeyBase k z s)) (l (LexicalLeftAngleBracketKeyBase k z s)) (l (LexicalEqualsKeyBase k z s)) (l (LexicalRightAngleBracketKeyBase k z s)) (l (LexicalQuestionMarkKeyBase k z s)) (l (LexicalAtKeyBase k z s)) (l (LexicalBackslashKeyBase k z s)) (l (LexicalCaretKeyBase k z s)) (l (LexicalPipeKeyBase k z s)) (l (LexicalHyphenKeyBase k z s)) (l (LexicalTildeKeyBase k z s)) (l (LexicalColonKeyBase k z s)) annotation (StandardLinking.AscSymbolSansAmpersand k z s l lexicalAnnotation annotation)

-- Base lexical structures.

-- Pseudo-foundational lexical structures.

-- | 'LexicalPseudoBase' with fewer unresolved variables, with alias linking.
type LexicalPseudo k z s l lexicalAnnotation annotation = LexicalPseudoBase (LexicalNonsymKeyword k z s l lexicalAnnotation lexicalAnnotation) (LexicalNonsymNonkeyword k z s l lexicalAnnotation lexicalAnnotation) (LexicalSymAlias k z s l lexicalAnnotation lexicalAnnotation) (LexicalAlias k z s l lexicalAnnotation lexicalAnnotation) (LexicalNumPrefix k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.LexicalPseudo k z s l lexicalAnnotation annotation)

-- Non-symbolic keyword pseudo-lexical structures.

-- | 'LexicalNonsymKeywordBase' with fewer unresolved variables, with alias linking.
type LexicalNonsymKeyword k z s l lexicalAnnotation annotation = LexicalNonsymKeywordBase (LexicalCase k z s l lexicalAnnotation lexicalAnnotation) (LexicalClass k z s l lexicalAnnotation lexicalAnnotation) (LexicalData k z s l lexicalAnnotation lexicalAnnotation) (LexicalDefault k z s l lexicalAnnotation lexicalAnnotation) (LexicalDeriving k z s l lexicalAnnotation lexicalAnnotation) (LexicalDo k z s l lexicalAnnotation lexicalAnnotation) (LexicalElse k z s l lexicalAnnotation lexicalAnnotation) (LexicalForeign k z s l lexicalAnnotation lexicalAnnotation) (LexicalIf k z s l lexicalAnnotation lexicalAnnotation) (LexicalImport k z s l lexicalAnnotation lexicalAnnotation) (LexicalIn k z s l lexicalAnnotation lexicalAnnotation) (LexicalInfix k z s l lexicalAnnotation lexicalAnnotation) (LexicalInfixl k z s l lexicalAnnotation lexicalAnnotation) (LexicalInfixr k z s l lexicalAnnotation lexicalAnnotation) (LexicalInstance k z s l lexicalAnnotation lexicalAnnotation) (LexicalLet k z s l lexicalAnnotation lexicalAnnotation) (LexicalModule k z s l lexicalAnnotation lexicalAnnotation) (LexicalNewtype k z s l lexicalAnnotation lexicalAnnotation) (LexicalOf k z s l lexicalAnnotation lexicalAnnotation) (LexicalThen k z s l lexicalAnnotation lexicalAnnotation) (LexicalType k z s l lexicalAnnotation lexicalAnnotation) (LexicalWhere k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.LexicalNonsymKeyword k z s l lexicalAnnotation annotation)
-- | 'LexicalCaseBase' with fewer unresolved variables, with alias linking.
type LexicalCase k z s l lexicalAnnotation annotation = LexicalCaseBase (l (LexicalCLowerKeyBase k z s)) (l (LexicalALowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) annotation (StandardLinking.LexicalCase k z s l lexicalAnnotation annotation)
-- | 'LexicalClassBase' with fewer unresolved variables, with alias linking.
type LexicalClass k z s l lexicalAnnotation annotation = LexicalClassBase (l (LexicalCLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalALowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) annotation (StandardLinking.LexicalClass k z s l lexicalAnnotation annotation)
-- | 'LexicalDataBase' with fewer unresolved variables, with alias linking.
type LexicalData k z s l lexicalAnnotation annotation = LexicalDataBase (l (LexicalDLowerKeyBase k z s)) (l (LexicalALowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) annotation (StandardLinking.LexicalData k z s l lexicalAnnotation annotation)
-- | 'LexicalDefaultBase' with fewer unresolved variables, with alias linking.
type LexicalDefault k z s l lexicalAnnotation annotation = LexicalDefaultBase (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalALowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) annotation (StandardLinking.LexicalDefault k z s l lexicalAnnotation annotation)
-- | 'LexicalDerivingBase' with fewer unresolved variables, with alias linking.
type LexicalDeriving k z s l lexicalAnnotation annotation = LexicalDerivingBase (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) annotation (StandardLinking.LexicalDeriving k z s l lexicalAnnotation annotation)
-- | 'LexicalDoBase' with fewer unresolved variables, with alias linking.
type LexicalDo k z s l lexicalAnnotation annotation = LexicalDoBase (l (LexicalDLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) annotation (StandardLinking.LexicalDo k z s l lexicalAnnotation annotation)
-- | 'LexicalElseBase' with fewer unresolved variables, with alias linking.
type LexicalElse k z s l lexicalAnnotation annotation = LexicalElseBase (l (LexicalELowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) annotation (StandardLinking.LexicalElse k z s l lexicalAnnotation annotation)
-- | 'LexicalForeignBase' with fewer unresolved variables, with alias linking.
type LexicalForeign k z s l lexicalAnnotation annotation = LexicalForeignBase (l (LexicalFLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) annotation (StandardLinking.LexicalForeign k z s l lexicalAnnotation annotation)
-- | 'LexicalIfBase' with fewer unresolved variables, with alias linking.
type LexicalIf k z s l lexicalAnnotation annotation = LexicalIfBase (l (LexicalILowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) annotation (StandardLinking.LexicalIf k z s l lexicalAnnotation annotation)
-- | 'LexicalImportBase' with fewer unresolved variables, with alias linking.
type LexicalImport k z s l lexicalAnnotation annotation = LexicalImportBase (l (LexicalILowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) annotation (StandardLinking.LexicalImport k z s l lexicalAnnotation annotation)
-- | 'LexicalInBase' with fewer unresolved variables, with alias linking.
type LexicalIn k z s l lexicalAnnotation annotation = LexicalInBase (l (LexicalILowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) annotation (StandardLinking.LexicalIn k z s l lexicalAnnotation annotation)
-- | 'LexicalInfixBase' with fewer unresolved variables, with alias linking.
type LexicalInfix k z s l lexicalAnnotation annotation = LexicalInfixBase (l (LexicalILowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) annotation (StandardLinking.LexicalInfix k z s l lexicalAnnotation annotation)
-- | 'LexicalInfixlBase' with fewer unresolved variables, with alias linking.
type LexicalInfixl k z s l lexicalAnnotation annotation = LexicalInfixlBase (l (LexicalILowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) annotation (StandardLinking.LexicalInfixl k z s l lexicalAnnotation annotation)
-- | 'LexicalInfixrBase' with fewer unresolved variables, with alias linking.
type LexicalInfixr k z s l lexicalAnnotation annotation = LexicalInfixrBase (l (LexicalILowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) annotation (StandardLinking.LexicalInfixr k z s l lexicalAnnotation annotation)
-- | 'LexicalInstanceBase' with fewer unresolved variables, with alias linking.
type LexicalInstance k z s l lexicalAnnotation annotation = LexicalInstanceBase (l (LexicalILowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalALowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) annotation (StandardLinking.LexicalInstance k z s l lexicalAnnotation annotation)
-- | 'LexicalLetBase' with fewer unresolved variables, with alias linking.
type LexicalLet k z s l lexicalAnnotation annotation = LexicalLetBase (l (LexicalLLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) annotation (StandardLinking.LexicalLet k z s l lexicalAnnotation annotation)
-- | 'LexicalModuleBase' with fewer unresolved variables, with alias linking.
type LexicalModule k z s l lexicalAnnotation annotation = LexicalModuleBase (l (LexicalMLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) annotation (StandardLinking.LexicalModule k z s l lexicalAnnotation annotation)
-- | 'LexicalNewtypeBase' with fewer unresolved variables, with alias linking.
type LexicalNewtype k z s l lexicalAnnotation annotation = LexicalNewtypeBase (l (LexicalNLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) annotation (StandardLinking.LexicalNewtype k z s l lexicalAnnotation annotation)
-- | 'LexicalOfBase' with fewer unresolved variables, with alias linking.
type LexicalOf k z s l lexicalAnnotation annotation = LexicalOfBase (l (LexicalOLowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) annotation (StandardLinking.LexicalOf k z s l lexicalAnnotation annotation)
-- | 'LexicalThenBase' with fewer unresolved variables, with alias linking.
type LexicalThen k z s l lexicalAnnotation annotation = LexicalThenBase (l (LexicalTLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) annotation (StandardLinking.LexicalThen k z s l lexicalAnnotation annotation)
-- | 'LexicalTypeBase' with fewer unresolved variables, with alias linking.
type LexicalType k z s l lexicalAnnotation annotation = LexicalTypeBase (l (LexicalTLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) annotation (StandardLinking.LexicalType k z s l lexicalAnnotation annotation)
-- | 'LexicalWhereBase' with fewer unresolved variables, with alias linking.
type LexicalWhere k z s l lexicalAnnotation annotation = LexicalWhereBase (l (LexicalWLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) annotation (StandardLinking.LexicalWhere k z s l lexicalAnnotation annotation)

-- Non-symbolic non-keyword pseudo-lexical structures.

-- | 'LexicalNonsymNonkeywordBase' with fewer unresolved variables, with alias linking.
type LexicalNonsymNonkeyword k z s l lexicalAnnotation annotation = LexicalNonsymNonkeywordBase (LexicalAs k z s l lexicalAnnotation lexicalAnnotation) (LexicalHiding k z s l lexicalAnnotation lexicalAnnotation) (LexicalQualified k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.LexicalNonsymNonkeyword k z s l lexicalAnnotation annotation)
-- | 'LexicalAsBase' with fewer unresolved variables, with alias linking.
type LexicalAs k z s l lexicalAnnotation annotation = LexicalAsBase (l (LexicalALowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) annotation (StandardLinking.LexicalAs k z s l lexicalAnnotation annotation)
-- | 'LexicalHidingBase' with fewer unresolved variables, with alias linking.
type LexicalHiding k z s l lexicalAnnotation annotation = LexicalHidingBase (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) annotation (StandardLinking.LexicalHiding k z s l lexicalAnnotation annotation)
-- | 'LexicalQualifiedBase' with fewer unresolved variables, with alias linking.
type LexicalQualified k z s l lexicalAnnotation annotation = LexicalQualifiedBase (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) annotation (StandardLinking.LexicalQualified k z s l lexicalAnnotation annotation)

-- Symbolic alias pseudo-lexical structures.

-- | 'LexicalSymAliasBase' with fewer unresolved variables, with alias linking.
type LexicalSymAlias k z s l lexicalAnnotation annotation = LexicalSymAliasBase (LexicalDotDot k z s l lexicalAnnotation lexicalAnnotation) (LexicalDoubleColon k z s l lexicalAnnotation lexicalAnnotation) (LexicalDoubleRightArrow k z s l lexicalAnnotation lexicalAnnotation) (LexicalLeftArrow k z s l lexicalAnnotation lexicalAnnotation) (LexicalRightArrow k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.LexicalSymAlias k z s l lexicalAnnotation annotation)

-- | 'LexicalDotDotArrowBase' with fewer unresolved variables, with alias linking.
type LexicalDotDot k z s l lexicalAnnotation annotation = LexicalDotDotBase (l (LexicalDotKeyBase k z s)) annotation (StandardLinking.LexicalDotDot k z s l lexicalAnnotation annotation)

-- | 'LexicalDoubleColonArrowBase' with fewer unresolved variables, with alias linking.
type LexicalDoubleColon k z s l lexicalAnnotation annotation = LexicalDoubleColonBase (l (LexicalColonKeyBase k z s)) annotation (StandardLinking.LexicalDoubleColon k z s l lexicalAnnotation annotation)

-- | 'LexicalDoubleRightArrowBase' with fewer unresolved variables, with alias linking.
type LexicalDoubleRightArrow k z s l lexicalAnnotation annotation = LexicalDoubleRightArrowBase (l (LexicalEqualsKeyBase k z s)) (l (LexicalRightAngleBracketKeyBase k z s)) annotation (StandardLinking.LexicalDoubleRightArrow k z s l lexicalAnnotation annotation)

-- | 'LexicalLeftArrowBase' with fewer unresolved variables, with alias linking.
type LexicalLeftArrow k z s l lexicalAnnotation annotation = LexicalLeftArrowBase (l (LexicalLeftAngleBracketKeyBase k z s)) (l (LexicalHyphenKeyBase k z s)) annotation (StandardLinking.LexicalLeftArrow k z s l lexicalAnnotation annotation)

-- | 'LexicalRightArrowBase' with fewer unresolved variables, with alias linking.
type LexicalRightArrow k z s l lexicalAnnotation annotation = LexicalRightArrowBase (l (LexicalHyphenKeyBase k z s)) (l (LexicalRightAngleBracketKeyBase k z s)) annotation (StandardLinking.LexicalRightArrow k z s l lexicalAnnotation annotation)

-- Alias pseudo-lexical structures.

-- | 'LexicalAliasBase' with fewer unresolved variables, with alias linking.
type LexicalAlias k z s l lexicalAnnotation annotation = LexicalAliasBase (LexicalSpace k z s l lexicalAnnotation lexicalAnnotation) (LexicalMinus k z s l lexicalAnnotation lexicalAnnotation) (LexicalAsciiLambda k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.LexicalAlias k z s l lexicalAnnotation annotation)

-- | 'LexicalSpaceBase' with fewer unresolved variables, with alias linking.
type LexicalSpace k z s l lexicalAnnotation annotation = LexicalSpaceBase (l (LexicalSPKeyBase k z s)) annotation (StandardLinking.LexicalSpace k z s l lexicalAnnotation annotation)

-- | 'LexicalMinusBase' with fewer unresolved variables, with alias linking.
type LexicalMinus k z s l lexicalAnnotation annotation = LexicalMinusBase (l (LexicalHyphenKeyBase k z s)) annotation (StandardLinking.LexicalMinus k z s l lexicalAnnotation annotation)

-- | 'LexicalAsciiLambdaBase' with fewer unresolved variables, with alias linking.
type LexicalAsciiLambda k z s l lexicalAnnotation annotation = LexicalAsciiLambdaBase (l (LexicalBackslashKeyBase k z s)) annotation (StandardLinking.LexicalAsciiLambda k z s l lexicalAnnotation annotation)

-- Non-symbolic numeric literal prefix pseudo-lexical structures.

-- | 'LexicalNumPrefixBase' with fewer unresolved variables, with alias linking.
type LexicalNumPrefix k z s l lexicalAnnotation annotation = LexicalNumPrefixBase (Lexical0o k z s l lexicalAnnotation lexicalAnnotation) (Lexical0O k z s l lexicalAnnotation lexicalAnnotation) (Lexical0x k z s l lexicalAnnotation lexicalAnnotation) (Lexical0X k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.LexicalNumPrefix k z s l lexicalAnnotation annotation)

-- | 'Lexical0oBase' with fewer unresolved variables, with alias linking.
type Lexical0o k z s l lexicalAnnotation annotation = Lexical0oBase (l (Lexical0KeyBase k z s)) (l (LexicalXKeyBase k z s)) annotation (StandardLinking.Lexical0o k z s l lexicalAnnotation annotation)

-- | 'Lexical0OBase' with fewer unresolved variables, with alias linking.
type Lexical0O k z s l lexicalAnnotation annotation = Lexical0OBase (l (Lexical0KeyBase k z s)) (l (LexicalXKeyBase k z s)) annotation (StandardLinking.Lexical0O k z s l lexicalAnnotation annotation)

-- | 'Lexical0xBase' with fewer unresolved variables, with alias linking.
type Lexical0x k z s l lexicalAnnotation annotation = Lexical0xBase (l (Lexical0KeyBase k z s)) (l (LexicalXKeyBase k z s)) annotation (StandardLinking.Lexical0x k z s l lexicalAnnotation annotation)

-- | 'Lexical0XBase' with fewer unresolved variables, with alias linking.
type Lexical0X k z s l lexicalAnnotation annotation = Lexical0XBase (l (Lexical0KeyBase k z s)) (l (LexicalXKeyBase k z s)) annotation (StandardLinking.Lexical0X k z s l lexicalAnnotation annotation)

-- FFI pseudo-lexical structures.

{-
-- | 'LexicalImportBase' with fewer unresolved variables, with alias linking.
type LexicalImport k z s l lexicalAnnotation annotation = LexicalImportBase (l (LexicalILowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) annotation (StandardLinking.LexicalImport k z s l lexicalAnnotation annotation)
-}
-- | 'LexicalExportBase' with fewer unresolved variables, with alias linking.
type LexicalExport k z s l lexicalAnnotation annotation = LexicalExportBase (l (LexicalELowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) annotation (StandardLinking.LexicalExport k z s l lexicalAnnotation annotation)
-- | 'LexicalCcallBase' with fewer unresolved variables, with alias linking.
type LexicalCcall k z s l lexicalAnnotation annotation = LexicalCcallBase (l (LexicalCLowerKeyBase k z s)) (l (LexicalALowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) annotation (StandardLinking.LexicalCcall k z s l lexicalAnnotation annotation)
-- | 'LexicalStdcallBase' with fewer unresolved variables, with alias linking.
type LexicalStdcall k z s l lexicalAnnotation annotation = LexicalStdcallBase (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalALowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) annotation (StandardLinking.LexicalStdcall k z s l lexicalAnnotation annotation)
-- | 'LexicalCplusplusBase' with fewer unresolved variables, with alias linking.
type LexicalCplusplus k z s l lexicalAnnotation annotation = LexicalCplusplusBase (l (LexicalCLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) annotation (StandardLinking.LexicalCplusplus k z s l lexicalAnnotation annotation)
-- | 'LexicalJvmBase' with fewer unresolved variables, with alias linking.
type LexicalJvm k z s l lexicalAnnotation annotation = LexicalJvmBase (l (LexicalJLowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) annotation (StandardLinking.LexicalJvm k z s l lexicalAnnotation annotation)
-- | 'LexicalDotnetBase' with fewer unresolved variables, with alias linking.
type LexicalDotnet k z s l lexicalAnnotation annotation = LexicalDotnetBase (l (LexicalDLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) annotation (StandardLinking.LexicalDotnet k z s l lexicalAnnotation annotation)
-- | 'LexicalUnsafeBase' with fewer unresolved variables, with alias linking.
type LexicalUnsafe k z s l lexicalAnnotation annotation = LexicalUnsafeBase (l (LexicalULowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalALowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) annotation (StandardLinking.LexicalUnsafe k z s l lexicalAnnotation annotation)
-- | 'LexicalSafeBase' with fewer unresolved variables, with alias linking.
type LexicalSafe k z s l lexicalAnnotation annotation = LexicalSafeBase (l (LexicalSLowerKeyBase k z s)) (l (LexicalALowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) annotation (StandardLinking.LexicalSafe k z s l lexicalAnnotation annotation)
-- | 'LexicalStaticBase' with fewer unresolved variables, with alias linking.
type LexicalStatic k z s l lexicalAnnotation annotation = LexicalStaticBase (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalALowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) annotation (StandardLinking.LexicalStatic k z s l lexicalAnnotation annotation)
-- | 'LexicalDynamicBase' with fewer unresolved variables, with alias linking.
type LexicalDynamic k z s l lexicalAnnotation annotation = LexicalDynamicBase (l (LexicalDLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalALowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) annotation (StandardLinking.LexicalDynamic k z s l lexicalAnnotation annotation)
-- | 'LexicalWrapperBase' with fewer unresolved variables, with alias linking.
type LexicalWrapper k z s l lexicalAnnotation annotation = LexicalWrapperBase (l (LexicalWLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalALowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) annotation (StandardLinking.LexicalWrapper k z s l lexicalAnnotation annotation)

-- Grammatical structures.

-- § 5.1 Module Structure types.

-- | 'ModuleBase' with fewer unresolved variables, with alias linking.
type Module k z s l lexicalAnnotation grammarAnnotation annotation = ModuleBase Prelude.Maybe (LexicalModule k z s l lexicalAnnotation lexicalAnnotation) (Modid k z s l lexicalAnnotation lexicalAnnotation) (Exports k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (LexicalWhere k z s l lexicalAnnotation lexicalAnnotation) (Body k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) annotation (StandardLinking.Module k z s l lexicalAnnotation grammarAnnotation annotation)
-- | 'BodyBase' with fewer unresolved variables, with alias linking.
type Body k z s l lexicalAnnotation grammarAnnotation annotation = BodyBase (l (LexicalLeftBraceKeyBase k z s)) (ImpDecls k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (l (LexicalSemicolonKeyBase k z s)) (TopDecls k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (l (LexicalRightBraceKeyBase k z s)) annotation (StandardLinking.Body k z s l lexicalAnnotation grammarAnnotation annotation)
-- | 'ImpDeclsBase' with fewer unresolved variables, with alias linking.
type ImpDecls k z s l lexicalAnnotation grammarAnnotation annotation = ImpDeclsBase (,) [] (ImpDecl k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (l (LexicalSemicolonKeyBase k z s)) annotation (StandardLinking.ImpDecls k z s l lexicalAnnotation grammarAnnotation annotation)
-- | 'TopDeclsBase' with fewer unresolved variables, with alias linking.
type TopDecls k z s l lexicalAnnotation grammarAnnotation annotation = TopDeclsBase (,) [] (TopDecl k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (l (LexicalSemicolonKeyBase k z s)) annotation (StandardLinking.TopDecls k z s l lexicalAnnotation grammarAnnotation annotation)

-- § 5.2 Export Lists types.

-- | 'ExportsBase' with fewer unresolved variables, with alias linking.
type Exports k z s l lexicalAnnotation grammarAnnotation annotation = ExportsBase (,) Prelude.Maybe [] (l (LexicalLeftParenthesisKeyBase k z s)) (Export k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (l (LexicalCommaKeyBase k z s)) (l (LexicalRightParenthesisKeyBase k z s)) annotation (StandardLinking.Exports k z s l lexicalAnnotation grammarAnnotation annotation)
-- | 'ExportBase' with fewer unresolved variables, with alias linking.
type Export k z s l lexicalAnnotation grammarAnnotation annotation = ExportBase (,) (,,) Prelude.Maybe Prelude.Either [] (Qvar k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (Qtycon k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalLeftParenthesisKeyBase k z s)) (LexicalDotDot k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalRightParenthesisKeyBase k z s)) (Cname k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (l (LexicalCommaKeyBase k z s)) (Qtycls k z s l lexicalAnnotation lexicalAnnotation) (Var k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (LexicalModule k z s l lexicalAnnotation lexicalAnnotation) (Modid k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.Export k z s l lexicalAnnotation grammarAnnotation annotation)
-- | 'CnameBase' with fewer unresolved variables, with alias linking.
type Cname k z s l lexicalAnnotation grammarAnnotation annotation = CnameBase (Var k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (Con k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) annotation (StandardLinking.Cname k z s l lexicalAnnotation grammarAnnotation annotation)

-- § 5.3 Import Declarations types.

-- | 'ImpDeclBase' with fewer unresolved variables, with alias linking.
type ImpDecl k z s l lexicalAnnotation grammarAnnotation annotation = ImpDeclBase (,) Prelude.Maybe (LexicalImport k z s l lexicalAnnotation lexicalAnnotation) (LexicalQualified k z s l lexicalAnnotation lexicalAnnotation) (Modid k z s l lexicalAnnotation lexicalAnnotation) (LexicalAs k z s l lexicalAnnotation lexicalAnnotation) (ImpSpec k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) annotation (StandardLinking.ImpDecl k z s l lexicalAnnotation grammarAnnotation annotation)
-- | 'ImpSpecBase' with fewer unresolved variables, with alias linking.
type ImpSpec k z s l lexicalAnnotation grammarAnnotation annotation = ImpSpecBase (,) Prelude.Maybe [] (l (LexicalLeftParenthesisKeyBase k z s)) (Import k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (l (LexicalCommaKeyBase k z s)) (l (LexicalRightParenthesisKeyBase k z s)) (LexicalHiding k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.ImpSpec k z s l lexicalAnnotation grammarAnnotation annotation)
-- | 'ImportBase' with fewer unresolved variables, with alias linking.
type Import k z s l lexicalAnnotation grammarAnnotation annotation = ImportBase (,,) (,) Prelude.Maybe Prelude.Either [] (Var k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (l (LexicalLeftParenthesisKeyBase k z s)) (LexicalDotDot k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalRightParenthesisKeyBase k z s)) (Cname k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (l (LexicalCommaKeyBase k z s)) annotation (StandardLinking.Import k z s l lexicalAnnotation grammarAnnotation annotation)

-- § 4 Declarations and Bindings types.

-- | 'TopDeclBase' with fewer unresolved variables, with alias linking.
type TopDecl k z s l lexicalAnnotation grammarAnnotation annotation = TopDeclBase (,) Prelude.Maybe [] (LexicalType k z s l lexicalAnnotation lexicalAnnotation) (SimpleType k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (Type k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (LexicalData k z s l lexicalAnnotation lexicalAnnotation) (Context k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (LexicalDoubleRightArrow k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalEqualsKeyBase k z s)) (Constrs k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (Deriving k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (LexicalNewtype k z s l lexicalAnnotation lexicalAnnotation) (NewConstr k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (LexicalClass k z s l lexicalAnnotation lexicalAnnotation) (Scontext k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (Tycls k z s l lexicalAnnotation lexicalAnnotation) (Tyvar k z s l lexicalAnnotation lexicalAnnotation) (LexicalWhere k z s l lexicalAnnotation lexicalAnnotation) (Cdecls k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (LexicalInstance k z s l lexicalAnnotation lexicalAnnotation) (Qtycls k z s l lexicalAnnotation lexicalAnnotation) (Inst k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (Idecls k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (LexicalDefault k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalLeftParenthesisKeyBase k z s)) (l (LexicalCommaKeyBase k z s)) (l (LexicalRightParenthesisKeyBase k z s)) (LexicalForeign k z s l lexicalAnnotation lexicalAnnotation) (Fdecl k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (Decl k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) annotation (StandardLinking.TopDecl k z s l lexicalAnnotation grammarAnnotation annotation)
-- | 'DeclsBase' with fewer unresolved variables, with alias linking.
type Decls k z s l lexicalAnnotation grammarAnnotation annotation = DeclsBase (,) Prelude.Maybe [] (l (LexicalLeftBraceKeyBase k z s)) (StandardLinking.Decl k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (l (LexicalSemicolonKeyBase k z s)) (l (LexicalRightBraceKeyBase k z s)) annotation (StandardLinking.Decls k z s l lexicalAnnotation grammarAnnotation annotation)
-- | 'DeclBase' with fewer unresolved variables, with alias linking.
type Decl k z s l lexicalAnnotation grammarAnnotation annotation = DeclBase Prelude.Either (GenDecl k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (Funlhs k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (Pat k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (StandardLinking.Rhs k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) annotation (StandardLinking.Decl k z s l lexicalAnnotation grammarAnnotation annotation)
-- | 'CdeclsBase' with fewer unresolved variables, with alias linking.
type Cdecls k z s l lexicalAnnotation grammarAnnotation annotation = CdeclsBase (,) Prelude.Maybe [] (l (LexicalLeftBraceKeyBase k z s)) (Cdecl k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (l (LexicalSemicolonKeyBase k z s)) (l (LexicalRightBraceKeyBase k z s)) annotation (StandardLinking.Cdecls k z s l lexicalAnnotation grammarAnnotation annotation)
-- | 'CdeclBase' with fewer unresolved variables, with alias linking.
type Cdecl k z s l lexicalAnnotation grammarAnnotation annotation = CdeclBase Prelude.Either (GenDecl k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (Funlhs k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (Pat k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (Rhs k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) annotation (StandardLinking.Cdecl k z s l lexicalAnnotation grammarAnnotation annotation)
-- | 'IdeclsBase' with fewer unresolved variables, with alias linking.
type Idecls k z s l lexicalAnnotation grammarAnnotation annotation = IdeclsBase (,) Prelude.Maybe [] (l (LexicalLeftBraceKeyBase k z s)) (Idecl k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (l (LexicalSemicolonKeyBase k z s)) (l (LexicalRightBraceKeyBase k z s)) annotation (StandardLinking.Idecls k z s l lexicalAnnotation grammarAnnotation annotation)
-- | 'IdeclBase' with fewer unresolved variables, with alias linking.
type Idecl k z s l lexicalAnnotation grammarAnnotation annotation = IdeclBase Prelude.Either (Funlhs k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (Pat k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (Rhs k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) annotation (StandardLinking.Idecl k z s l lexicalAnnotation grammarAnnotation annotation)
-- | 'GenDeclBase' with fewer unresolved variables, with alias linking.
type GenDecl k z s l lexicalAnnotation grammarAnnotation annotation = GenDeclBase (,) Prelude.Maybe (Vars k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (LexicalDoubleColon k z s l lexicalAnnotation lexicalAnnotation) (Context k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (LexicalDoubleRightArrow k z s l lexicalAnnotation lexicalAnnotation) (Type k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (Fixity k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (Integer k z s l lexicalAnnotation lexicalAnnotation) (Ops k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) annotation (StandardLinking.GenDecl k z s l lexicalAnnotation grammarAnnotation annotation)
-- | 'OpsBase' with fewer unresolved variables, with alias linking.
type Ops k z s l lexicalAnnotation grammarAnnotation annotation = OpsBase (,) [] (Op k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (l (LexicalCommaKeyBase k z s)) annotation (StandardLinking.Ops k z s l lexicalAnnotation grammarAnnotation annotation)
-- | 'VarsBase' with fewer unresolved variables, with alias linking.
type Vars k z s l lexicalAnnotation grammarAnnotation annotation = VarsBase (,) [] (Var k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (l (LexicalCommaKeyBase k z s)) annotation (StandardLinking.Vars k z s l lexicalAnnotation grammarAnnotation annotation)
-- | 'FixityBase' with fewer unresolved variables, with alias linking.
type Fixity k z s l lexicalAnnotation grammarAnnotation annotation = FixityBase (LexicalInfixl k z s l lexicalAnnotation lexicalAnnotation) (LexicalInfixr k z s l lexicalAnnotation lexicalAnnotation) (LexicalInfix k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.Fixity k z s l lexicalAnnotation grammarAnnotation annotation)

-- § 4.1.2 Syntax of Types types.

-- | 'TypeBase' with fewer unresolved variables, with alias linking.
type Type k z s l lexicalAnnotation grammarAnnotation annotation = TypeBase (,) Prelude.Maybe (StandardLinking.Btype k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (LexicalRightArrow k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.Type k z s l lexicalAnnotation grammarAnnotation annotation)
-- | 'BtypeBase' with fewer unresolved variables, with alias linking.
type Btype k z s l lexicalAnnotation grammarAnnotation annotation = BtypeBase Prelude.Maybe (StandardLinking.Atype k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) annotation (StandardLinking.Btype k z s l lexicalAnnotation grammarAnnotation annotation)
-- | 'AtypeBase' with fewer unresolved variables, with alias linking.
type Atype k z s l lexicalAnnotation grammarAnnotation annotation = AtypeBase (,) [] (Gtycon k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (Tyvar k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalLeftParenthesisKeyBase k z s)) (StandardLinking.Type k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (l (LexicalCommaKeyBase k z s)) (l (LexicalRightParenthesisKeyBase k z s)) (l (LexicalLeftAngleBracketKeyBase k z s)) (l (LexicalRightAngleBracketKeyBase k z s)) annotation (StandardLinking.Atype k z s l lexicalAnnotation grammarAnnotation annotation)
-- | 'GtyconBase' with fewer unresolved variables, with alias linking.
type Gtycon k z s l lexicalAnnotation grammarAnnotation annotation = GtyconBase [] (Qtycon k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalLeftParenthesisKeyBase k z s)) (l (LexicalRightParenthesisKeyBase k z s)) (l (LexicalLeftBracketKeyBase k z s)) (l (LexicalRightBracketKeyBase k z s)) (LexicalRightArrow k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalCommaKeyBase k z s)) annotation (StandardLinking.Gtycon k z s l lexicalAnnotation grammarAnnotation annotation)

-- § 4.1.3 Syntax of Class Assertions and Contexts types.

-- | 'ContextBase' with fewer unresolved variables, with alias linking.
type Context k z s l lexicalAnnotation grammarAnnotation annotation = ContextBase (,) Prelude.Maybe [] (Class k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (l (LexicalLeftParenthesisKeyBase k z s)) (l (LexicalCommaKeyBase k z s)) (l (LexicalRightParenthesisKeyBase k z s)) annotation (StandardLinking.Context k z s l lexicalAnnotation grammarAnnotation annotation)
-- | 'ClassBase' with fewer unresolved variables, with alias linking.
type Class k z s l lexicalAnnotation grammarAnnotation annotation = ClassBase [] (Qtycls k z s l lexicalAnnotation lexicalAnnotation) (Tyvar k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalLeftParenthesisKeyBase k z s)) (Atype k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (l (LexicalRightParenthesisKeyBase k z s)) annotation (StandardLinking.Class k z s l lexicalAnnotation grammarAnnotation annotation)
{-
-- | 'ClassQtyclsBase' with fewer unresolved variables, with alias linking.
type ClassQtycls k z s l lexicalAnnotation grammarAnnotation annotation = ClassQtyclsBase (,) Prelude.Maybe (Modid k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalDotKeyBase k z s)) (Tycls k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.ClassQtycls k z s l lexicalAnnotation grammarAnnotation annotation)
-- | 'ClassTyclsBase' with fewer unresolved variables, with alias linking.
type ClassTycls k z s l lexicalAnnotation grammarAnnotation annotation = ClassTyclsBase (Conid k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.ClassTycls k z s l lexicalAnnotation grammarAnnotation annotation)
-- | 'ClassTyvarBase' with fewer unresolved variables, with alias linking.
type ClassTyvar k z s l lexicalAnnotation grammarAnnotation annotation = ClassTyvarBase (Varid k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.ClassTyvar k z s l lexicalAnnotation grammarAnnotation annotation)
-}

-- § 4.2.1 Algebraic Datatype Declarations types.

-- | 'SimpleTypeBase' with fewer unresolved variables, with alias linking.
type SimpleType k z s l lexicalAnnotation grammarAnnotation annotation = SimpleTypeBase [] (Tycon k z s l lexicalAnnotation lexicalAnnotation) (Tyvar k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.SimpleType k z s l lexicalAnnotation grammarAnnotation annotation)
-- | 'ConstrsBase' with fewer unresolved variables, with alias linking.
type Constrs k z s l lexicalAnnotation grammarAnnotation annotation = ConstrsBase (,) [] (Constr k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (l (LexicalPipeKeyBase k z s)) annotation (StandardLinking.Constrs k z s l lexicalAnnotation grammarAnnotation annotation)
-- | 'ConstrBase' with fewer unresolved variables, with alias linking.
type Constr k z s l lexicalAnnotation grammarAnnotation annotation = ConstrBase (,) [] Prelude.Either Prelude.Maybe (Con k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (EvalAtype k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (Btype k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (Conop k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (l (LexicalLeftBraceKeyBase k z s)) (FieldDecl k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (l (LexicalCommaKeyBase k z s)) (l (LexicalRightBraceKeyBase k z s)) annotation (StandardLinking.Constr k z s l lexicalAnnotation grammarAnnotation annotation)
-- | 'EvalAtypeBase' with fewer unresolved variables, with alias linking.
type EvalAtype k z s l lexicalAnnotation grammarAnnotation annotation = EvalAtypeBase Prelude.Maybe (l (LexicalExclamationKeyBase k z s)) (Atype k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) annotation (StandardLinking.EvalAtype k z s l lexicalAnnotation grammarAnnotation annotation)
-- | 'FieldDeclBase' with fewer unresolved variables, with alias linking.
type FieldDecl k z s l lexicalAnnotation grammarAnnotation annotation = FieldDeclBase Prelude.Either (Vars k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (LexicalDoubleColon k z s l lexicalAnnotation lexicalAnnotation) (Type k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (EvalAtype k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) annotation (StandardLinking.FieldDecl k z s l lexicalAnnotation grammarAnnotation annotation)
-- | 'DerivingBase' with fewer unresolved variables, with alias linking.
type Deriving k z s l lexicalAnnotation grammarAnnotation annotation = DerivingBase (,,) (,) Prelude.Either Prelude.Maybe [] (LexicalDeriving k z s l lexicalAnnotation lexicalAnnotation) (Dclass k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (l (LexicalLeftParenthesisKeyBase k z s)) (l (LexicalCommaKeyBase k z s)) (l (LexicalRightParenthesisKeyBase k z s)) annotation (StandardLinking.Deriving k z s l lexicalAnnotation grammarAnnotation annotation)
-- | 'DclassBase' with fewer unresolved variables, with alias linking.
type Dclass k z s l lexicalAnnotation grammarAnnotation annotation = DclassBase (Qtycls k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.Dclass k z s l lexicalAnnotation grammarAnnotation annotation)

-- § 4.2.3 Datatype Renamings types.

-- | 'NewConstrBase' with fewer unresolved variables, with alias linking.
type NewConstr k z s l lexicalAnnotation grammarAnnotation annotation = NewConstrBase (Con k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (Atype k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (l (LexicalLeftBraceKeyBase k z s)) (Var k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (LexicalDoubleColon k z s l lexicalAnnotation lexicalAnnotation) (Type k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (l (LexicalRightBraceKeyBase k z s)) annotation (StandardLinking.NewConstr k z s l lexicalAnnotation grammarAnnotation annotation)

-- § 4.3.1 Type Classes and Overloading types.

-- | 'ScontextBase' with fewer unresolved variables, with alias linking.
type Scontext k z s l lexicalAnnotation grammarAnnotation annotation = ScontextBase (,) Prelude.Maybe [] (SimpleClass k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (l (LexicalLeftParenthesisKeyBase k z s)) (l (LexicalCommaKeyBase k z s)) (l (LexicalRightParenthesisKeyBase k z s)) annotation (StandardLinking.Scontext k z s l lexicalAnnotation grammarAnnotation annotation)
-- | 'SimpleClassBase' with fewer unresolved variables, with alias linking.
type SimpleClass k z s l lexicalAnnotation grammarAnnotation annotation = SimpleClassBase (Qtycls k z s l lexicalAnnotation lexicalAnnotation) (Tyvar k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.SimpleClass k z s l lexicalAnnotation grammarAnnotation annotation)

-- § 4.3.2 Instance Declarations types.

-- | 'InstBase' with fewer unresolved variables, with alias linking.
type Inst k z s l lexicalAnnotation grammarAnnotation annotation = InstBase (,) [] (Gtycon k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (l (LexicalLeftParenthesisKeyBase k z s)) (Tyvar k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalRightParenthesisKeyBase k z s)) (l (LexicalCommaKeyBase k z s)) (l (LexicalLeftBracketKeyBase k z s)) (l (LexicalRightBracketKeyBase k z s)) (LexicalRightArrow k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.Inst k z s l lexicalAnnotation grammarAnnotation annotation)
{-
-- | 'FixityOpBase' with fewer unresolved variables, with alias linking.
type FixityOp k z s l lexicalAnnotation grammarAnnotation annotation = FixityOpBase (Varop k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (Conop k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) annotation (StandardLinking.FixityOp k z s l lexicalAnnotation grammarAnnotation annotation)
-}

{-
-- § 4.4.2 Fixity Declarations types.
-- | 'FixityOpBase' with fewer unresolved variables, with alias linking.
type FixityOp k z s l lexicalAnnotation grammarAnnotation annotation = FixityOpBase (Varop k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (Conop k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) annotation (StandardLinking.FixityOp k z s l lexicalAnnotation grammarAnnotation annotation)
-}

-- § 4.4.3 Function and Pattern Bindings types.

-- | 'FunlhsBase' with fewer unresolved variables, with alias linking.
type Funlhs k z s l lexicalAnnotation grammarAnnotation annotation = FunlhsBase [] (Var k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (Apat k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (Pat k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (Varop k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (l (LexicalLeftParenthesisKeyBase k z s)) (l (LexicalRightParenthesisKeyBase k z s)) annotation (StandardLinking.Funlhs k z s l lexicalAnnotation grammarAnnotation annotation)
-- | 'RhsBase' with fewer unresolved variables, with alias linking.
type Rhs k z s l lexicalAnnotation grammarAnnotation annotation = RhsBase (,) Prelude.Maybe (l (LexicalEqualsKeyBase k z s)) (StandardLinking.Exp k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (LexicalWhere k z s l lexicalAnnotation lexicalAnnotation) (Decls k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (StandardLinking.Gdrhs k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) annotation (StandardLinking.Rhs k z s l lexicalAnnotation grammarAnnotation annotation)
-- | 'GdrhsBase' with fewer unresolved variables, with alias linking.
type Gdrhs k z s l lexicalAnnotation grammarAnnotation annotation = GdrhsBase Prelude.Maybe (StandardLinking.Guards k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (l (LexicalEqualsKeyBase k z s)) (StandardLinking.Exp k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) annotation (StandardLinking.Gdrhs k z s l lexicalAnnotation grammarAnnotation annotation)
{-
-- | 'GuardsBase' with fewer unresolved variables, with alias linking.
type Guards k z s l lexicalAnnotation grammarAnnotation annotation = GuardsBase (,) [] (l (LexicalPipeKeyBase k z s)) (StandardLinking.Guard k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (l (LexicalCommaKeyBase k z s)) annotation (StandardLinking.Guards k z s l lexicalAnnotation grammarAnnotation annotation)
-- | 'GuardBase' with fewer unresolved variables, with alias linking.
type Guard k z s l lexicalAnnotation grammarAnnotation annotation = GuardBase (Pat k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (LexicalLeftArrow k z s l lexicalAnnotation lexicalAnnotation) (StandardLinking.InfixExp k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (LexicalLet k z s l lexicalAnnotation lexicalAnnotation) (StandardLinking.Decls k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) annotation (StandardLinking.Guard k z s l lexicalAnnotation grammarAnnotation annotation)
-}

-- § 8.4 Foreign Declarations types.

-- | 'FdeclBase' with fewer unresolved variables, with alias linking.
type Fdecl k z s l lexicalAnnotation grammarAnnotation annotation = FdeclBase Prelude.Maybe (LexicalImport k z s l lexicalAnnotation lexicalAnnotation) (CallConv k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (Safety k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (Impent k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (Var k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (LexicalDoubleColon k z s l lexicalAnnotation lexicalAnnotation) (Ftype k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (LexicalExport k z s l lexicalAnnotation lexicalAnnotation) (Expent k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) annotation (StandardLinking.Fdecl k z s l lexicalAnnotation grammarAnnotation annotation)
-- | 'CallConvBase' with fewer unresolved variables, with alias linking.
type CallConv k z s l lexicalAnnotation grammarAnnotation annotation = CallConvBase (LexicalCcall k z s l lexicalAnnotation lexicalAnnotation) (LexicalStdcall k z s l lexicalAnnotation lexicalAnnotation) (LexicalCplusplus k z s l lexicalAnnotation lexicalAnnotation) (LexicalJvm k z s l lexicalAnnotation lexicalAnnotation) (LexicalDotnet k z s l lexicalAnnotation lexicalAnnotation) (Varid k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.CallConv k z s l lexicalAnnotation grammarAnnotation annotation)
-- | 'ImpentBase' with fewer unresolved variables, with alias linking.
type Impent k z s l lexicalAnnotation grammarAnnotation annotation = ImpentBase Prelude.Maybe (String k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.Impent k z s l lexicalAnnotation grammarAnnotation annotation)
-- | 'ExpentBase' with fewer unresolved variables, with alias linking.
type Expent k z s l lexicalAnnotation grammarAnnotation annotation = ExpentBase Prelude.Maybe (String k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.Expent k z s l lexicalAnnotation grammarAnnotation annotation)
-- | 'SafetyBase' with fewer unresolved variables, with alias linking.
type Safety k z s l lexicalAnnotation grammarAnnotation annotation = SafetyBase (LexicalUnsafe k z s l lexicalAnnotation lexicalAnnotation) (LexicalSafe k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.Safety k z s l lexicalAnnotation grammarAnnotation annotation)

-- § 8.4.2 Foreign Types types.

-- | 'FtypeBase' with fewer unresolved variables, with alias linking.
type Ftype k z s l lexicalAnnotation grammarAnnotation annotation = FtypeBase (Frtype k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (Fatype k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (LexicalRightArrow k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.Ftype k z s l lexicalAnnotation grammarAnnotation annotation)
-- | 'FrtypeBase' with fewer unresolved variables, with alias linking.
type Frtype k z s l lexicalAnnotation grammarAnnotation annotation = FrtypeBase (Fatype k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (l (LexicalLeftParenthesisKeyBase k z s)) (l (LexicalRightParenthesisKeyBase k z s)) annotation (StandardLinking.Frtype k z s l lexicalAnnotation grammarAnnotation annotation)
-- | 'FatypeBase' with fewer unresolved variables, with alias linking.
type Fatype k z s l lexicalAnnotation grammarAnnotation annotation = FatypeBase [] (Qtycon k z s l lexicalAnnotation lexicalAnnotation) (Atype k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) annotation (StandardLinking.Fatype k z s l lexicalAnnotation grammarAnnotation annotation)

-- § 8.5.1 Standard C Calls types.

-- | 'ImpentCcallBase' with fewer unresolved variables, with alias linking.
type ImpentCcall k z s l lexicalAnnotation grammarAnnotation annotation = ImpentCcallBase Prelude.Maybe (l (LexicalDoubleQuoteKeyBase k z s)) (LexicalStatic k z s l lexicalAnnotation lexicalAnnotation) (Chname k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalAmpersandKeyBase k z s)) (Cid k z s l lexicalAnnotation lexicalAnnotation) (LexicalDynamic k z s l lexicalAnnotation lexicalAnnotation) (LexicalWrapper k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.ImpentCcall k z s l lexicalAnnotation grammarAnnotation annotation)
-- | 'ExpentCcallBase' with fewer unresolved variables, with alias linking.
type ExpentCcall k z s l lexicalAnnotation grammarAnnotation annotation = ExpentCcallBase Prelude.Maybe (l (LexicalDoubleQuoteKeyBase k z s)) (Cid k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.ExpentCcall k z s l lexicalAnnotation grammarAnnotation annotation)

-- § 3 Expressions types.

-- | 'ExpBase' with fewer unresolved variables, with alias linking.
type Exp k z s l lexicalAnnotation grammarAnnotation annotation = ExpBase (,) Prelude.Maybe (StandardLinking.InfixExp k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (LexicalDoubleColon k z s l lexicalAnnotation lexicalAnnotation) (Context k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (LexicalDoubleRightArrow k z s l lexicalAnnotation lexicalAnnotation) (Type k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) annotation (StandardLinking.Exp k z s l lexicalAnnotation grammarAnnotation annotation)
-- | 'InfixExpBase' with fewer unresolved variables, with alias linking.
type InfixExp k z s l lexicalAnnotation grammarAnnotation annotation = InfixExpBase (StandardLinking.Lexp k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (Qop k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (LexicalMinus k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.InfixExp k z s l lexicalAnnotation grammarAnnotation annotation)
-- | 'LexpBase' with fewer unresolved variables, with alias linking.
type Lexp k z s l lexicalAnnotation grammarAnnotation annotation = LexpBase Prelude.Maybe [] (LexicalAsciiLambda k z s l lexicalAnnotation lexicalAnnotation) (Apat k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (LexicalRightArrow k z s l lexicalAnnotation lexicalAnnotation) (StandardLinking.Exp k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (LexicalLet k z s l lexicalAnnotation lexicalAnnotation) (Decls k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (LexicalIn k z s l lexicalAnnotation lexicalAnnotation) (LexicalIf k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalSemicolonKeyBase k z s)) (LexicalThen k z s l lexicalAnnotation lexicalAnnotation) (LexicalElse k z s l lexicalAnnotation lexicalAnnotation) (LexicalCase k z s l lexicalAnnotation lexicalAnnotation) (LexicalOf k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalLeftBraceKeyBase k z s)) (StandardLinking.Alts k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (l (LexicalRightBraceKeyBase k z s)) (StandardLinking.Stmts k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (StandardLinking.Fexp k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) annotation (StandardLinking.Lexp k z s l lexicalAnnotation grammarAnnotation annotation)
-- | 'FexpBase' with fewer unresolved variables, with alias linking.
type Fexp k z s l lexicalAnnotation grammarAnnotation annotation = FexpBase Prelude.Maybe (StandardLinking.Aexp k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) annotation (StandardLinking.Fexp k z s l lexicalAnnotation grammarAnnotation annotation)
-- | 'AexpBase' with fewer unresolved variables, with alias linking.
type Aexp k z s l lexicalAnnotation grammarAnnotation annotation = AexpBase (,) Prelude.Maybe [] (Qvar k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (Gcon k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (Literal k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalLeftParenthesisKeyBase k z s)) (StandardLinking.Exp k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (l (LexicalRightParenthesisKeyBase k z s)) (l (LexicalCommaKeyBase k z s)) (l (LexicalLeftBracketKeyBase k z s)) (l (LexicalRightBracketKeyBase k z s)) (LexicalDotDot k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalPipeKeyBase k z s)) (StandardLinking.Qual k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (StandardLinking.InfixExp k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (Qop k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (QopSansMinus k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (Qcon k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (l (LexicalLeftBraceKeyBase k z s)) (StandardLinking.Fbind k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (l (LexicalRightBraceKeyBase k z s)) (StandardLinking.AexpSansQcon k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) annotation (StandardLinking.Aexp k z s l lexicalAnnotation grammarAnnotation annotation)
-- | 'AexpSansQconBase' with fewer unresolved variables, with alias linking.
type AexpSansQcon k z s l lexicalAnnotation grammarAnnotation annotation = AexpSansQconBase (,) Prelude.Maybe [] (Qvar k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (GconSansQcon k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (Literal k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalLeftParenthesisKeyBase k z s)) (StandardLinking.Exp k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (l (LexicalRightParenthesisKeyBase k z s)) (l (LexicalCommaKeyBase k z s)) (l (LexicalLeftBracketKeyBase k z s)) (l (LexicalRightBracketKeyBase k z s)) (LexicalDotDot k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalPipeKeyBase k z s)) (StandardLinking.Qual k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (StandardLinking.InfixExp k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (Qop k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (QopSansMinus k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (Qcon k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (l (LexicalLeftBraceKeyBase k z s)) (StandardLinking.Fbind k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (l (LexicalRightBraceKeyBase k z s)) annotation (StandardLinking.AexpSansQcon k z s l lexicalAnnotation grammarAnnotation annotation)

-- § 3.2 Variables, Constructors, Operators, and Literals types.

-- | 'GconBase' with fewer unresolved variables, with alias linking.
type Gcon k z s l lexicalAnnotation grammarAnnotation annotation = GconBase [] (l (LexicalLeftParenthesisKeyBase k z s)) (l (LexicalRightParenthesisKeyBase k z s)) (l (LexicalLeftBracketKeyBase k z s)) (l (LexicalRightBracketKeyBase k z s)) (l (LexicalCommaKeyBase k z s)) (Qcon k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) annotation (StandardLinking.Gcon k z s l lexicalAnnotation grammarAnnotation annotation)
-- | 'GconSansQconBase' with fewer unresolved variables, with alias linking.
type GconSansQcon k z s l lexicalAnnotation grammarAnnotation annotation = GconSansQconBase [] (l (LexicalLeftParenthesisKeyBase k z s)) (l (LexicalRightParenthesisKeyBase k z s)) (l (LexicalLeftBracketKeyBase k z s)) (l (LexicalRightBracketKeyBase k z s)) (l (LexicalCommaKeyBase k z s)) annotation (StandardLinking.GconSansQcon k z s l lexicalAnnotation grammarAnnotation annotation)
-- | 'VarBase' with fewer unresolved variables, with alias linking.
type Var k z s l lexicalAnnotation grammarAnnotation annotation = VarBase (Varid k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalLeftParenthesisKeyBase k z s)) (VarSym k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalRightParenthesisKeyBase k z s)) annotation (StandardLinking.Var k z s l lexicalAnnotation grammarAnnotation annotation)
-- | 'QvarBase' with fewer unresolved variables, with alias linking.
type Qvar k z s l lexicalAnnotation grammarAnnotation annotation = QvarBase (Qvarid k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalLeftParenthesisKeyBase k z s)) (QvarSym k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalRightParenthesisKeyBase k z s)) annotation (StandardLinking.Qvar k z s l lexicalAnnotation grammarAnnotation annotation)
-- | 'ConBase' with fewer unresolved variables, with alias linking.
type Con k z s l lexicalAnnotation grammarAnnotation annotation = ConBase (Conid k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalLeftParenthesisKeyBase k z s)) (ConSym k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalRightParenthesisKeyBase k z s)) annotation (StandardLinking.Con k z s l lexicalAnnotation grammarAnnotation annotation)
-- | 'QconBase' with fewer unresolved variables, with alias linking.
type Qcon k z s l lexicalAnnotation grammarAnnotation annotation = QconBase (Qconid k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalLeftParenthesisKeyBase k z s)) (StandardLinking.GconSym k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (l (LexicalRightParenthesisKeyBase k z s)) annotation (StandardLinking.Qcon k z s l lexicalAnnotation grammarAnnotation annotation)
-- | 'VaropBase' with fewer unresolved variables, with alias linking.
type Varop k z s l lexicalAnnotation grammarAnnotation annotation = VaropBase (VarSym k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalBacktickKeyBase k z s)) (Varid k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.Varop k z s l lexicalAnnotation grammarAnnotation annotation)
-- | 'QvaropBase' with fewer unresolved variables, with alias linking.
type Qvarop k z s l lexicalAnnotation grammarAnnotation annotation = QvaropBase (QvarSym k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalBacktickKeyBase k z s)) (Qvarid k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.Qvarop k z s l lexicalAnnotation grammarAnnotation annotation)
-- | 'QvaropSansMinusBase' with fewer unresolved variables, with alias linking.
type QvaropSansMinus k z s l lexicalAnnotation grammarAnnotation annotation = QvaropSansMinusBase (QvarSymSansMinus k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalBacktickKeyBase k z s)) (Qvarid k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.QvaropSansMinus k z s l lexicalAnnotation grammarAnnotation annotation)
-- | 'ConopBase' with fewer unresolved variables, with alias linking.
type Conop k z s l lexicalAnnotation grammarAnnotation annotation = ConopBase (ConSym k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalBacktickKeyBase k z s)) (Conid k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.Conop k z s l lexicalAnnotation grammarAnnotation annotation)
-- | 'QconopBase' with fewer unresolved variables, with alias linking.
type Qconop k z s l lexicalAnnotation grammarAnnotation annotation = QconopBase (QconSym k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalBacktickKeyBase k z s)) (Qconid k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.Qconop k z s l lexicalAnnotation grammarAnnotation annotation)
-- | 'OpBase' with fewer unresolved variables, with alias linking.
type Op k z s l lexicalAnnotation grammarAnnotation annotation = OpBase (Varop k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (Conop k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) annotation (StandardLinking.Op k z s l lexicalAnnotation grammarAnnotation annotation)
-- | 'QopBase' with fewer unresolved variables, with alias linking.
type Qop k z s l lexicalAnnotation grammarAnnotation annotation = QopBase (Qvarop k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (Qconop k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) annotation (StandardLinking.Qop k z s l lexicalAnnotation grammarAnnotation annotation)
-- | 'QopSansMinusBase' with fewer unresolved variables, with alias linking.
type QopSansMinus k z s l lexicalAnnotation grammarAnnotation annotation = QopSansMinusBase (QvaropSansMinus k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (Qconop k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) annotation (StandardLinking.QopSansMinus k z s l lexicalAnnotation grammarAnnotation annotation)
-- | 'GconSymBase' with fewer unresolved variables, with alias linking.
type GconSym k z s l lexicalAnnotation grammarAnnotation annotation = GconSymBase (l (LexicalColonKeyBase k z s)) (StandardLinking.Qcon k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) annotation (StandardLinking.GconSym k z s l lexicalAnnotation grammarAnnotation annotation)

-- § 3.11 List Comprehensions types.

-- | 'QualBase' with fewer unresolved variables, with alias linking.
type Qual k z s l lexicalAnnotation grammarAnnotation annotation = QualBase (Pat k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (LexicalLeftArrow k z s l lexicalAnnotation lexicalAnnotation) (StandardLinking.Exp k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (LexicalLet k z s l lexicalAnnotation lexicalAnnotation) (StandardLinking.Decls k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) annotation (StandardLinking.Qual k z s l lexicalAnnotation grammarAnnotation annotation)

-- § 3.13 Case Expressions types.

-- | 'AltsBase' with fewer unresolved variables, with alias linking.
type Alts k z s l lexicalAnnotation grammarAnnotation annotation = AltsBase (,) [] (StandardLinking.Alt k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (l (LexicalSemicolonKeyBase k z s)) annotation (StandardLinking.Alts k z s l lexicalAnnotation grammarAnnotation annotation)
-- | 'AltBase' with fewer unresolved variables, with alias linking.
type Alt k z s l lexicalAnnotation grammarAnnotation annotation = AltBase (,) Prelude.Maybe (Pat k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (LexicalRightArrow k z s l lexicalAnnotation lexicalAnnotation) (StandardLinking.Exp k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (LexicalWhere k z s l lexicalAnnotation lexicalAnnotation) (StandardLinking.Decls k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (StandardLinking.Gdpat k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) annotation (StandardLinking.Alt k z s l lexicalAnnotation grammarAnnotation annotation)
-- | 'GdpatBase' with fewer unresolved variables, with alias linking.
type Gdpat k z s l lexicalAnnotation grammarAnnotation annotation = GdpatBase Prelude.Maybe (Guards k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (LexicalRightArrow k z s l lexicalAnnotation lexicalAnnotation) (StandardLinking.Exp k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) annotation (StandardLinking.Gdpat k z s l lexicalAnnotation grammarAnnotation annotation)
-- | 'GuardsBase' with fewer unresolved variables, with alias linking.
type Guards k z s l lexicalAnnotation grammarAnnotation annotation = GuardsBase (,) [] (l (LexicalPipeKeyBase k z s)) (StandardLinking.Guard k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (l (LexicalCommaKeyBase k z s)) annotation (StandardLinking.Guards k z s l lexicalAnnotation grammarAnnotation annotation)
-- | 'GuardBase' with fewer unresolved variables, with alias linking.
type Guard k z s l lexicalAnnotation grammarAnnotation annotation = GuardBase (Pat k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (LexicalLeftArrow k z s l lexicalAnnotation lexicalAnnotation) (StandardLinking.InfixExp k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (LexicalLet k z s l lexicalAnnotation lexicalAnnotation) (StandardLinking.Decls k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) annotation (StandardLinking.Guard k z s l lexicalAnnotation grammarAnnotation annotation)

-- § 3.14 Do Expressions types.

-- | 'StmtsBase' with fewer unresolved variables, with alias linking.
type Stmts k z s l lexicalAnnotation grammarAnnotation annotation = StmtsBase [] Prelude.Maybe (Exp k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (l (LexicalSemicolonKeyBase k z s)) (StandardLinking.Stmt k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) annotation (StandardLinking.Stmts k z s l lexicalAnnotation grammarAnnotation annotation)
-- | 'StmtBase' with fewer unresolved variables, with alias linking.
type Stmt k z s l lexicalAnnotation grammarAnnotation annotation = StmtBase (StandardLinking.Exp k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (l (LexicalSemicolonKeyBase k z s)) (Pat k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (LexicalLeftArrow k z s l lexicalAnnotation lexicalAnnotation) (LexicalLet k z s l lexicalAnnotation lexicalAnnotation) (StandardLinking.Decls k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) annotation (StandardLinking.Stmt k z s l lexicalAnnotation grammarAnnotation annotation)

-- § 3.15.2 Construction Using Field Labels types.

-- | 'FbindBase' with fewer unresolved variables, with alias linking.
type Fbind k z s l lexicalAnnotation grammarAnnotation annotation = FbindBase (Qvar k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (l (LexicalEqualsKeyBase k z s)) (StandardLinking.Exp k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) annotation (StandardLinking.Fbind k z s l lexicalAnnotation grammarAnnotation annotation)

-- § 3.17.1 Patterns types.

-- | 'PatBase' with fewer unresolved variables, with alias linking.
type Pat k z s l lexicalAnnotation grammarAnnotation annotation = PatBase (StandardLinking.Lpat k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (Qconop k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) annotation (StandardLinking.Pat k z s l lexicalAnnotation grammarAnnotation annotation)
-- | 'LpatBase' with fewer unresolved variables, with alias linking.
type Lpat k z s l lexicalAnnotation grammarAnnotation annotation = LpatBase Prelude.Either [] (StandardLinking.Apat k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (LexicalMinus k z s l lexicalAnnotation lexicalAnnotation) (Integer k z s l lexicalAnnotation lexicalAnnotation) (Float k z s l lexicalAnnotation lexicalAnnotation) (Gcon k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) annotation (StandardLinking.Lpat k z s l lexicalAnnotation grammarAnnotation annotation)
-- | 'ApatBase' with fewer unresolved variables, with alias linking.
type Apat k z s l lexicalAnnotation grammarAnnotation annotation = ApatBase (,) Prelude.Maybe [] (Var k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (l (LexicalAtKeyBase k z s)) (Gcon k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (Qcon k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (l (LexicalLeftBraceKeyBase k z s)) (StandardLinking.Fpat k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (l (LexicalCommaKeyBase k z s)) (l (LexicalRightBraceKeyBase k z s)) (Literal k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalUnderscoreKeyBase k z s)) (l (LexicalLeftParenthesisKeyBase k z s)) (StandardLinking.Pat k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (l (LexicalRightParenthesisKeyBase k z s)) (l (LexicalLeftBracketKeyBase k z s)) (l (LexicalRightBracketKeyBase k z s)) (l (LexicalTildeKeyBase k z s)) annotation (StandardLinking.Apat k z s l lexicalAnnotation grammarAnnotation annotation)
-- | 'FpatBase' with fewer unresolved variables, with alias linking.
type Fpat k z s l lexicalAnnotation grammarAnnotation annotation = FpatBase (Qvar k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) (l (LexicalEqualsKeyBase k z s)) (StandardLinking.Pat k z s l lexicalAnnotation grammarAnnotation grammarAnnotation) annotation (StandardLinking.Fpat k z s l lexicalAnnotation grammarAnnotation annotation)

-- Exclusion structures.

-- § 2.4 Identifiers and Operators types.

-- Exclusion structures types.

-- | 'VaridInnerSansAscSmallUnderscoreBase' with fewer unresolved variables, with alias linking.
type VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation annotation = VaridInnerSansAscSmallUnderscoreBase (SmallSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation) (Large k z s l lexicalAnnotation lexicalAnnotation) (Digit k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalSingleQuoteKeyBase k z s)) annotation (StandardLinking.VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation annotation)
-- | 'SmallSansAscSmallUnderscoreBase' with fewer unresolved variables, with alias linking.
type SmallSansAscSmallUnderscore k z s l lexicalAnnotation annotation = SmallSansAscSmallUnderscoreBase (AscSmall k z s l lexicalAnnotation lexicalAnnotation) (UniSmallSansAsc k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalUnderscoreKeyBase k z s)) annotation (StandardLinking.SmallSansAscSmallUnderscore k z s l lexicalAnnotation annotation)
-- | 'UniSmallSansAscBase' with fewer unresolved variables, with alias linking.
type UniSmallSansAsc k z s l lexicalAnnotation annotation = UniSmallSansAscBase (l (LexicalUnicodeSmallSansAscUnderscoreKeyBase k z s)) annotation (StandardLinking.UniSmallSansAsc k z s l lexicalAnnotation annotation)
-- | 'VaridBase' with fewer unresolved variables, with alias linking.
type Varid k z s l lexicalAnnotation annotation = VaridBase [] (SmallSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation) (VaridInner k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (VaridC k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalDLowerKeyBase k z s)) (VaridD k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalELowerKeyBase k z s)) (VaridE k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalFLowerKeyBase k z s)) (VaridF_ k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (VaridI k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (VaridL k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalMLowerKeyBase k z s)) (VaridM k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalNLowerKeyBase k z s)) (VaridN k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalOLowerKeyBase k z s)) (VaridO k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (VaridT k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (VaridW k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation (StandardLinking.Varid k z s l lexicalAnnotation annotation)
-- | 'VaridCBase' with fewer unresolved variables, with alias linking.
type VaridC k z s l lexicalAnnotation annotation = VaridCBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation) (VaridInner k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalALowerKeyBase k z s)) (VaridCa k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (VaridCl k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation (StandardLinking.VaridC k z s l lexicalAnnotation annotation)
-- | 'VaridDBase' with fewer unresolved variables, with alias linking.
type VaridD k z s l lexicalAnnotation annotation = VaridDBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation) (VaridInner k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalALowerKeyBase k z s)) (VaridDa k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (VaridDe k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation (StandardLinking.VaridD k z s l lexicalAnnotation annotation)
-- | 'VaridEBase' with fewer unresolved variables, with alias linking.
type VaridE k z s l lexicalAnnotation annotation = VaridEBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation) (VaridInner k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (VaridEl k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation (StandardLinking.VaridE k z s l lexicalAnnotation annotation)
-- | 'VaridFBase' with fewer unresolved variables, with alias linking.
type VaridF_ k z s l lexicalAnnotation annotation = VaridFBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation) (VaridInner k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (VaridFo k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation (StandardLinking.VaridF_ k z s l lexicalAnnotation annotation)
-- | 'VaridIBase' with fewer unresolved variables, with alias linking.
type VaridI k z s l lexicalAnnotation annotation = VaridIBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation) (VaridInner k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (VaridIm k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalNLowerKeyBase k z s)) (VaridIn k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation (StandardLinking.VaridI k z s l lexicalAnnotation annotation)
-- | 'VaridLBase' with fewer unresolved variables, with alias linking.
type VaridL k z s l lexicalAnnotation annotation = VaridLBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation) (VaridInner k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (VaridLe k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation (StandardLinking.VaridL k z s l lexicalAnnotation annotation)
-- | 'VaridMBase' with fewer unresolved variables, with alias linking.
type VaridM k z s l lexicalAnnotation annotation = VaridMBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation) (VaridInner k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (VaridMo k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation (StandardLinking.VaridM k z s l lexicalAnnotation annotation)
-- | 'VaridNBase' with fewer unresolved variables, with alias linking.
type VaridN k z s l lexicalAnnotation annotation = VaridNBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation) (VaridInner k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (VaridNe k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation (StandardLinking.VaridN k z s l lexicalAnnotation annotation)
-- | 'VaridOBase' with fewer unresolved variables, with alias linking.
type VaridO k z s l lexicalAnnotation annotation = VaridOBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation) (VaridInner k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation (StandardLinking.VaridO k z s l lexicalAnnotation annotation)
-- | 'VaridTBase' with fewer unresolved variables, with alias linking.
type VaridT k z s l lexicalAnnotation annotation = VaridTBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation) (VaridInner k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (VaridTh k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (VaridTy k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalZLowerKeyBase k z s)) annotation (StandardLinking.VaridT k z s l lexicalAnnotation annotation)
-- | 'VaridWBase' with fewer unresolved variables, with alias linking.
type VaridW k z s l lexicalAnnotation annotation = VaridWBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation) (VaridInner k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (VaridWh k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation (StandardLinking.VaridW k z s l lexicalAnnotation annotation)
-- | 'VaridCaBase' with fewer unresolved variables, with alias linking.
type VaridCa k z s l lexicalAnnotation annotation = VaridCaBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation) (VaridInner k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (VaridCas k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation (StandardLinking.VaridCa k z s l lexicalAnnotation annotation)
-- | 'VaridClBase' with fewer unresolved variables, with alias linking.
type VaridCl k z s l lexicalAnnotation annotation = VaridClBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation) (VaridInner k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalALowerKeyBase k z s)) (VaridCla k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation (StandardLinking.VaridCl k z s l lexicalAnnotation annotation)
-- | 'VaridDaBase' with fewer unresolved variables, with alias linking.
type VaridDa k z s l lexicalAnnotation annotation = VaridDaBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation) (VaridInner k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (VaridDat k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation (StandardLinking.VaridDa k z s l lexicalAnnotation annotation)
-- | 'VaridDeBase' with fewer unresolved variables, with alias linking.
type VaridDe k z s l lexicalAnnotation annotation = VaridDeBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation) (VaridInner k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (VaridDef k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (VaridDer k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation (StandardLinking.VaridDe k z s l lexicalAnnotation annotation)
-- | 'VaridElBase' with fewer unresolved variables, with alias linking.
type VaridEl k z s l lexicalAnnotation annotation = VaridElBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation) (VaridInner k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (VaridEls k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation (StandardLinking.VaridEl k z s l lexicalAnnotation annotation)
-- | 'VaridFoBase' with fewer unresolved variables, with alias linking.
type VaridFo k z s l lexicalAnnotation annotation = VaridFoBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation) (VaridInner k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (VaridFor k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation (StandardLinking.VaridFo k z s l lexicalAnnotation annotation)
-- | 'VaridImBase' with fewer unresolved variables, with alias linking.
type VaridIm k z s l lexicalAnnotation annotation = VaridImBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation) (VaridInner k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (VaridImp k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation (StandardLinking.VaridIm k z s l lexicalAnnotation annotation)
-- | 'VaridInBase' with fewer unresolved variables, with alias linking.
type VaridIn k z s l lexicalAnnotation annotation = VaridInBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation) (VaridInner k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (VaridInf k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (VaridIns k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation (StandardLinking.VaridIn k z s l lexicalAnnotation annotation)
-- | 'VaridLeBase' with fewer unresolved variables, with alias linking.
type VaridLe k z s l lexicalAnnotation annotation = VaridLeBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation) (VaridInner k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation (StandardLinking.VaridLe k z s l lexicalAnnotation annotation)
-- | 'VaridMoBase' with fewer unresolved variables, with alias linking.
type VaridMo k z s l lexicalAnnotation annotation = VaridMoBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation) (VaridInner k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (VaridMod k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation (StandardLinking.VaridMo k z s l lexicalAnnotation annotation)
-- | 'VaridNeBase' with fewer unresolved variables, with alias linking.
type VaridNe k z s l lexicalAnnotation annotation = VaridNeBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation) (VaridInner k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (VaridNew k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation (StandardLinking.VaridNe k z s l lexicalAnnotation annotation)
-- | 'VaridThBase' with fewer unresolved variables, with alias linking.
type VaridTh k z s l lexicalAnnotation annotation = VaridThBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation) (VaridInner k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (VaridThe k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation (StandardLinking.VaridTh k z s l lexicalAnnotation annotation)
-- | 'VaridTyBase' with fewer unresolved variables, with alias linking.
type VaridTy k z s l lexicalAnnotation annotation = VaridTyBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation) (VaridInner k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (VaridTyp k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalZLowerKeyBase k z s)) annotation (StandardLinking.VaridTy k z s l lexicalAnnotation annotation)
-- | 'VaridWhBase' with fewer unresolved variables, with alias linking.
type VaridWh k z s l lexicalAnnotation annotation = VaridWhBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation) (VaridInner k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (VaridWhe k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation (StandardLinking.VaridWh k z s l lexicalAnnotation annotation)
-- | 'VaridCasBase' with fewer unresolved variables, with alias linking.
type VaridCas k z s l lexicalAnnotation annotation = VaridCasBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation) (VaridInner k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation (StandardLinking.VaridCas k z s l lexicalAnnotation annotation)
-- | 'VaridClaBase' with fewer unresolved variables, with alias linking.
type VaridCla k z s l lexicalAnnotation annotation = VaridClaBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation) (VaridInner k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (VaridClas k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation (StandardLinking.VaridCla k z s l lexicalAnnotation annotation)
-- | 'VaridDatBase' with fewer unresolved variables, with alias linking.
type VaridDat k z s l lexicalAnnotation annotation = VaridDatBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation) (VaridInner k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation (StandardLinking.VaridDat k z s l lexicalAnnotation annotation)
-- | 'VaridDefBase' with fewer unresolved variables, with alias linking.
type VaridDef k z s l lexicalAnnotation annotation = VaridDefBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation) (VaridInner k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalALowerKeyBase k z s)) (VaridDefa k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation (StandardLinking.VaridDef k z s l lexicalAnnotation annotation)
-- | 'VaridDerBase' with fewer unresolved variables, with alias linking.
type VaridDer k z s l lexicalAnnotation annotation = VaridDerBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation) (VaridInner k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (VaridDeri k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation (StandardLinking.VaridDer k z s l lexicalAnnotation annotation)
-- | 'VaridElsBase' with fewer unresolved variables, with alias linking.
type VaridEls k z s l lexicalAnnotation annotation = VaridElsBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation) (VaridInner k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation (StandardLinking.VaridEls k z s l lexicalAnnotation annotation)
-- | 'VaridForBase' with fewer unresolved variables, with alias linking.
type VaridFor k z s l lexicalAnnotation annotation = VaridForBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation) (VaridInner k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (VaridFore k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation (StandardLinking.VaridFor k z s l lexicalAnnotation annotation)
-- | 'VaridImpBase' with fewer unresolved variables, with alias linking.
type VaridImp k z s l lexicalAnnotation annotation = VaridImpBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation) (VaridInner k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (VaridImpo k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation (StandardLinking.VaridImp k z s l lexicalAnnotation annotation)
-- | 'VaridInfBase' with fewer unresolved variables, with alias linking.
type VaridInf k z s l lexicalAnnotation annotation = VaridInfBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation) (VaridInner k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (VaridInfi k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation (StandardLinking.VaridInf k z s l lexicalAnnotation annotation)
-- | 'VaridInsBase' with fewer unresolved variables, with alias linking.
type VaridIns k z s l lexicalAnnotation annotation = VaridInsBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation) (VaridInner k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (VaridInst k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation (StandardLinking.VaridIns k z s l lexicalAnnotation annotation)
-- | 'VaridModBase' with fewer unresolved variables, with alias linking.
type VaridMod k z s l lexicalAnnotation annotation = VaridModBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation) (VaridInner k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (VaridModu k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation (StandardLinking.VaridMod k z s l lexicalAnnotation annotation)
-- | 'VaridNewBase' with fewer unresolved variables, with alias linking.
type VaridNew k z s l lexicalAnnotation annotation = VaridNewBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation) (VaridInner k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (VaridNewt k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation (StandardLinking.VaridNew k z s l lexicalAnnotation annotation)
-- | 'VaridTheBase' with fewer unresolved variables, with alias linking.
type VaridThe k z s l lexicalAnnotation annotation = VaridTheBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation) (VaridInner k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation (StandardLinking.VaridThe k z s l lexicalAnnotation annotation)
-- | 'VaridTypBase' with fewer unresolved variables, with alias linking.
type VaridTyp k z s l lexicalAnnotation annotation = VaridTypBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation) (VaridInner k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation (StandardLinking.VaridTyp k z s l lexicalAnnotation annotation)
-- | 'VaridWheBase' with fewer unresolved variables, with alias linking.
type VaridWhe k z s l lexicalAnnotation annotation = VaridWheBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation) (VaridInner k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (VaridWher k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation (StandardLinking.VaridWhe k z s l lexicalAnnotation annotation)
-- | 'VaridClasBase' with fewer unresolved variables, with alias linking.
type VaridClas k z s l lexicalAnnotation annotation = VaridClasBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation) (VaridInner k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation (StandardLinking.VaridClas k z s l lexicalAnnotation annotation)
-- | 'VaridDefaBase' with fewer unresolved variables, with alias linking.
type VaridDefa k z s l lexicalAnnotation annotation = VaridDefaBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation) (VaridInner k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (VaridDefau k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation (StandardLinking.VaridDefa k z s l lexicalAnnotation annotation)
-- | 'VaridDeriBase' with fewer unresolved variables, with alias linking.
type VaridDeri k z s l lexicalAnnotation annotation = VaridDeriBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation) (VaridInner k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (VaridDeriv k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation (StandardLinking.VaridDeri k z s l lexicalAnnotation annotation)
-- | 'VaridForeBase' with fewer unresolved variables, with alias linking.
type VaridFore k z s l lexicalAnnotation annotation = VaridForeBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation) (VaridInner k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (VaridForei k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation (StandardLinking.VaridFore k z s l lexicalAnnotation annotation)
-- | 'VaridImpoBase' with fewer unresolved variables, with alias linking.
type VaridImpo k z s l lexicalAnnotation annotation = VaridImpoBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation) (VaridInner k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (VaridImpor k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation (StandardLinking.VaridImpo k z s l lexicalAnnotation annotation)
-- | 'VaridInfiBase' with fewer unresolved variables, with alias linking.
type VaridInfi k z s l lexicalAnnotation annotation = VaridInfiBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation) (VaridInner k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (VaridInfix k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation (StandardLinking.VaridInfi k z s l lexicalAnnotation annotation)
-- | 'VaridInstBase' with fewer unresolved variables, with alias linking.
type VaridInst k z s l lexicalAnnotation annotation = VaridInstBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation) (VaridInner k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalALowerKeyBase k z s)) (VaridInsta k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation (StandardLinking.VaridInst k z s l lexicalAnnotation annotation)
-- | 'VaridModuBase' with fewer unresolved variables, with alias linking.
type VaridModu k z s l lexicalAnnotation annotation = VaridModuBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation) (VaridInner k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (VaridModul k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation (StandardLinking.VaridModu k z s l lexicalAnnotation annotation)
-- | 'VaridNewtBase' with fewer unresolved variables, with alias linking.
type VaridNewt k z s l lexicalAnnotation annotation = VaridNewtBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation) (VaridInner k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (VaridNewty k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalZLowerKeyBase k z s)) annotation (StandardLinking.VaridNewt k z s l lexicalAnnotation annotation)
-- | 'VaridWherBase' with fewer unresolved variables, with alias linking.
type VaridWher k z s l lexicalAnnotation annotation = VaridWherBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation) (VaridInner k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation (StandardLinking.VaridWher k z s l lexicalAnnotation annotation)
-- | 'VaridDefauBase' with fewer unresolved variables, with alias linking.
type VaridDefau k z s l lexicalAnnotation annotation = VaridDefauBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation) (VaridInner k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (VaridDefaul k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation (StandardLinking.VaridDefau k z s l lexicalAnnotation annotation)
-- | 'VaridDerivBase' with fewer unresolved variables, with alias linking.
type VaridDeriv k z s l lexicalAnnotation annotation = VaridDerivBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation) (VaridInner k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (VaridDerivi k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation (StandardLinking.VaridDeriv k z s l lexicalAnnotation annotation)
-- | 'VaridForeiBase' with fewer unresolved variables, with alias linking.
type VaridForei k z s l lexicalAnnotation annotation = VaridForeiBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation) (VaridInner k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (VaridForeig k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation (StandardLinking.VaridForei k z s l lexicalAnnotation annotation)
-- | 'VaridImporBase' with fewer unresolved variables, with alias linking.
type VaridImpor k z s l lexicalAnnotation annotation = VaridImporBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation) (VaridInner k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation (StandardLinking.VaridImpor k z s l lexicalAnnotation annotation)
-- | 'VaridInfixBase' with fewer unresolved variables, with alias linking.
type VaridInfix k z s l lexicalAnnotation annotation = VaridInfixBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation) (VaridInner k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation (StandardLinking.VaridInfix k z s l lexicalAnnotation annotation)
-- | 'VaridInstaBase' with fewer unresolved variables, with alias linking.
type VaridInsta k z s l lexicalAnnotation annotation = VaridInstaBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation) (VaridInner k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (VaridInstan k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation (StandardLinking.VaridInsta k z s l lexicalAnnotation annotation)
-- | 'VaridModulBase' with fewer unresolved variables, with alias linking.
type VaridModul k z s l lexicalAnnotation annotation = VaridModulBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation) (VaridInner k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation (StandardLinking.VaridModul k z s l lexicalAnnotation annotation)
-- | 'VaridNewtyBase' with fewer unresolved variables, with alias linking.
type VaridNewty k z s l lexicalAnnotation annotation = VaridNewtyBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation) (VaridInner k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (VaridNewtyp k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation (StandardLinking.VaridNewty k z s l lexicalAnnotation annotation)
-- | 'VaridDefaulBase' with fewer unresolved variables, with alias linking.
type VaridDefaul k z s l lexicalAnnotation annotation = VaridDefaulBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation) (VaridInner k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation (StandardLinking.VaridDefaul k z s l lexicalAnnotation annotation)
-- | 'VaridDeriviBase' with fewer unresolved variables, with alias linking.
type VaridDerivi k z s l lexicalAnnotation annotation = VaridDeriviBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation) (VaridInner k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (VaridDerivin k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation (StandardLinking.VaridDerivi k z s l lexicalAnnotation annotation)
-- | 'VaridForeigBase' with fewer unresolved variables, with alias linking.
type VaridForeig k z s l lexicalAnnotation annotation = VaridForeigBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation) (VaridInner k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation (StandardLinking.VaridForeig k z s l lexicalAnnotation annotation)
-- | 'VaridInstanBase' with fewer unresolved variables, with alias linking.
type VaridInstan k z s l lexicalAnnotation annotation = VaridInstanBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation) (VaridInner k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (VaridInstanc k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation (StandardLinking.VaridInstan k z s l lexicalAnnotation annotation)
-- | 'VaridNewtypBase' with fewer unresolved variables, with alias linking.
type VaridNewtyp k z s l lexicalAnnotation annotation = VaridNewtypBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation) (VaridInner k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation (StandardLinking.VaridNewtyp k z s l lexicalAnnotation annotation)
-- | 'VaridDerivinBase' with fewer unresolved variables, with alias linking.
type VaridDerivin k z s l lexicalAnnotation annotation = VaridDerivinBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation) (VaridInner k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation (StandardLinking.VaridDerivin k z s l lexicalAnnotation annotation)
-- | 'VaridInstancBase' with fewer unresolved variables, with alias linking.
type VaridInstanc k z s l lexicalAnnotation annotation = VaridInstancBase [] (l (LexicalEndOfParseKeyBase k z s)) (VaridInnerSansAscSmallUnderscore k z s l lexicalAnnotation lexicalAnnotation) (VaridInner k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalALowerKeyBase k z s)) (l (LexicalBLowerKeyBase k z s)) (l (LexicalCLowerKeyBase k z s)) (l (LexicalDLowerKeyBase k z s)) (l (LexicalELowerKeyBase k z s)) (l (LexicalFLowerKeyBase k z s)) (l (LexicalGLowerKeyBase k z s)) (l (LexicalHLowerKeyBase k z s)) (l (LexicalILowerKeyBase k z s)) (l (LexicalJLowerKeyBase k z s)) (l (LexicalKLowerKeyBase k z s)) (l (LexicalLLowerKeyBase k z s)) (l (LexicalMLowerKeyBase k z s)) (l (LexicalNLowerKeyBase k z s)) (l (LexicalOLowerKeyBase k z s)) (l (LexicalPLowerKeyBase k z s)) (l (LexicalQLowerKeyBase k z s)) (l (LexicalRLowerKeyBase k z s)) (l (LexicalSLowerKeyBase k z s)) (l (LexicalTLowerKeyBase k z s)) (l (LexicalULowerKeyBase k z s)) (l (LexicalVLowerKeyBase k z s)) (l (LexicalWLowerKeyBase k z s)) (l (LexicalXLowerKeyBase k z s)) (l (LexicalYLowerKeyBase k z s)) (l (LexicalZLowerKeyBase k z s)) annotation (StandardLinking.VaridInstanc k z s l lexicalAnnotation annotation)
-- | 'SymbolSansAscBase' with fewer unresolved variables, with alias linking.
type SymbolSansAsc k z s l lexicalAnnotation annotation = SymbolSansAscBase (UniSymbolSansSpecialishAsc k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.SymbolSansAsc k z s l lexicalAnnotation annotation)
-- | 'UniSymbolSansSpecialishAscBase' with fewer unresolved variables, with alias linking.
type UniSymbolSansSpecialishAsc k z s l lexicalAnnotation annotation = UniSymbolSansSpecialishAscBase (l (LexicalUnicodeSymbolSansSpecialUnderscoreDoubleQuoteSingleQuoteAsciiKeyBase k z s)) annotation (StandardLinking.UniSymbolSansSpecialishAsc k z s l lexicalAnnotation annotation)
-- | 'SymbolSansHyphenBase' with fewer unresolved variables, with alias linking.
type SymbolSansHyphen k z s l lexicalAnnotation annotation = SymbolSansHyphenBase (AscSymbolSansHyphen k z s l lexicalAnnotation lexicalAnnotation) (UniSymbolSansSpecialishHyphen k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.SymbolSansHyphen k z s l lexicalAnnotation annotation)
-- | 'AscSymbolSansHyphenBase' with fewer unresolved variables, with alias linking.
type AscSymbolSansHyphen k z s l lexicalAnnotation annotation = AscSymbolSansHyphenBase (l (LexicalExclamationKeyBase k z s)) (l (LexicalHashKeyBase k z s)) (l (LexicalDollarKeyBase k z s)) (l (LexicalPercentKeyBase k z s)) (l (LexicalAmpersandKeyBase k z s)) (l (LexicalAsteriskKeyBase k z s)) (l (LexicalPlusKeyBase k z s)) (l (LexicalDotKeyBase k z s)) (l (LexicalSlashKeyBase k z s)) (l (LexicalLeftAngleBracketKeyBase k z s)) (l (LexicalEqualsKeyBase k z s)) (l (LexicalRightAngleBracketKeyBase k z s)) (l (LexicalQuestionMarkKeyBase k z s)) (l (LexicalAtKeyBase k z s)) (l (LexicalBackslashKeyBase k z s)) (l (LexicalCaretKeyBase k z s)) (l (LexicalPipeKeyBase k z s)) (l (LexicalTildeKeyBase k z s)) (l (LexicalColonKeyBase k z s)) annotation (StandardLinking.AscSymbolSansHyphen k z s l lexicalAnnotation annotation)
-- | 'UniSymbolSansSpecialishHyphenBase' with fewer unresolved variables, with alias linking.
type UniSymbolSansSpecialishHyphen k z s l lexicalAnnotation annotation = UniSymbolSansSpecialishHyphenBase (l (LexicalUnicodeSymbolSansSpecialUnderscoreDoubleQuoteSingleQuoteHyphenKeyBase k z s)) annotation (StandardLinking.UniSymbolSansSpecialishHyphen k z s l lexicalAnnotation annotation)
-- | 'VarSymBase' with fewer unresolved variables, with alias linking.
type VarSym k z s l lexicalAnnotation annotation = VarSymBase [] (SymbolSansAsc k z s l lexicalAnnotation lexicalAnnotation) (Symbol k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalExclamationKeyBase k z s)) (l (LexicalHashKeyBase k z s)) (l (LexicalDollarKeyBase k z s)) (l (LexicalPercentKeyBase k z s)) (l (LexicalAmpersandKeyBase k z s)) (l (LexicalAsteriskKeyBase k z s)) (l (LexicalPlusKeyBase k z s)) (l (LexicalDotKeyBase k z s)) (l (LexicalSlashKeyBase k z s)) (l (LexicalLeftAngleBracketKeyBase k z s)) (l (LexicalRightAngleBracketKeyBase k z s)) (l (LexicalQuestionMarkKeyBase k z s)) (l (LexicalCaretKeyBase k z s)) (l (LexicalHyphenKeyBase k z s)) (l (LexicalEqualsKeyBase k z s)) (l (LexicalAtKeyBase k z s)) (l (LexicalBackslashKeyBase k z s)) (l (LexicalPipeKeyBase k z s)) (l (LexicalTildeKeyBase k z s)) (l (LexicalColonKeyBase k z s)) (SymbolSansHyphen k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.VarSym k z s l lexicalAnnotation annotation)
-- | 'VarSymSansMinusBase' with fewer unresolved variables, with alias linking.
type VarSymSansMinus k z s l lexicalAnnotation annotation = VarSymSansMinusBase [] (SymbolSansAsc k z s l lexicalAnnotation lexicalAnnotation) (Symbol k z s l lexicalAnnotation lexicalAnnotation) (l (LexicalExclamationKeyBase k z s)) (l (LexicalHashKeyBase k z s)) (l (LexicalDollarKeyBase k z s)) (l (LexicalPercentKeyBase k z s)) (l (LexicalAmpersandKeyBase k z s)) (l (LexicalAsteriskKeyBase k z s)) (l (LexicalPlusKeyBase k z s)) (l (LexicalDotKeyBase k z s)) (l (LexicalSlashKeyBase k z s)) (l (LexicalLeftAngleBracketKeyBase k z s)) (l (LexicalRightAngleBracketKeyBase k z s)) (l (LexicalQuestionMarkKeyBase k z s)) (l (LexicalCaretKeyBase k z s)) (l (LexicalHyphenKeyBase k z s)) (l (LexicalEqualsKeyBase k z s)) (l (LexicalAtKeyBase k z s)) (l (LexicalBackslashKeyBase k z s)) (l (LexicalPipeKeyBase k z s)) (l (LexicalTildeKeyBase k z s)) (l (LexicalColonKeyBase k z s)) (SymbolSansHyphen k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.VarSymSansMinus k z s l lexicalAnnotation annotation)
-- | 'SymbolSansColonBase' with fewer unresolved variables, with alias linking.
type SymbolSansColon k z s l lexicalAnnotation annotation = SymbolSansColonBase (AscSymbolSansColon k z s l lexicalAnnotation lexicalAnnotation) (UniSymbolSansSpecialishColon k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.SymbolSansColon k z s l lexicalAnnotation annotation)
-- | 'AscSymbolSansColonBase' with fewer unresolved variables, with alias linking.
type AscSymbolSansColon k z s l lexicalAnnotation annotation = AscSymbolSansColonBase (l (LexicalExclamationKeyBase k z s)) (l (LexicalHashKeyBase k z s)) (l (LexicalDollarKeyBase k z s)) (l (LexicalPercentKeyBase k z s)) (l (LexicalAmpersandKeyBase k z s)) (l (LexicalAsteriskKeyBase k z s)) (l (LexicalPlusKeyBase k z s)) (l (LexicalDotKeyBase k z s)) (l (LexicalSlashKeyBase k z s)) (l (LexicalLeftAngleBracketKeyBase k z s)) (l (LexicalEqualsKeyBase k z s)) (l (LexicalRightAngleBracketKeyBase k z s)) (l (LexicalQuestionMarkKeyBase k z s)) (l (LexicalAtKeyBase k z s)) (l (LexicalBackslashKeyBase k z s)) (l (LexicalCaretKeyBase k z s)) (l (LexicalPipeKeyBase k z s)) (l (LexicalHyphenKeyBase k z s)) (l (LexicalTildeKeyBase k z s)) annotation (StandardLinking.AscSymbolSansColon k z s l lexicalAnnotation annotation)
-- | 'UniSymbolSansSpecialishColonBase' with fewer unresolved variables, with alias linking.
type UniSymbolSansSpecialishColon k z s l lexicalAnnotation annotation = UniSymbolSansSpecialishColonBase (l (LexicalUnicodeSymbolSansSpecialUnderscoreDoubleQuoteSingleQuoteColonKeyBase k z s)) annotation (StandardLinking.UniSymbolSansSpecialishColon k z s l lexicalAnnotation annotation)
-- | 'ConSymBase' with fewer unresolved variables, with alias linking.
type ConSym k z s l lexicalAnnotation annotation = ConSymBase [] (l (LexicalColonKeyBase k z s)) (Symbol k z s l lexicalAnnotation lexicalAnnotation) (SymbolSansColon k z s l lexicalAnnotation lexicalAnnotation) annotation (StandardLinking.ConSym k z s l lexicalAnnotation annotation)
