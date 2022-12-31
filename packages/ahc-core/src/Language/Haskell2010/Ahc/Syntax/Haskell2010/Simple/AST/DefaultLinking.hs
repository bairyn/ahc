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

	-- ** Intra-module and inter-module linking.

	-- *** Lexical Foundation structures.

	LexicalFoundation,

	-- *** Lexical structures.

	-- **** Symbolic alias pseudo-lexical structures.

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

	-- **** Alias pseudo-lexical structures.
	LexicalAliasF(MkLexicalAliasF, _unLexicalAliasF),
	LexicalAlias,
	LexicalSpaceF(MkLexicalSpaceF, _unLexicalSpaceF),
	LexicalSpace,
	LexicalMinusF(MkLexicalMinusF, _unLexicalMinusF),
	LexicalMinus,
	LexicalAsciiLambdaF(MkLexicalAsciiLambdaF, _unLexicalAsciiLambdaF),
	LexicalAsciiLambda,

	-- **** Non-symbolic numeric literal prefix pseudo-lexical structures.

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

	-- *** Grammatical structures.

	-- TODO
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

-- TODO

--type FooF = Aoeu
--type Foo = Fixed.Fix FooF

-- Grammatical structures.

-- TODO

--type ModuleF blah blah blah = ModuleBase Prelude.Maybe () () () () () () () ()
--data ModuleBase maybe lexicalModule modid exports lexicalWhere body annotation fixpoint =

-- TODO
