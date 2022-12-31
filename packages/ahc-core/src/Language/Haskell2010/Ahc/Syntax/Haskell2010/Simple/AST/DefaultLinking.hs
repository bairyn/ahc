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
-- building up the module instantiations:
-- - lexicalAnnotation: all section 2 structures.
-- - grammarAnnotation: all section 3-5 structures.
-- - TODO: TODO update this part.
--
-- Finally, this module uses 'Language.Haskell2010.Ahc.Data.Fixed.Fixed' to
-- wrap each structure to provide F-algebra style recursion.  (This is not
-- directly provided by the Haskell2010 specification, which would have been a
-- convenient Schelling point.)
module Language.Haskell2010.Ahc.Syntax.Haskell2010.Simple.AST.DefaultLinking (
	-- Export everything, explicitly.

	-- * Structures with default linking.

	-- ** Intra-module and inter-module linking.

	-- *** Lexical Foundation structures.

	LexicalFoundation,

	-- *** Lexical structures.

	-- **** Non-symbolic numeric literal prefix pseudo-lexical structures.

	Lexical0XF(MkLexical0XF, _unLexical0XF),
	Lexical0x,

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

-- Non-symbolic numeric literal prefix pseudo-lexical structures.

-- | TODO
newtype Lexical0XF k z s l lexicalAnnotation fixpoint = MkLexical0XF { _unLexical0XF :: (Lexical0XBase (l (Lexical0KeyBase k z s)) (l (LexicalXKeyBase k z s)) lexicalAnnotation fixpoint) }
-- | TODO
type Lexical0x k z s l lexicalAnnotation = Fixed.Fix (Lexical0XF k z s l lexicalAnnotation)

-- TODO

--type FooF = Aoeu
--type Foo = Fixed.Fix FooF

-- Grammatical structures.

-- TODO

--type ModuleF blah blah blah = ModuleBase Prelude.Maybe () () () () () () () ()
--data ModuleBase maybe lexicalModule modid exports lexicalWhere body annotation fixpoint =

-- TODO
