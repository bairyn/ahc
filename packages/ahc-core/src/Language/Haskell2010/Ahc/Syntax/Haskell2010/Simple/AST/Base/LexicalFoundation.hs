-- vim: set filetype=haskell noet

{-
 - LexicalFoundation.hs
 -
 - Haskell2010 Simple syntax AST: abstraction layer over base characters.  When
 - implemented by instantiating it, the AST becomes available.  This enables
 - supporting all the terminals.
 -}

{-# LANGUAGE Haskell2010 #-}

-- | Haskell2010 Simple syntax AST: lexical foundation.
--
-- This module provides an abstraction layer over base characters.  When
-- implemented by instantiating it, the AST becomes available.  This enables
-- supporting all the terminals.
module Language.Haskell2010.Ahc.Syntax.Haskell2010.Simple.AST.Base.LexicalFoundation (
	-- Export everything, explicitly.

	-- * Base structures.

	-- ** Base lexical structures.

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
-- ‘lexicalTypeIndexer (lexicalEndOfParseKey lexicalTypeIndexerKey z s)’ is
-- expected to correspond to the actual ‘lexicalEndOfParseKey’ type itself.
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
