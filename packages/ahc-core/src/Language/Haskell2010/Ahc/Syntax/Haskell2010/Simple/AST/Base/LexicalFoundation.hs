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

	LexicalEndOfParseKeyBase(MkLexicalEndOfParseKey, _unLexicalEndOfParseKey),

	LexicalUnicodeDigitKeyBase(MkLexicalUnicodeDigitKey, _unLexicalUnicodeDigitKey),
	LexicalUnicodeLargeKeyBase(MkLexicalUnicodeLargeKey, _unLexicalUnicodeLargeKey),
	LexicalUnicodeSmallKeyBase(MkLexicalUnicodeSmallKey, _unLexicalUnicodeSmallKey),
	LexicalUnicodeSymbolKeyBase(MkLexicalUnicodeSymbolKey, _unLexicalUnicodeSymbolKey),
	LexicalUnicodeWhitespaceCharKeyBase(MkLexicalUnicodeWhitespaceCharKey, _unLexicalUnicodeWhitespaceCharKey),

	LexicalUnicodeSmallSansAscUnderscoreKeyBase(MkLexicalUnicodeSmallSansAscUnderscoreKey, _unLexicalUnicodeSmallSansAscUnderscoreKey),
	LexicalUnicodeSymbolSansSpecialUnderscoreDoubleQuoteSingleQuoteKeyBase(MkLexicalUnicodeSymbolSansSpecialUnderscoreDoubleQuoteSingleQuoteKey, _unLexicalUnicodeSymbolSansSpecialUnderscoreDoubleQuoteSingleQuoteKey),
	LexicalUnicodeSymbolSansSpecialUnderscoreDoubleQuoteSingleQuoteAsciiKeyBase(MkLexicalUnicodeSymbolSansSpecialUnderscoreDoubleQuoteSingleQuoteAsciiKey, _unLexicalUnicodeSymbolSansSpecialUnderscoreDoubleQuoteSingleQuoteAsciiKey),
	LexicalUnicodeSymbolSansSpecialUnderscoreDoubleQuoteSingleQuoteColonKeyBase(MkLexicalUnicodeSymbolSansSpecialUnderscoreDoubleQuoteSingleQuoteColonKey, _unLexicalUnicodeSymbolSansSpecialUnderscoreDoubleQuoteSingleQuoteColonKey),
	LexicalUnicodeSymbolSansSpecialUnderscoreDoubleQuoteSingleQuoteHyphenKeyBase(MkLexicalUnicodeSymbolSansSpecialUnderscoreDoubleQuoteSingleQuoteHyphenKey, _unLexicalUnicodeSymbolSansSpecialUnderscoreDoubleQuoteSingleQuoteHyphenKey),

	LexicalNULKeyBase(MkLexicalNULKey, _unLexicalNULKey),
	LexicalSOHKeyBase(MkLexicalSOHKey, _unLexicalSOHKey),
	LexicalSTXKeyBase(MkLexicalSTXKey, _unLexicalSTXKey),
	LexicalETXKeyBase(MkLexicalETXKey, _unLexicalETXKey),
	LexicalEOTKeyBase(MkLexicalEOTKey, _unLexicalEOTKey),
	LexicalENQKeyBase(MkLexicalENQKey, _unLexicalENQKey),
	LexicalACKKeyBase(MkLexicalACKKey, _unLexicalACKKey),
	LexicalBELKeyBase(MkLexicalBELKey, _unLexicalBELKey),
	LexicalBSKeyBase(MkLexicalBSKey, _unLexicalBSKey),
	LexicalHTKeyBase(MkLexicalHTKey, _unLexicalHTKey),
	LexicalLFKeyBase(MkLexicalLFKey, _unLexicalLFKey),
	LexicalVTKeyBase(MkLexicalVTKey, _unLexicalVTKey),
	LexicalFFKeyBase(MkLexicalFFKey, _unLexicalFFKey),
	LexicalCRKeyBase(MkLexicalCRKey, _unLexicalCRKey),
	LexicalSOKeyBase(MkLexicalSOKey, _unLexicalSOKey),
	LexicalSIKeyBase(MkLexicalSIKey, _unLexicalSIKey),
	LexicalDLEKeyBase(MkLexicalDLEKey, _unLexicalDLEKey),
	LexicalDC1KeyBase(MkLexicalDC1Key, _unLexicalDC1Key),
	LexicalDC2KeyBase(MkLexicalDC2Key, _unLexicalDC2Key),
	LexicalDC3KeyBase(MkLexicalDC3Key, _unLexicalDC3Key),
	LexicalDC4KeyBase(MkLexicalDC4Key, _unLexicalDC4Key),
	LexicalNAKKeyBase(MkLexicalNAKKey, _unLexicalNAKKey),
	LexicalSYNKeyBase(MkLexicalSYNKey, _unLexicalSYNKey),
	LexicalETBKeyBase(MkLexicalETBKey, _unLexicalETBKey),
	LexicalCANKeyBase(MkLexicalCANKey, _unLexicalCANKey),
	LexicalEMKeyBase(MkLexicalEMKey, _unLexicalEMKey),
	LexicalSUBKeyBase(MkLexicalSUBKey, _unLexicalSUBKey),
	LexicalESCKeyBase(MkLexicalESCKey, _unLexicalESCKey),
	LexicalFSKeyBase(MkLexicalFSKey, _unLexicalFSKey),
	LexicalGSKeyBase(MkLexicalGSKey, _unLexicalGSKey),
	LexicalRSKeyBase(MkLexicalRSKey, _unLexicalRSKey),
	LexicalUSKeyBase(MkLexicalUSKey, _unLexicalUSKey),

	LexicalSPKeyBase(MkLexicalSPKey, _unLexicalSPKey),
	LexicalExclamationKeyBase(MkLexicalExclamationKey, _unLexicalExclamationKey),
	LexicalDoubleQuoteKeyBase(MkLexicalDoubleQuoteKey, _unLexicalDoubleQuoteKey),
	LexicalHashKeyBase(MkLexicalHashKey, _unLexicalHashKey),
	LexicalDollarKeyBase(MkLexicalDollarKey, _unLexicalDollarKey),
	LexicalPercentKeyBase(MkLexicalPercentKey, _unLexicalPercentKey),
	LexicalAmpersandKeyBase(MkLexicalAmpersandKey, _unLexicalAmpersandKey),
	LexicalSingleQuoteKeyBase(MkLexicalSingleQuoteKey, _unLexicalSingleQuoteKey),
	LexicalLeftParenthesisKeyBase(MkLexicalLeftParenthesisKey, _unLexicalLeftParenthesisKey),
	LexicalRightParenthesisKeyBase(MkLexicalRightParenthesisKey, _unLexicalRightParenthesisKey),
	LexicalAsteriskKeyBase(MkLexicalAsteriskKey, _unLexicalAsteriskKey),
	LexicalPlusKeyBase(MkLexicalPlusKey, _unLexicalPlusKey),
	LexicalCommaKeyBase(MkLexicalCommaKey, _unLexicalCommaKey),
	LexicalHyphenKeyBase(MkLexicalHyphenKey, _unLexicalHyphenKey),
	LexicalDotKeyBase(MkLexicalDotKey, _unLexicalDotKey),
	LexicalSlashKeyBase(MkLexicalSlashKey, _unLexicalSlashKey),

	Lexical0KeyBase(MkLexical0Key, _unLexical0Key),
	Lexical1KeyBase(MkLexical1Key, _unLexical1Key),
	Lexical2KeyBase(MkLexical2Key, _unLexical2Key),
	Lexical3KeyBase(MkLexical3Key, _unLexical3Key),
	Lexical4KeyBase(MkLexical4Key, _unLexical4Key),
	Lexical5KeyBase(MkLexical5Key, _unLexical5Key),
	Lexical6KeyBase(MkLexical6Key, _unLexical6Key),
	Lexical7KeyBase(MkLexical7Key, _unLexical7Key),
	Lexical8KeyBase(MkLexical8Key, _unLexical8Key),
	Lexical9KeyBase(MkLexical9Key, _unLexical9Key),

	LexicalColonKeyBase(MkLexicalColonKey, _unLexicalColonKey),
	LexicalSemicolonKeyBase(MkLexicalSemicolonKey, _unLexicalSemicolonKey),
	LexicalLeftAngleBracketKeyBase(MkLexicalLeftAngleBracketKey, _unLexicalLeftAngleBracketKey),
	LexicalEqualsKeyBase(MkLexicalEqualsKey, _unLexicalEqualsKey),
	LexicalRightAngleBracketKeyBase(MkLexicalRightAngleBracketKey, _unLexicalRightAngleBracketKey),
	LexicalQuestionMarkKeyBase(MkLexicalQuestionMarkKey, _unLexicalQuestionMarkKey),
	LexicalAtKeyBase(MkLexicalAtKey, _unLexicalAtKey),

	LexicalAKeyBase(MkLexicalAKey, _unLexicalAKey),
	LexicalBKeyBase(MkLexicalBKey, _unLexicalBKey),
	LexicalCKeyBase(MkLexicalCKey, _unLexicalCKey),
	LexicalDKeyBase(MkLexicalDKey, _unLexicalDKey),
	LexicalEKeyBase(MkLexicalEKey, _unLexicalEKey),
	LexicalFKeyBase(MkLexicalFKey, _unLexicalFKey),
	LexicalGKeyBase(MkLexicalGKey, _unLexicalGKey),
	LexicalHKeyBase(MkLexicalHKey, _unLexicalHKey),
	LexicalIKeyBase(MkLexicalIKey, _unLexicalIKey),
	LexicalJKeyBase(MkLexicalJKey, _unLexicalJKey),
	LexicalKKeyBase(MkLexicalKKey, _unLexicalKKey),
	LexicalLKeyBase(MkLexicalLKey, _unLexicalLKey),
	LexicalMKeyBase(MkLexicalMKey, _unLexicalMKey),
	LexicalNKeyBase(MkLexicalNKey, _unLexicalNKey),
	LexicalOKeyBase(MkLexicalOKey, _unLexicalOKey),
	LexicalPKeyBase(MkLexicalPKey, _unLexicalPKey),
	LexicalQKeyBase(MkLexicalQKey, _unLexicalQKey),
	LexicalRKeyBase(MkLexicalRKey, _unLexicalRKey),
	LexicalSKeyBase(MkLexicalSKey, _unLexicalSKey),
	LexicalTKeyBase(MkLexicalTKey, _unLexicalTKey),
	LexicalUKeyBase(MkLexicalUKey, _unLexicalUKey),
	LexicalVKeyBase(MkLexicalVKey, _unLexicalVKey),
	LexicalWKeyBase(MkLexicalWKey, _unLexicalWKey),
	LexicalXKeyBase(MkLexicalXKey, _unLexicalXKey),
	LexicalYKeyBase(MkLexicalYKey, _unLexicalYKey),
	LexicalZKeyBase(MkLexicalZKey, _unLexicalZKey),

	LexicalLeftBracketKeyBase(MkLexicalLeftBracketKey, _unLexicalLeftBracketKey),
	LexicalBackslashKeyBase(MkLexicalBackslashKey, _unLexicalBackslashKey),
	LexicalRightBracketKeyBase(MkLexicalRightBracketKey, _unLexicalRightBracketKey),
	LexicalCaretKeyBase(MkLexicalCaretKey, _unLexicalCaretKey),
	LexicalUnderscoreKeyBase(MkLexicalUnderscoreKey, _unLexicalUnderscoreKey),
	LexicalBacktickKeyBase(MkLexicalBacktickKey, _unLexicalBacktickKey),

	LexicalALowerKeyBase(MkLexicalALowerKey, _unLexicalALowerKey),
	LexicalBLowerKeyBase(MkLexicalBLowerKey, _unLexicalBLowerKey),
	LexicalCLowerKeyBase(MkLexicalCLowerKey, _unLexicalCLowerKey),
	LexicalDLowerKeyBase(MkLexicalDLowerKey, _unLexicalDLowerKey),
	LexicalELowerKeyBase(MkLexicalELowerKey, _unLexicalELowerKey),
	LexicalFLowerKeyBase(MkLexicalFLowerKey, _unLexicalFLowerKey),
	LexicalGLowerKeyBase(MkLexicalGLowerKey, _unLexicalGLowerKey),
	LexicalHLowerKeyBase(MkLexicalHLowerKey, _unLexicalHLowerKey),
	LexicalILowerKeyBase(MkLexicalILowerKey, _unLexicalILowerKey),
	LexicalJLowerKeyBase(MkLexicalJLowerKey, _unLexicalJLowerKey),
	LexicalKLowerKeyBase(MkLexicalKLowerKey, _unLexicalKLowerKey),
	LexicalLLowerKeyBase(MkLexicalLLowerKey, _unLexicalLLowerKey),
	LexicalMLowerKeyBase(MkLexicalMLowerKey, _unLexicalMLowerKey),
	LexicalNLowerKeyBase(MkLexicalNLowerKey, _unLexicalNLowerKey),
	LexicalOLowerKeyBase(MkLexicalOLowerKey, _unLexicalOLowerKey),
	LexicalPLowerKeyBase(MkLexicalPLowerKey, _unLexicalPLowerKey),
	LexicalQLowerKeyBase(MkLexicalQLowerKey, _unLexicalQLowerKey),
	LexicalRLowerKeyBase(MkLexicalRLowerKey, _unLexicalRLowerKey),
	LexicalSLowerKeyBase(MkLexicalSLowerKey, _unLexicalSLowerKey),
	LexicalTLowerKeyBase(MkLexicalTLowerKey, _unLexicalTLowerKey),
	LexicalULowerKeyBase(MkLexicalULowerKey, _unLexicalULowerKey),
	LexicalVLowerKeyBase(MkLexicalVLowerKey, _unLexicalVLowerKey),
	LexicalWLowerKeyBase(MkLexicalWLowerKey, _unLexicalWLowerKey),
	LexicalXLowerKeyBase(MkLexicalXLowerKey, _unLexicalXLowerKey),
	LexicalYLowerKeyBase(MkLexicalYLowerKey, _unLexicalYLowerKey),
	LexicalZLowerKeyBase(MkLexicalZLowerKey, _unLexicalZLowerKey),

	LexicalLeftBraceKeyBase(MkLexicalLeftBraceKey, _unLexicalLeftBraceKey),
	LexicalPipeKeyBase(MkLexicalPipeKey, _unLexicalPipeKey),
	LexicalRightBraceKeyBase(MkLexicalRightBraceKey, _unLexicalRightBraceKey),
	LexicalTildeKeyBase(MkLexicalTildeKey, _unLexicalTildeKey),

	LexicalDELKeyBase(MkLexicalDELKey, _unLexicalDELKey),

	LexicalFoundationBase(
		LexicalFoundationPrimitives,

		_lexicalFoundationPrimitives_ignoredTypeInferenceFixer                                             ,

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
newtype LexicalEndOfParseKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalEndOfParseKey { _unLexicalEndOfParseKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) z) }

-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalUnicodeDigitKey’: index (for 137) 1.
newtype LexicalUnicodeDigitKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalUnicodeDigitKey { _unLexicalUnicodeDigitKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s z)) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalUnicodeLargeKey’: index (for 136) 2.
newtype LexicalUnicodeLargeKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalUnicodeLargeKey { _unLexicalUnicodeLargeKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s z))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalUnicodeSmallKey’: index (for 135) 3.
newtype LexicalUnicodeSmallKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalUnicodeSmallKey { _unLexicalUnicodeSmallKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s z)))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalUnicodeSymbolKey’: index (for 134) 4.
newtype LexicalUnicodeSymbolKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalUnicodeSymbolKey { _unLexicalUnicodeSymbolKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s z))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalUnicodeWhitespaceCharKey’: index (for 133) 5.
newtype LexicalUnicodeWhitespaceCharKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalUnicodeWhitespaceCharKey { _unLexicalUnicodeWhitespaceCharKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s z)))))) }

-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalUnicodeSmallSansAscUnderscoreKey’: index (for 132) 6.
newtype LexicalUnicodeSmallSansAscUnderscoreKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalUnicodeSmallSansAscUnderscoreKey { _unLexicalUnicodeSmallSansAscUnderscoreKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s z))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalUnicodeSymbolSansSpecialUnderscoreDoubleQuoteSingleQuoteKey’: index (for 131) 7.
newtype LexicalUnicodeSymbolSansSpecialUnderscoreDoubleQuoteSingleQuoteKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalUnicodeSymbolSansSpecialUnderscoreDoubleQuoteSingleQuoteKey { _unLexicalUnicodeSymbolSansSpecialUnderscoreDoubleQuoteSingleQuoteKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s z)))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalUnicodeSymbolSansSpecialUnderscoreDoubleQuoteSingleQuoteAsciiKey’: index (for 130) 8.
newtype LexicalUnicodeSymbolSansSpecialUnderscoreDoubleQuoteSingleQuoteAsciiKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalUnicodeSymbolSansSpecialUnderscoreDoubleQuoteSingleQuoteAsciiKey { _unLexicalUnicodeSymbolSansSpecialUnderscoreDoubleQuoteSingleQuoteAsciiKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s z))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalUnicodeSymbolSansSpecialUnderscoreDoubleQuoteSingleQuoteColonKey’: index (for 129) 9.
newtype LexicalUnicodeSymbolSansSpecialUnderscoreDoubleQuoteSingleQuoteColonKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalUnicodeSymbolSansSpecialUnderscoreDoubleQuoteSingleQuoteColonKey { _unLexicalUnicodeSymbolSansSpecialUnderscoreDoubleQuoteSingleQuoteColonKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s z)))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalUnicodeSymbolSansSpecialUnderscoreDoubleQuoteSingleQuoteHyphenKey’: index (for 128) 10.
newtype LexicalUnicodeSymbolSansSpecialUnderscoreDoubleQuoteSingleQuoteHyphenKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalUnicodeSymbolSansSpecialUnderscoreDoubleQuoteSingleQuoteHyphenKey { _unLexicalUnicodeSymbolSansSpecialUnderscoreDoubleQuoteSingleQuoteHyphenKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s z))))))))))) }

-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalNULKey’: index (for 127) 11.
newtype LexicalNULKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalNULKey { _unLexicalNULKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s z)))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalSOHKey’: index (for 126) 12.
newtype LexicalSOHKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalSOHKey { _unLexicalSOHKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalSTXKey’: index (for 125) 13.
newtype LexicalSTXKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalSTXKey { _unLexicalSTXKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalETXKey’: index (for 124) 14.
newtype LexicalETXKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalETXKey { _unLexicalETXKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalEOTKey’: index (for 123) 15.
newtype LexicalEOTKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalEOTKey { _unLexicalEOTKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalENQKey’: index (for 122) 16.
newtype LexicalENQKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalENQKey { _unLexicalENQKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalACKKey’: index (for 121) 17.
newtype LexicalACKKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalACKKey { _unLexicalACKKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalBELKey’: index (for 120) 18.
newtype LexicalBELKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalBELKey { _unLexicalBELKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalBSKey’: index (for 119) 19.
newtype LexicalBSKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalBSKey { _unLexicalBSKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalHTKey’: index (for 118) 20.
newtype LexicalHTKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalHTKey { _unLexicalHTKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalLFKey’: index (for 117) 21.
newtype LexicalLFKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalLFKey { _unLexicalLFKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalVTKey’: index (for 116) 22.
newtype LexicalVTKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalVTKey { _unLexicalVTKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalFFKey’: index (for 115) 23.
newtype LexicalFFKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalFFKey { _unLexicalFFKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalCRKey’: index (for 114) 24.
newtype LexicalCRKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalCRKey { _unLexicalCRKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalSOKey’: index (for 113) 25.
newtype LexicalSOKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalSOKey { _unLexicalSOKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalSIKey’: index (for 112) 26.
newtype LexicalSIKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalSIKey { _unLexicalSIKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalDLEKey’: index (for 111) 27.
newtype LexicalDLEKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalDLEKey { _unLexicalDLEKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalDC1Key’: index (for 110) 28.
newtype LexicalDC1KeyBase lexicalTypeIndexerKeyCons z s = MkLexicalDC1Key { _unLexicalDC1Key :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalDC2Key’: index (for 109) 29.
newtype LexicalDC2KeyBase lexicalTypeIndexerKeyCons z s = MkLexicalDC2Key { _unLexicalDC2Key :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalDC3Key’: index (for 108) 30.
newtype LexicalDC3KeyBase lexicalTypeIndexerKeyCons z s = MkLexicalDC3Key { _unLexicalDC3Key :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalDC4Key’: index (for 107) 31.
newtype LexicalDC4KeyBase lexicalTypeIndexerKeyCons z s = MkLexicalDC4Key { _unLexicalDC4Key :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalNAKKey’: index (for 106) 32.
newtype LexicalNAKKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalNAKKey { _unLexicalNAKKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalSYNKey’: index (for 105) 33.
newtype LexicalSYNKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalSYNKey { _unLexicalSYNKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalETBKey’: index (for 104) 34.
newtype LexicalETBKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalETBKey { _unLexicalETBKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalCANKey’: index (for 103) 35.
newtype LexicalCANKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalCANKey { _unLexicalCANKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalEMKey’: index (for 102) 36.
newtype LexicalEMKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalEMKey { _unLexicalEMKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalSUBKey’: index (for 101) 37.
newtype LexicalSUBKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalSUBKey { _unLexicalSUBKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalESCKey’: index (for 100) 38.
newtype LexicalESCKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalESCKey { _unLexicalESCKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalFSKey’: index (for 99) 39.
newtype LexicalFSKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalFSKey { _unLexicalFSKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalGSKey’: index (for 98) 40.
newtype LexicalGSKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalGSKey { _unLexicalGSKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalRSKey’: index (for 97) 41.
newtype LexicalRSKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalRSKey { _unLexicalRSKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalUSKey’: index (for 96) 42.
newtype LexicalUSKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalUSKey { _unLexicalUSKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))) }

-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalSPKey’: index (for 95) 43.
newtype LexicalSPKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalSPKey { _unLexicalSPKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalExclamationKey’: index (for 94) 44.
newtype LexicalExclamationKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalExclamationKey { _unLexicalExclamationKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalDoubleQuoteKey’: index (for 93) 45.
newtype LexicalDoubleQuoteKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalDoubleQuoteKey { _unLexicalDoubleQuoteKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalHashKey’: index (for 92) 46.
newtype LexicalHashKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalHashKey { _unLexicalHashKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalDollarKey’: index (for 91) 47.
newtype LexicalDollarKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalDollarKey { _unLexicalDollarKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalPercentKey’: index (for 90) 48.
newtype LexicalPercentKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalPercentKey { _unLexicalPercentKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalAmpersandKey’: index (for 89) 49.
newtype LexicalAmpersandKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalAmpersandKey { _unLexicalAmpersandKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalSingleQuoteKey’: index (for 88) 50.
newtype LexicalSingleQuoteKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalSingleQuoteKey { _unLexicalSingleQuoteKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalLeftParenthesisKey’: index (for 87) 51.
newtype LexicalLeftParenthesisKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalLeftParenthesisKey { _unLexicalLeftParenthesisKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalRightParenthesisKey’: index (for 86) 52.
newtype LexicalRightParenthesisKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalRightParenthesisKey { _unLexicalRightParenthesisKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalAsteriskKey’: index (for 85) 53.
newtype LexicalAsteriskKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalAsteriskKey { _unLexicalAsteriskKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalPlusKey’: index (for 84) 54.
newtype LexicalPlusKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalPlusKey { _unLexicalPlusKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalCommaKey’: index (for 83) 55.
newtype LexicalCommaKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalCommaKey { _unLexicalCommaKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalHyphenKey’: index (for 82) 56.
newtype LexicalHyphenKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalHyphenKey { _unLexicalHyphenKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalDotKey’: index (for 81) 57.
newtype LexicalDotKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalDotKey { _unLexicalDotKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalSlashKey’: index (for 80) 58.
newtype LexicalSlashKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalSlashKey { _unLexicalSlashKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }

-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexical0Key’: index (for 79) 59.
newtype Lexical0KeyBase lexicalTypeIndexerKeyCons z s = MkLexical0Key { _unLexical0Key :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexical1Key’: index (for 78) 60.
newtype Lexical1KeyBase lexicalTypeIndexerKeyCons z s = MkLexical1Key { _unLexical1Key :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexical2Key’: index (for 77) 61.
newtype Lexical2KeyBase lexicalTypeIndexerKeyCons z s = MkLexical2Key { _unLexical2Key :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexical3Key’: index (for 76) 62.
newtype Lexical3KeyBase lexicalTypeIndexerKeyCons z s = MkLexical3Key { _unLexical3Key :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexical4Key’: index (for 75) 63.
newtype Lexical4KeyBase lexicalTypeIndexerKeyCons z s = MkLexical4Key { _unLexical4Key :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexical5Key’: index (for 74) 64.
newtype Lexical5KeyBase lexicalTypeIndexerKeyCons z s = MkLexical5Key { _unLexical5Key :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexical6Key’: index (for 73) 65.
newtype Lexical6KeyBase lexicalTypeIndexerKeyCons z s = MkLexical6Key { _unLexical6Key :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexical7Key’: index (for 72) 66.
newtype Lexical7KeyBase lexicalTypeIndexerKeyCons z s = MkLexical7Key { _unLexical7Key :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexical8Key’: index (for 71) 67.
newtype Lexical8KeyBase lexicalTypeIndexerKeyCons z s = MkLexical8Key { _unLexical8Key :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexical9Key’: index (for 70) 68.
newtype Lexical9KeyBase lexicalTypeIndexerKeyCons z s = MkLexical9Key { _unLexical9Key :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }

-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalColonKey’: index (for 69) 69.
newtype LexicalColonKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalColonKey { _unLexicalColonKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalSemicolonKey’: index (for 68) 70.
newtype LexicalSemicolonKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalSemicolonKey { _unLexicalSemicolonKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalLeftAngleBracketKey’: index (for 67) 71.
newtype LexicalLeftAngleBracketKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalLeftAngleBracketKey { _unLexicalLeftAngleBracketKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalEqualsKey’: index (for 66) 72.
newtype LexicalEqualsKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalEqualsKey { _unLexicalEqualsKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalRightAngleBracketKey’: index (for 65) 73.
newtype LexicalRightAngleBracketKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalRightAngleBracketKey { _unLexicalRightAngleBracketKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalQuestionMarkKey’: index (for 64) 74.
newtype LexicalQuestionMarkKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalQuestionMarkKey { _unLexicalQuestionMarkKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalAtKey’: index (for 63) 75.
newtype LexicalAtKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalAtKey { _unLexicalAtKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }

-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalAKey’: index (for 62) 76.
newtype LexicalAKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalAKey { _unLexicalAKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalBKey’: index (for 61) 77.
newtype LexicalBKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalBKey { _unLexicalBKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalCKey’: index (for 60) 78.
newtype LexicalCKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalCKey { _unLexicalCKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalDKey’: index (for 59) 79.
newtype LexicalDKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalDKey { _unLexicalDKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalEKey’: index (for 58) 80.
newtype LexicalEKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalEKey { _unLexicalEKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalFKey’: index (for 57) 81.
newtype LexicalFKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalFKey { _unLexicalFKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalGKey’: index (for 56) 82.
newtype LexicalGKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalGKey { _unLexicalGKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalHKey’: index (for 55) 83.
newtype LexicalHKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalHKey { _unLexicalHKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalIKey’: index (for 54) 84.
newtype LexicalIKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalIKey { _unLexicalIKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalJKey’: index (for 53) 85.
newtype LexicalJKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalJKey { _unLexicalJKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalKKey’: index (for 52) 86.
newtype LexicalKKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalKKey { _unLexicalKKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalLKey’: index (for 51) 87.
newtype LexicalLKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalLKey { _unLexicalLKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalMKey’: index (for 50) 88.
newtype LexicalMKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalMKey { _unLexicalMKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalNKey’: index (for 49) 89.
newtype LexicalNKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalNKey { _unLexicalNKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalOKey’: index (for 48) 90.
newtype LexicalOKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalOKey { _unLexicalOKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalPKey’: index (for 47) 91.
newtype LexicalPKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalPKey { _unLexicalPKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalQKey’: index (for 46) 92.
newtype LexicalQKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalQKey { _unLexicalQKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalRKey’: index (for 45) 93.
newtype LexicalRKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalRKey { _unLexicalRKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalSKey’: index (for 44) 94.
newtype LexicalSKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalSKey { _unLexicalSKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalTKey’: index (for 43) 95.
newtype LexicalTKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalTKey { _unLexicalTKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalUKey’: index (for 42) 96.
newtype LexicalUKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalUKey { _unLexicalUKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalVKey’: index (for 41) 97.
newtype LexicalVKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalVKey { _unLexicalVKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalWKey’: index (for 40) 98.
newtype LexicalWKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalWKey { _unLexicalWKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalXKey’: index (for 39) 99.
newtype LexicalXKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalXKey { _unLexicalXKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalYKey’: index (for 38) 100.
newtype LexicalYKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalYKey { _unLexicalYKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalZKey’: index (for 37) 101.
newtype LexicalZKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalZKey { _unLexicalZKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }

-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalLeftBracketKey’: index (for 36) 102.
newtype LexicalLeftBracketKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalLeftBracketKey { _unLexicalLeftBracketKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalBackslashKey’: index (for 35) 103.
newtype LexicalBackslashKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalBackslashKey { _unLexicalBackslashKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalRightBracketKey’: index (for 34) 104.
newtype LexicalRightBracketKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalRightBracketKey { _unLexicalRightBracketKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalCaretKey’: index (for 33) 105.
newtype LexicalCaretKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalCaretKey { _unLexicalCaretKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalUnderscoreKey’: index (for 32) 106.
newtype LexicalUnderscoreKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalUnderscoreKey { _unLexicalUnderscoreKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalBacktickKey’: index (for 31) 107.
newtype LexicalBacktickKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalBacktickKey { _unLexicalBacktickKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }

-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalALowerKey’: index (for 30) 108.
newtype LexicalALowerKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalALowerKey { _unLexicalALowerKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalBLowerKey’: index (for 29) 109.
newtype LexicalBLowerKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalBLowerKey { _unLexicalBLowerKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalCLowerKey’: index (for 28) 110.
newtype LexicalCLowerKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalCLowerKey { _unLexicalCLowerKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalDLowerKey’: index (for 27) 111.
newtype LexicalDLowerKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalDLowerKey { _unLexicalDLowerKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalELowerKey’: index (for 26) 112.
newtype LexicalELowerKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalELowerKey { _unLexicalELowerKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalFLowerKey’: index (for 25) 113.
newtype LexicalFLowerKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalFLowerKey { _unLexicalFLowerKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalGLowerKey’: index (for 24) 114.
newtype LexicalGLowerKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalGLowerKey { _unLexicalGLowerKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalHLowerKey’: index (for 23) 115.
newtype LexicalHLowerKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalHLowerKey { _unLexicalHLowerKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalILowerKey’: index (for 22) 116.
newtype LexicalILowerKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalILowerKey { _unLexicalILowerKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalJLowerKey’: index (for 21) 117.
newtype LexicalJLowerKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalJLowerKey { _unLexicalJLowerKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalKLowerKey’: index (for 20) 118.
newtype LexicalKLowerKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalKLowerKey { _unLexicalKLowerKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalLLowerKey’: index (for 19) 119.
newtype LexicalLLowerKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalLLowerKey { _unLexicalLLowerKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalMLowerKey’: index (for 18) 120.
newtype LexicalMLowerKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalMLowerKey { _unLexicalMLowerKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalNLowerKey’: index (for 17) 121.
newtype LexicalNLowerKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalNLowerKey { _unLexicalNLowerKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalOLowerKey’: index (for 16) 122.
newtype LexicalOLowerKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalOLowerKey { _unLexicalOLowerKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalPLowerKey’: index (for 15) 123.
newtype LexicalPLowerKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalPLowerKey { _unLexicalPLowerKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalQLowerKey’: index (for 14) 124.
newtype LexicalQLowerKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalQLowerKey { _unLexicalQLowerKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalRLowerKey’: index (for 13) 125.
newtype LexicalRLowerKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalRLowerKey { _unLexicalRLowerKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalSLowerKey’: index (for 12) 126.
newtype LexicalSLowerKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalSLowerKey { _unLexicalSLowerKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalTLowerKey’: index (for 11) 127.
newtype LexicalTLowerKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalTLowerKey { _unLexicalTLowerKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s (s z))))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalULowerKey’: index (for 10) 128.
newtype LexicalULowerKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalULowerKey { _unLexicalULowerKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s (s z)))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalVLowerKey’: index (for 9) 129.
newtype LexicalVLowerKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalVLowerKey { _unLexicalVLowerKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s (s z))))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalWLowerKey’: index (for 8) 130.
newtype LexicalWLowerKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalWLowerKey { _unLexicalWLowerKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s (s z)))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalXLowerKey’: index (for 7) 131.
newtype LexicalXLowerKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalXLowerKey { _unLexicalXLowerKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s (s z))))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalYLowerKey’: index (for 6) 132.
newtype LexicalYLowerKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalYLowerKey { _unLexicalYLowerKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s (s z)))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalZLowerKey’: index (for 5) 133.
newtype LexicalZLowerKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalZLowerKey { _unLexicalZLowerKey :: (lexicalTypeIndexerKeyCons (s (s (s (s (s z))))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }

-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalLeftBraceKey’: index (for 4) 134.
newtype LexicalLeftBraceKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalLeftBraceKey { _unLexicalLeftBraceKey :: (lexicalTypeIndexerKeyCons (s (s (s (s z)))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalPipeKey’: index (for 3) 135.
newtype LexicalPipeKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalPipeKey { _unLexicalPipeKey :: (lexicalTypeIndexerKeyCons (s (s (s z))) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalRightBraceKey’: index (for 2) 136.
newtype LexicalRightBraceKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalRightBraceKey { _unLexicalRightBraceKey :: (lexicalTypeIndexerKeyCons (s (s z)) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }
-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalTildeKey’: index (for 1) 137.
newtype LexicalTildeKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalTildeKey { _unLexicalTildeKey :: (lexicalTypeIndexerKeyCons (s z) (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }

-- | A key to a lexical foundation type indexer to get the type for
-- ‘lexicalDELKey’: index (for 0) 138.
newtype LexicalDELKeyBase lexicalTypeIndexerKeyCons z s = MkLexicalDELKey { _unLexicalDELKey :: (lexicalTypeIndexerKeyCons z (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s z))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) }

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
data LexicalFoundationBase proxy lexicalEndOfParseKey lexicalUnicodeDigitKey lexicalUnicodeLargeKey lexicalUnicodeSmallKey lexicalUnicodeSymbolKey lexicalUnicodeWhitespaceCharKey lexicalUnicodeSmallSansAscUnderscoreKey lexicalUnicodeSymbolSansSpecialUnderscoreDoubleQuoteSingleQuoteKey lexicalUnicodeSymbolSansSpecialUnderscoreDoubleQuoteSingleQuoteAsciiKey lexicalUnicodeSymbolSansSpecialUnderscoreDoubleQuoteSingleQuoteColonKey lexicalUnicodeSymbolSansSpecialUnderscoreDoubleQuoteSingleQuoteHyphenKey lexicalNULKey lexicalSOHKey lexicalSTXKey lexicalETXKey lexicalEOTKey lexicalENQKey lexicalACKKey lexicalBELKey lexicalBSKey lexicalHTKey lexicalLFKey lexicalVTKey lexicalFFKey lexicalCRKey lexicalSOKey lexicalSIKey lexicalDLEKey lexicalDC1Key lexicalDC2Key lexicalDC3Key lexicalDC4Key lexicalNAKKey lexicalSYNKey lexicalETBKey lexicalCANKey lexicalEMKey lexicalSUBKey lexicalESCKey lexicalFSKey lexicalGSKey lexicalRSKey lexicalUSKey lexicalSPKey lexicalExclamationKey lexicalDoubleQuoteKey lexicalHashKey lexicalDollarKey lexicalPercentKey lexicalAmpersandKey lexicalSingleQuoteKey lexicalLeftParenthesisKey lexicalRightParenthesisKey lexicalAsteriskKey lexicalPlusKey lexicalCommaKey lexicalHyphenKey lexicalDotKey lexicalSlashKey lexical0Key lexical1Key lexical2Key lexical3Key lexical4Key lexical5Key lexical6Key lexical7Key lexical8Key lexical9Key lexicalColonKey lexicalSemicolonKey lexicalLeftAngleBracketKey lexicalEqualsKey lexicalRightAngleBracketKey lexicalQuestionMarkKey lexicalAtKey lexicalAKey lexicalBKey lexicalCKey lexicalDKey lexicalEKey lexicalFKey lexicalGKey lexicalHKey lexicalIKey lexicalJKey lexicalKKey lexicalLKey lexicalMKey lexicalNKey lexicalOKey lexicalPKey lexicalQKey lexicalRKey lexicalSKey lexicalTKey lexicalUKey lexicalVKey lexicalWKey lexicalXKey lexicalYKey lexicalZKey lexicalLeftBracketKey lexicalBackslashKey lexicalRightBracketKey lexicalCaretKey lexicalUnderscoreKey lexicalBacktickKey lexicalALowerKey lexicalBLowerKey lexicalCLowerKey lexicalDLowerKey lexicalELowerKey lexicalFLowerKey lexicalGLowerKey lexicalHLowerKey lexicalILowerKey lexicalJLowerKey lexicalKLowerKey lexicalLLowerKey lexicalMLowerKey lexicalNLowerKey lexicalOLowerKey lexicalPLowerKey lexicalQLowerKey lexicalRLowerKey lexicalSLowerKey lexicalTLowerKey lexicalULowerKey lexicalVLowerKey lexicalWLowerKey lexicalXLowerKey lexicalYLowerKey lexicalZLowerKey lexicalLeftBraceKey lexicalPipeKey lexicalRightBraceKey lexicalTildeKey lexicalDELKey lexicalTypeIndexerKeyCons zero succ lexicalTypeIndexer typeValue =
	LexicalFoundationPrimitives {
		-- | The only purpose of this field is to let ‘LexicalFoundationBase’
		-- compile with correctly inferred kinds.
		_lexicalFoundationPrimitives_ignoredTypeInferenceFixer                                             :: proxy (lexicalTypeIndexerKeyCons (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ zero)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) zero),

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
