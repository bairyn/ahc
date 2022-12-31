-- vim: set filetype=haskell noet

{-
 - ExclusionStructures.hs
 -
 - Haskell2010 Simple syntax AST: base data structures, organized just for
 - exclusion structures.
 -
 - Instantiates AST structures with intra-module references, providing a
 - default implementation.
 -}

{-# LANGUAGE Haskell2010 #-}

-- | Haskell2010 Simple syntax AST: exclusion data structures.
--
-- The AST structures are organized to split the exclusion structures off.
-- This module contains them.  This enables syntax-level keyword exclusions.
--
-- The default Haskell2010 syntax uses these, although it is possible to build
-- an AST based on the Simple syntax except that uses simpler structures
-- without exclusions, and handles exclusions at a higher level, e.g. as a
-- detail in semantics, not syntax.
module Language.Haskell2010.Ahc.Syntax.Haskell2010.Simple.AST.Base.ExclusionStructures (
	-- Export everything, explicitly.

	-- * Base structures.

	-- ** § 2.4 Identifiers and Operators types.

	-- *** Exclusion structures types.
	-- | In contrast with the rest of this module group, this section much less
	-- dense in that more details and complexity is used to encode the simple
	-- idea of keyword exclusion, which is what this section is all about.  (An
	-- alternative would be to provide syntax data structures with a proof
	-- value that the data structure is correct, and that proporty proof would
	-- include a proof related to keyword inequality.  However, our compromise
	-- with complexity here 1) gets us conformance with the Haskell2010
	-- specification (particularly syntax-level keyword exclusion) and 2) lets
	-- syntax data structures automatically come with the property that they
	-- are already syntactically correct.  And the restricted set size may be
	-- somewhat small enough to deal with as a base size not further
	-- recursively split.)
	VaridInnerSansAscSmallUnderscoreBase(SmallVaridInnerSansAscSmallUnderscore, LargeVaridInnerSansAscSmallUnderscore, DigitVaridInnerSansAscSmallUnderscore, SingleQuoteVaridInnerInnerSansAscSmallUnderscore),
	SmallSansAscSmallUnderscoreBase(UnicodeSmallSansAscSmallUnderscore),
	UniSmallSansAscBase(UnicodeSmallUniSmallSansAsc),
	VaridBase(SmallSansAscVarid, ALowerVarid, BLowerVarid, CLowerVarid, DLowerVarid, ELowerVarid, FLowerVarid, GLowerVarid, HLowerVarid, ILowerVarid, JLowerVarid, KLowerVarid, LLowerVarid, MLowerVarid, NLowerVarid, OLowerVarid, PLowerVarid, QLowerVarid, RLowerVarid, SLowerVarid, TLowerVarid, ULowerVarid, VLowerVarid, WLowerVarid, XLowerVarid, YLowerVarid, ZLowerVarid),
	VaridCBase(EOPVaridC, InnerSansAscVaridC, ALowerVaridC, BLowerVaridC, CLowerVaridC, DLowerVaridC, ELowerVaridC, FLowerVaridC, GLowerVaridC, HLowerVaridC, ILowerVaridC, JLowerVaridC, KLowerVaridC, LLowerVaridC, MLowerVaridC, NLowerVaridC, OLowerVaridC, PLowerVaridC, QLowerVaridC, RLowerVaridC, SLowerVaridC, TLowerVaridC, ULowerVaridC, VLowerVaridC, WLowerVaridC, XLowerVaridC, YLowerVaridC, ZLowerVaridC),
	VaridDBase(EOPVaridD, InnerSansAscVaridD, ALowerVaridD, BLowerVaridD, CLowerVaridD, DLowerVaridD, ELowerVaridD, FLowerVaridD, GLowerVaridD, HLowerVaridD, ILowerVaridD, JLowerVaridD, KLowerVaridD, LLowerVaridD, MLowerVaridD, NLowerVaridD, OLowerVaridD, PLowerVaridD, QLowerVaridD, RLowerVaridD, SLowerVaridD, TLowerVaridD, ULowerVaridD, VLowerVaridD, WLowerVaridD, XLowerVaridD, YLowerVaridD, ZLowerVaridD),
	VaridEBase(EOPVaridE, InnerSansAscVaridE, ALowerVaridE, BLowerVaridE, CLowerVaridE, DLowerVaridE, ELowerVaridE, FLowerVaridE, GLowerVaridE, HLowerVaridE, ILowerVaridE, JLowerVaridE, KLowerVaridE, LLowerVaridE, MLowerVaridE, NLowerVaridE, OLowerVaridE, PLowerVaridE, QLowerVaridE, RLowerVaridE, SLowerVaridE, TLowerVaridE, ULowerVaridE, VLowerVaridE, WLowerVaridE, XLowerVaridE, YLowerVaridE, ZLowerVaridE),
	VaridFBase(EOPVaridF, InnerSansAscVaridF, ALowerVaridF, BLowerVaridF, CLowerVaridF, DLowerVaridF, ELowerVaridF, FLowerVaridF, GLowerVaridF, HLowerVaridF, ILowerVaridF, JLowerVaridF, KLowerVaridF, LLowerVaridF, MLowerVaridF, NLowerVaridF, OLowerVaridF, PLowerVaridF, QLowerVaridF, RLowerVaridF, SLowerVaridF, TLowerVaridF, ULowerVaridF, VLowerVaridF, WLowerVaridF, XLowerVaridF, YLowerVaridF, ZLowerVaridF),
	VaridIBase(EOPVaridI, InnerSansAscVaridI, ALowerVaridI, BLowerVaridI, CLowerVaridI, DLowerVaridI, ELowerVaridI, FLowerVaridI, GLowerVaridI, HLowerVaridI, ILowerVaridI, JLowerVaridI, KLowerVaridI, LLowerVaridI, MLowerVaridI, NLowerVaridI, OLowerVaridI, PLowerVaridI, QLowerVaridI, RLowerVaridI, SLowerVaridI, TLowerVaridI, ULowerVaridI, VLowerVaridI, WLowerVaridI, XLowerVaridI, YLowerVaridI, ZLowerVaridI),
	VaridLBase(EOPVaridL, InnerSansAscVaridL, ALowerVaridL, BLowerVaridL, CLowerVaridL, DLowerVaridL, ELowerVaridL, FLowerVaridL, GLowerVaridL, HLowerVaridL, ILowerVaridL, JLowerVaridL, KLowerVaridL, LLowerVaridL, MLowerVaridL, NLowerVaridL, OLowerVaridL, PLowerVaridL, QLowerVaridL, RLowerVaridL, SLowerVaridL, TLowerVaridL, ULowerVaridL, VLowerVaridL, WLowerVaridL, XLowerVaridL, YLowerVaridL, ZLowerVaridL),
	VaridMBase(EOPVaridM, InnerSansAscVaridM, ALowerVaridM, BLowerVaridM, CLowerVaridM, DLowerVaridM, ELowerVaridM, FLowerVaridM, GLowerVaridM, HLowerVaridM, ILowerVaridM, JLowerVaridM, KLowerVaridM, LLowerVaridM, MLowerVaridM, NLowerVaridM, OLowerVaridM, PLowerVaridM, QLowerVaridM, RLowerVaridM, SLowerVaridM, TLowerVaridM, ULowerVaridM, VLowerVaridM, WLowerVaridM, XLowerVaridM, YLowerVaridM, ZLowerVaridM),
	VaridNBase(EOPVaridN, InnerSansAscVaridN, ALowerVaridN, BLowerVaridN, CLowerVaridN, DLowerVaridN, ELowerVaridN, FLowerVaridN, GLowerVaridN, HLowerVaridN, ILowerVaridN, JLowerVaridN, KLowerVaridN, LLowerVaridN, MLowerVaridN, NLowerVaridN, OLowerVaridN, PLowerVaridN, QLowerVaridN, RLowerVaridN, SLowerVaridN, TLowerVaridN, ULowerVaridN, VLowerVaridN, WLowerVaridN, XLowerVaridN, YLowerVaridN, ZLowerVaridN),
	VaridOBase(EOPVaridO, InnerSansAscVaridO, ALowerVaridO, BLowerVaridO, CLowerVaridO, DLowerVaridO, ELowerVaridO, FLowerVaridO, GLowerVaridO, HLowerVaridO, ILowerVaridO, JLowerVaridO, KLowerVaridO, LLowerVaridO, MLowerVaridO, NLowerVaridO, OLowerVaridO, PLowerVaridO, QLowerVaridO, RLowerVaridO, SLowerVaridO, TLowerVaridO, ULowerVaridO, VLowerVaridO, WLowerVaridO, XLowerVaridO, YLowerVaridO, ZLowerVaridO),
	VaridTBase(EOPVaridT, InnerSansAscVaridT, ALowerVaridT, BLowerVaridT, CLowerVaridT, DLowerVaridT, ELowerVaridT, FLowerVaridT, GLowerVaridT, HLowerVaridT, ILowerVaridT, JLowerVaridT, KLowerVaridT, LLowerVaridT, MLowerVaridT, NLowerVaridT, OLowerVaridT, PLowerVaridT, QLowerVaridT, RLowerVaridT, SLowerVaridT, TLowerVaridT, ULowerVaridT, VLowerVaridT, WLowerVaridT, XLowerVaridT, YLowerVaridT, ZLowerVaridT),
	VaridWBase(EOPVaridW, InnerSansAscVaridW, ALowerVaridW, BLowerVaridW, CLowerVaridW, DLowerVaridW, ELowerVaridW, FLowerVaridW, GLowerVaridW, HLowerVaridW, ILowerVaridW, JLowerVaridW, KLowerVaridW, LLowerVaridW, MLowerVaridW, NLowerVaridW, OLowerVaridW, PLowerVaridW, QLowerVaridW, RLowerVaridW, SLowerVaridW, TLowerVaridW, ULowerVaridW, VLowerVaridW, WLowerVaridW, XLowerVaridW, YLowerVaridW, ZLowerVaridW),
	VaridCaBase(EOPVaridCa, InnerSansAscVaridCa, ALowerVaridCa, BLowerVaridCa, CLowerVaridCa, DLowerVaridCa, ELowerVaridCa, FLowerVaridCa, GLowerVaridCa, HLowerVaridCa, ILowerVaridCa, JLowerVaridCa, KLowerVaridCa, LLowerVaridCa, MLowerVaridCa, NLowerVaridCa, OLowerVaridCa, PLowerVaridCa, QLowerVaridCa, RLowerVaridCa, SLowerVaridCa, TLowerVaridCa, ULowerVaridCa, VLowerVaridCa, WLowerVaridCa, XLowerVaridCa, YLowerVaridCa, ZLowerVaridCa),
	VaridClBase(EOPVaridCl, InnerSansAscVaridCl, ALowerVaridCl, BLowerVaridCl, CLowerVaridCl, DLowerVaridCl, ELowerVaridCl, FLowerVaridCl, GLowerVaridCl, HLowerVaridCl, ILowerVaridCl, JLowerVaridCl, KLowerVaridCl, LLowerVaridCl, MLowerVaridCl, NLowerVaridCl, OLowerVaridCl, PLowerVaridCl, QLowerVaridCl, RLowerVaridCl, SLowerVaridCl, TLowerVaridCl, ULowerVaridCl, VLowerVaridCl, WLowerVaridCl, XLowerVaridCl, YLowerVaridCl, ZLowerVaridCl),
	VaridDaBase(EOPVaridDa, InnerSansAscVaridDa, ALowerVaridDa, BLowerVaridDa, CLowerVaridDa, DLowerVaridDa, ELowerVaridDa, FLowerVaridDa, GLowerVaridDa, HLowerVaridDa, ILowerVaridDa, JLowerVaridDa, KLowerVaridDa, LLowerVaridDa, MLowerVaridDa, NLowerVaridDa, OLowerVaridDa, PLowerVaridDa, QLowerVaridDa, RLowerVaridDa, SLowerVaridDa, TLowerVaridDa, ULowerVaridDa, VLowerVaridDa, WLowerVaridDa, XLowerVaridDa, YLowerVaridDa, ZLowerVaridDa),
	VaridDeBase(EOPVaridDe, InnerSansAscVaridDe, ALowerVaridDe, BLowerVaridDe, CLowerVaridDe, DLowerVaridDe, ELowerVaridDe, FLowerVaridDe, GLowerVaridDe, HLowerVaridDe, ILowerVaridDe, JLowerVaridDe, KLowerVaridDe, LLowerVaridDe, MLowerVaridDe, NLowerVaridDe, OLowerVaridDe, PLowerVaridDe, QLowerVaridDe, RLowerVaridDe, SLowerVaridDe, TLowerVaridDe, ULowerVaridDe, VLowerVaridDe, WLowerVaridDe, XLowerVaridDe, YLowerVaridDe, ZLowerVaridDe),
	VaridElBase(EOPVaridEl, InnerSansAscVaridEl, ALowerVaridEl, BLowerVaridEl, CLowerVaridEl, DLowerVaridEl, ELowerVaridEl, FLowerVaridEl, GLowerVaridEl, HLowerVaridEl, ILowerVaridEl, JLowerVaridEl, KLowerVaridEl, LLowerVaridEl, MLowerVaridEl, NLowerVaridEl, OLowerVaridEl, PLowerVaridEl, QLowerVaridEl, RLowerVaridEl, SLowerVaridEl, TLowerVaridEl, ULowerVaridEl, VLowerVaridEl, WLowerVaridEl, XLowerVaridEl, YLowerVaridEl, ZLowerVaridEl),
	VaridFoBase(EOPVaridFo, InnerSansAscVaridFo, ALowerVaridFo, BLowerVaridFo, CLowerVaridFo, DLowerVaridFo, ELowerVaridFo, FLowerVaridFo, GLowerVaridFo, HLowerVaridFo, ILowerVaridFo, JLowerVaridFo, KLowerVaridFo, LLowerVaridFo, MLowerVaridFo, NLowerVaridFo, OLowerVaridFo, PLowerVaridFo, QLowerVaridFo, RLowerVaridFo, SLowerVaridFo, TLowerVaridFo, ULowerVaridFo, VLowerVaridFo, WLowerVaridFo, XLowerVaridFo, YLowerVaridFo, ZLowerVaridFo),
	VaridImBase(EOPVaridIm, InnerSansAscVaridIm, ALowerVaridIm, BLowerVaridIm, CLowerVaridIm, DLowerVaridIm, ELowerVaridIm, FLowerVaridIm, GLowerVaridIm, HLowerVaridIm, ILowerVaridIm, JLowerVaridIm, KLowerVaridIm, LLowerVaridIm, MLowerVaridIm, NLowerVaridIm, OLowerVaridIm, PLowerVaridIm, QLowerVaridIm, RLowerVaridIm, SLowerVaridIm, TLowerVaridIm, ULowerVaridIm, VLowerVaridIm, WLowerVaridIm, XLowerVaridIm, YLowerVaridIm, ZLowerVaridIm),
	VaridInBase(InnerSansAscVaridIn, ALowerVaridIn, BLowerVaridIn, CLowerVaridIn, DLowerVaridIn, ELowerVaridIn, FLowerVaridIn, GLowerVaridIn, HLowerVaridIn, ILowerVaridIn, JLowerVaridIn, KLowerVaridIn, LLowerVaridIn, MLowerVaridIn, NLowerVaridIn, OLowerVaridIn, PLowerVaridIn, QLowerVaridIn, RLowerVaridIn, SLowerVaridIn, TLowerVaridIn, ULowerVaridIn, VLowerVaridIn, WLowerVaridIn, XLowerVaridIn, YLowerVaridIn, ZLowerVaridIn),
	VaridLeBase(EOPVaridLe, InnerSansAscVaridLe, ALowerVaridLe, BLowerVaridLe, CLowerVaridLe, DLowerVaridLe, ELowerVaridLe, FLowerVaridLe, GLowerVaridLe, HLowerVaridLe, ILowerVaridLe, JLowerVaridLe, KLowerVaridLe, LLowerVaridLe, MLowerVaridLe, NLowerVaridLe, OLowerVaridLe, PLowerVaridLe, QLowerVaridLe, RLowerVaridLe, SLowerVaridLe, TLowerVaridLe, ULowerVaridLe, VLowerVaridLe, WLowerVaridLe, XLowerVaridLe, YLowerVaridLe, ZLowerVaridLe),
	VaridMoBase(EOPVaridMo, InnerSansAscVaridMo, ALowerVaridMo, BLowerVaridMo, CLowerVaridMo, DLowerVaridMo, ELowerVaridMo, FLowerVaridMo, GLowerVaridMo, HLowerVaridMo, ILowerVaridMo, JLowerVaridMo, KLowerVaridMo, LLowerVaridMo, MLowerVaridMo, NLowerVaridMo, OLowerVaridMo, PLowerVaridMo, QLowerVaridMo, RLowerVaridMo, SLowerVaridMo, TLowerVaridMo, ULowerVaridMo, VLowerVaridMo, WLowerVaridMo, XLowerVaridMo, YLowerVaridMo, ZLowerVaridMo),
	VaridNeBase(EOPVaridNe, InnerSansAscVaridNe, ALowerVaridNe, BLowerVaridNe, CLowerVaridNe, DLowerVaridNe, ELowerVaridNe, FLowerVaridNe, GLowerVaridNe, HLowerVaridNe, ILowerVaridNe, JLowerVaridNe, KLowerVaridNe, LLowerVaridNe, MLowerVaridNe, NLowerVaridNe, OLowerVaridNe, PLowerVaridNe, QLowerVaridNe, RLowerVaridNe, SLowerVaridNe, TLowerVaridNe, ULowerVaridNe, VLowerVaridNe, WLowerVaridNe, XLowerVaridNe, YLowerVaridNe, ZLowerVaridNe),
	VaridThBase(EOPVaridTh, InnerSansAscVaridTh, ALowerVaridTh, BLowerVaridTh, CLowerVaridTh, DLowerVaridTh, ELowerVaridTh, FLowerVaridTh, GLowerVaridTh, HLowerVaridTh, ILowerVaridTh, JLowerVaridTh, KLowerVaridTh, LLowerVaridTh, MLowerVaridTh, NLowerVaridTh, OLowerVaridTh, PLowerVaridTh, QLowerVaridTh, RLowerVaridTh, SLowerVaridTh, TLowerVaridTh, ULowerVaridTh, VLowerVaridTh, WLowerVaridTh, XLowerVaridTh, YLowerVaridTh, ZLowerVaridTh),
	VaridTyBase(EOPVaridTy, InnerSansAscVaridTy, ALowerVaridTy, BLowerVaridTy, CLowerVaridTy, DLowerVaridTy, ELowerVaridTy, FLowerVaridTy, GLowerVaridTy, HLowerVaridTy, ILowerVaridTy, JLowerVaridTy, KLowerVaridTy, LLowerVaridTy, MLowerVaridTy, NLowerVaridTy, OLowerVaridTy, PLowerVaridTy, QLowerVaridTy, RLowerVaridTy, SLowerVaridTy, TLowerVaridTy, ULowerVaridTy, VLowerVaridTy, WLowerVaridTy, XLowerVaridTy, YLowerVaridTy, ZLowerVaridTy),
	VaridCasBase(EOPVaridCas, InnerSansAscVaridCas, ALowerVaridCas, BLowerVaridCas, CLowerVaridCas, DLowerVaridCas, ELowerVaridCas, FLowerVaridCas, GLowerVaridCas, HLowerVaridCas, ILowerVaridCas, JLowerVaridCas, KLowerVaridCas, LLowerVaridCas, MLowerVaridCas, NLowerVaridCas, OLowerVaridCas, PLowerVaridCas, QLowerVaridCas, RLowerVaridCas, SLowerVaridCas, TLowerVaridCas, ULowerVaridCas, VLowerVaridCas, WLowerVaridCas, XLowerVaridCas, YLowerVaridCas, ZLowerVaridCas),
	VaridClaBase(EOPVaridCla, InnerSansAscVaridCla, ALowerVaridCla, BLowerVaridCla, CLowerVaridCla, DLowerVaridCla, ELowerVaridCla, FLowerVaridCla, GLowerVaridCla, HLowerVaridCla, ILowerVaridCla, JLowerVaridCla, KLowerVaridCla, LLowerVaridCla, MLowerVaridCla, NLowerVaridCla, OLowerVaridCla, PLowerVaridCla, QLowerVaridCla, RLowerVaridCla, SLowerVaridCla, TLowerVaridCla, ULowerVaridCla, VLowerVaridCla, WLowerVaridCla, XLowerVaridCla, YLowerVaridCla, ZLowerVaridCla),
	VaridDatBase(EOPVaridDat, InnerSansAscVaridDat, ALowerVaridDat, BLowerVaridDat, CLowerVaridDat, DLowerVaridDat, ELowerVaridDat, FLowerVaridDat, GLowerVaridDat, HLowerVaridDat, ILowerVaridDat, JLowerVaridDat, KLowerVaridDat, LLowerVaridDat, MLowerVaridDat, NLowerVaridDat, OLowerVaridDat, PLowerVaridDat, QLowerVaridDat, RLowerVaridDat, SLowerVaridDat, TLowerVaridDat, ULowerVaridDat, VLowerVaridDat, WLowerVaridDat, XLowerVaridDat, YLowerVaridDat, ZLowerVaridDat),
	VaridDefBase(EOPVaridDef, InnerSansAscVaridDef, ALowerVaridDef, BLowerVaridDef, CLowerVaridDef, DLowerVaridDef, ELowerVaridDef, FLowerVaridDef, GLowerVaridDef, HLowerVaridDef, ILowerVaridDef, JLowerVaridDef, KLowerVaridDef, LLowerVaridDef, MLowerVaridDef, NLowerVaridDef, OLowerVaridDef, PLowerVaridDef, QLowerVaridDef, RLowerVaridDef, SLowerVaridDef, TLowerVaridDef, ULowerVaridDef, VLowerVaridDef, WLowerVaridDef, XLowerVaridDef, YLowerVaridDef, ZLowerVaridDef),
	VaridDerBase(EOPVaridDer, InnerSansAscVaridDer, ALowerVaridDer, BLowerVaridDer, CLowerVaridDer, DLowerVaridDer, ELowerVaridDer, FLowerVaridDer, GLowerVaridDer, HLowerVaridDer, ILowerVaridDer, JLowerVaridDer, KLowerVaridDer, LLowerVaridDer, MLowerVaridDer, NLowerVaridDer, OLowerVaridDer, PLowerVaridDer, QLowerVaridDer, RLowerVaridDer, SLowerVaridDer, TLowerVaridDer, ULowerVaridDer, VLowerVaridDer, WLowerVaridDer, XLowerVaridDer, YLowerVaridDer, ZLowerVaridDer),
	VaridElsBase(EOPVaridEls, InnerSansAscVaridEls, ALowerVaridEls, BLowerVaridEls, CLowerVaridEls, DLowerVaridEls, ELowerVaridEls, FLowerVaridEls, GLowerVaridEls, HLowerVaridEls, ILowerVaridEls, JLowerVaridEls, KLowerVaridEls, LLowerVaridEls, MLowerVaridEls, NLowerVaridEls, OLowerVaridEls, PLowerVaridEls, QLowerVaridEls, RLowerVaridEls, SLowerVaridEls, TLowerVaridEls, ULowerVaridEls, VLowerVaridEls, WLowerVaridEls, XLowerVaridEls, YLowerVaridEls, ZLowerVaridEls),
	VaridForBase(EOPVaridFor, InnerSansAscVaridFor, ALowerVaridFor, BLowerVaridFor, CLowerVaridFor, DLowerVaridFor, ELowerVaridFor, FLowerVaridFor, GLowerVaridFor, HLowerVaridFor, ILowerVaridFor, JLowerVaridFor, KLowerVaridFor, LLowerVaridFor, MLowerVaridFor, NLowerVaridFor, OLowerVaridFor, PLowerVaridFor, QLowerVaridFor, RLowerVaridFor, SLowerVaridFor, TLowerVaridFor, ULowerVaridFor, VLowerVaridFor, WLowerVaridFor, XLowerVaridFor, YLowerVaridFor, ZLowerVaridFor),
	VaridImpBase(EOPVaridImp, InnerSansAscVaridImp, ALowerVaridImp, BLowerVaridImp, CLowerVaridImp, DLowerVaridImp, ELowerVaridImp, FLowerVaridImp, GLowerVaridImp, HLowerVaridImp, ILowerVaridImp, JLowerVaridImp, KLowerVaridImp, LLowerVaridImp, MLowerVaridImp, NLowerVaridImp, OLowerVaridImp, PLowerVaridImp, QLowerVaridImp, RLowerVaridImp, SLowerVaridImp, TLowerVaridImp, ULowerVaridImp, VLowerVaridImp, WLowerVaridImp, XLowerVaridImp, YLowerVaridImp, ZLowerVaridImp),
	VaridInfBase(EOPVaridInf, InnerSansAscVaridInf, ALowerVaridInf, BLowerVaridInf, CLowerVaridInf, DLowerVaridInf, ELowerVaridInf, FLowerVaridInf, GLowerVaridInf, HLowerVaridInf, ILowerVaridInf, JLowerVaridInf, KLowerVaridInf, LLowerVaridInf, MLowerVaridInf, NLowerVaridInf, OLowerVaridInf, PLowerVaridInf, QLowerVaridInf, RLowerVaridInf, SLowerVaridInf, TLowerVaridInf, ULowerVaridInf, VLowerVaridInf, WLowerVaridInf, XLowerVaridInf, YLowerVaridInf, ZLowerVaridInf),
	VaridInsBase(EOPVaridIns, InnerSansAscVaridIns, ALowerVaridIns, BLowerVaridIns, CLowerVaridIns, DLowerVaridIns, ELowerVaridIns, FLowerVaridIns, GLowerVaridIns, HLowerVaridIns, ILowerVaridIns, JLowerVaridIns, KLowerVaridIns, LLowerVaridIns, MLowerVaridIns, NLowerVaridIns, OLowerVaridIns, PLowerVaridIns, QLowerVaridIns, RLowerVaridIns, SLowerVaridIns, TLowerVaridIns, ULowerVaridIns, VLowerVaridIns, WLowerVaridIns, XLowerVaridIns, YLowerVaridIns, ZLowerVaridIns),
	VaridModBase(EOPVaridMod, InnerSansAscVaridMod, ALowerVaridMod, BLowerVaridMod, CLowerVaridMod, DLowerVaridMod, ELowerVaridMod, FLowerVaridMod, GLowerVaridMod, HLowerVaridMod, ILowerVaridMod, JLowerVaridMod, KLowerVaridMod, LLowerVaridMod, MLowerVaridMod, NLowerVaridMod, OLowerVaridMod, PLowerVaridMod, QLowerVaridMod, RLowerVaridMod, SLowerVaridMod, TLowerVaridMod, ULowerVaridMod, VLowerVaridMod, WLowerVaridMod, XLowerVaridMod, YLowerVaridMod, ZLowerVaridMod),
	VaridNewBase(EOPVaridNew, InnerSansAscVaridNew, ALowerVaridNew, BLowerVaridNew, CLowerVaridNew, DLowerVaridNew, ELowerVaridNew, FLowerVaridNew, GLowerVaridNew, HLowerVaridNew, ILowerVaridNew, JLowerVaridNew, KLowerVaridNew, LLowerVaridNew, MLowerVaridNew, NLowerVaridNew, OLowerVaridNew, PLowerVaridNew, QLowerVaridNew, RLowerVaridNew, SLowerVaridNew, TLowerVaridNew, ULowerVaridNew, VLowerVaridNew, WLowerVaridNew, XLowerVaridNew, YLowerVaridNew, ZLowerVaridNew),
	VaridTheBase(EOPVaridThe, InnerSansAscVaridThe, ALowerVaridThe, BLowerVaridThe, CLowerVaridThe, DLowerVaridThe, ELowerVaridThe, FLowerVaridThe, GLowerVaridThe, HLowerVaridThe, ILowerVaridThe, JLowerVaridThe, KLowerVaridThe, LLowerVaridThe, MLowerVaridThe, NLowerVaridThe, OLowerVaridThe, PLowerVaridThe, QLowerVaridThe, RLowerVaridThe, SLowerVaridThe, TLowerVaridThe, ULowerVaridThe, VLowerVaridThe, WLowerVaridThe, XLowerVaridThe, YLowerVaridThe, ZLowerVaridThe),
	VaridTypBase(EOPVaridTyp, InnerSansAscVaridTyp, ALowerVaridTyp, BLowerVaridTyp, CLowerVaridTyp, DLowerVaridTyp, ELowerVaridTyp, FLowerVaridTyp, GLowerVaridTyp, HLowerVaridTyp, ILowerVaridTyp, JLowerVaridTyp, KLowerVaridTyp, LLowerVaridTyp, MLowerVaridTyp, NLowerVaridTyp, OLowerVaridTyp, PLowerVaridTyp, QLowerVaridTyp, RLowerVaridTyp, SLowerVaridTyp, TLowerVaridTyp, ULowerVaridTyp, VLowerVaridTyp, WLowerVaridTyp, XLowerVaridTyp, YLowerVaridTyp, ZLowerVaridTyp),
	VaridWheBase(EOPVaridWhe, InnerSansAscVaridWhe, ALowerVaridWhe, BLowerVaridWhe, CLowerVaridWhe, DLowerVaridWhe, ELowerVaridWhe, FLowerVaridWhe, GLowerVaridWhe, HLowerVaridWhe, ILowerVaridWhe, JLowerVaridWhe, KLowerVaridWhe, LLowerVaridWhe, MLowerVaridWhe, NLowerVaridWhe, OLowerVaridWhe, PLowerVaridWhe, QLowerVaridWhe, RLowerVaridWhe, SLowerVaridWhe, TLowerVaridWhe, ULowerVaridWhe, VLowerVaridWhe, WLowerVaridWhe, XLowerVaridWhe, YLowerVaridWhe, ZLowerVaridWhe),
	VaridClasBase(EOPVaridClas, InnerSansAscVaridClas, ALowerVaridClas, BLowerVaridClas, CLowerVaridClas, DLowerVaridClas, ELowerVaridClas, FLowerVaridClas, GLowerVaridClas, HLowerVaridClas, ILowerVaridClas, JLowerVaridClas, KLowerVaridClas, LLowerVaridClas, MLowerVaridClas, NLowerVaridClas, OLowerVaridClas, PLowerVaridClas, QLowerVaridClas, RLowerVaridClas, SLowerVaridClas, TLowerVaridClas, ULowerVaridClas, VLowerVaridClas, WLowerVaridClas, XLowerVaridClas, YLowerVaridClas, ZLowerVaridClas),
	VaridDefaBase(EOPVaridDefa, InnerSansAscVaridDefa, ALowerVaridDefa, BLowerVaridDefa, CLowerVaridDefa, DLowerVaridDefa, ELowerVaridDefa, FLowerVaridDefa, GLowerVaridDefa, HLowerVaridDefa, ILowerVaridDefa, JLowerVaridDefa, KLowerVaridDefa, LLowerVaridDefa, MLowerVaridDefa, NLowerVaridDefa, OLowerVaridDefa, PLowerVaridDefa, QLowerVaridDefa, RLowerVaridDefa, SLowerVaridDefa, TLowerVaridDefa, ULowerVaridDefa, VLowerVaridDefa, WLowerVaridDefa, XLowerVaridDefa, YLowerVaridDefa, ZLowerVaridDefa),
	VaridDeriBase(EOPVaridDeri, InnerSansAscVaridDeri, ALowerVaridDeri, BLowerVaridDeri, CLowerVaridDeri, DLowerVaridDeri, ELowerVaridDeri, FLowerVaridDeri, GLowerVaridDeri, HLowerVaridDeri, ILowerVaridDeri, JLowerVaridDeri, KLowerVaridDeri, LLowerVaridDeri, MLowerVaridDeri, NLowerVaridDeri, OLowerVaridDeri, PLowerVaridDeri, QLowerVaridDeri, RLowerVaridDeri, SLowerVaridDeri, TLowerVaridDeri, ULowerVaridDeri, VLowerVaridDeri, WLowerVaridDeri, XLowerVaridDeri, YLowerVaridDeri, ZLowerVaridDeri),
	VaridForeBase(EOPVaridFore, InnerSansAscVaridFore, ALowerVaridFore, BLowerVaridFore, CLowerVaridFore, DLowerVaridFore, ELowerVaridFore, FLowerVaridFore, GLowerVaridFore, HLowerVaridFore, ILowerVaridFore, JLowerVaridFore, KLowerVaridFore, LLowerVaridFore, MLowerVaridFore, NLowerVaridFore, OLowerVaridFore, PLowerVaridFore, QLowerVaridFore, RLowerVaridFore, SLowerVaridFore, TLowerVaridFore, ULowerVaridFore, VLowerVaridFore, WLowerVaridFore, XLowerVaridFore, YLowerVaridFore, ZLowerVaridFore),
	VaridImpoBase(EOPVaridImpo, InnerSansAscVaridImpo, ALowerVaridImpo, BLowerVaridImpo, CLowerVaridImpo, DLowerVaridImpo, ELowerVaridImpo, FLowerVaridImpo, GLowerVaridImpo, HLowerVaridImpo, ILowerVaridImpo, JLowerVaridImpo, KLowerVaridImpo, LLowerVaridImpo, MLowerVaridImpo, NLowerVaridImpo, OLowerVaridImpo, PLowerVaridImpo, QLowerVaridImpo, RLowerVaridImpo, SLowerVaridImpo, TLowerVaridImpo, ULowerVaridImpo, VLowerVaridImpo, WLowerVaridImpo, XLowerVaridImpo, YLowerVaridImpo, ZLowerVaridImpo),
	VaridInfiBase(EOPVaridInfi, InnerSansAscVaridInfi, ALowerVaridInfi, BLowerVaridInfi, CLowerVaridInfi, DLowerVaridInfi, ELowerVaridInfi, FLowerVaridInfi, GLowerVaridInfi, HLowerVaridInfi, ILowerVaridInfi, JLowerVaridInfi, KLowerVaridInfi, LLowerVaridInfi, MLowerVaridInfi, NLowerVaridInfi, OLowerVaridInfi, PLowerVaridInfi, QLowerVaridInfi, RLowerVaridInfi, SLowerVaridInfi, TLowerVaridInfi, ULowerVaridInfi, VLowerVaridInfi, WLowerVaridInfi, XLowerVaridInfi, YLowerVaridInfi, ZLowerVaridInfi),
	VaridInstBase(EOPVaridInst, InnerSansAscVaridInst, ALowerVaridInst, BLowerVaridInst, CLowerVaridInst, DLowerVaridInst, ELowerVaridInst, FLowerVaridInst, GLowerVaridInst, HLowerVaridInst, ILowerVaridInst, JLowerVaridInst, KLowerVaridInst, LLowerVaridInst, MLowerVaridInst, NLowerVaridInst, OLowerVaridInst, PLowerVaridInst, QLowerVaridInst, RLowerVaridInst, SLowerVaridInst, TLowerVaridInst, ULowerVaridInst, VLowerVaridInst, WLowerVaridInst, XLowerVaridInst, YLowerVaridInst, ZLowerVaridInst),
	VaridModuBase(EOPVaridModu, InnerSansAscVaridModu, ALowerVaridModu, BLowerVaridModu, CLowerVaridModu, DLowerVaridModu, ELowerVaridModu, FLowerVaridModu, GLowerVaridModu, HLowerVaridModu, ILowerVaridModu, JLowerVaridModu, KLowerVaridModu, LLowerVaridModu, MLowerVaridModu, NLowerVaridModu, OLowerVaridModu, PLowerVaridModu, QLowerVaridModu, RLowerVaridModu, SLowerVaridModu, TLowerVaridModu, ULowerVaridModu, VLowerVaridModu, WLowerVaridModu, XLowerVaridModu, YLowerVaridModu, ZLowerVaridModu),
	VaridNewtBase(EOPVaridNewt, InnerSansAscVaridNewt, ALowerVaridNewt, BLowerVaridNewt, CLowerVaridNewt, DLowerVaridNewt, ELowerVaridNewt, FLowerVaridNewt, GLowerVaridNewt, HLowerVaridNewt, ILowerVaridNewt, JLowerVaridNewt, KLowerVaridNewt, LLowerVaridNewt, MLowerVaridNewt, NLowerVaridNewt, OLowerVaridNewt, PLowerVaridNewt, QLowerVaridNewt, RLowerVaridNewt, SLowerVaridNewt, TLowerVaridNewt, ULowerVaridNewt, VLowerVaridNewt, WLowerVaridNewt, XLowerVaridNewt, YLowerVaridNewt, ZLowerVaridNewt),
	VaridWherBase(EOPVaridWher, InnerSansAscVaridWher, ALowerVaridWher, BLowerVaridWher, CLowerVaridWher, DLowerVaridWher, ELowerVaridWher, FLowerVaridWher, GLowerVaridWher, HLowerVaridWher, ILowerVaridWher, JLowerVaridWher, KLowerVaridWher, LLowerVaridWher, MLowerVaridWher, NLowerVaridWher, OLowerVaridWher, PLowerVaridWher, QLowerVaridWher, RLowerVaridWher, SLowerVaridWher, TLowerVaridWher, ULowerVaridWher, VLowerVaridWher, WLowerVaridWher, XLowerVaridWher, YLowerVaridWher, ZLowerVaridWher),
	VaridDefauBase(EOPVaridDefau, InnerSansAscVaridDefau, ALowerVaridDefau, BLowerVaridDefau, CLowerVaridDefau, DLowerVaridDefau, ELowerVaridDefau, FLowerVaridDefau, GLowerVaridDefau, HLowerVaridDefau, ILowerVaridDefau, JLowerVaridDefau, KLowerVaridDefau, LLowerVaridDefau, MLowerVaridDefau, NLowerVaridDefau, OLowerVaridDefau, PLowerVaridDefau, QLowerVaridDefau, RLowerVaridDefau, SLowerVaridDefau, TLowerVaridDefau, ULowerVaridDefau, VLowerVaridDefau, WLowerVaridDefau, XLowerVaridDefau, YLowerVaridDefau, ZLowerVaridDefau),
	VaridDerivBase(EOPVaridDeriv, InnerSansAscVaridDeriv, ALowerVaridDeriv, BLowerVaridDeriv, CLowerVaridDeriv, DLowerVaridDeriv, ELowerVaridDeriv, FLowerVaridDeriv, GLowerVaridDeriv, HLowerVaridDeriv, ILowerVaridDeriv, JLowerVaridDeriv, KLowerVaridDeriv, LLowerVaridDeriv, MLowerVaridDeriv, NLowerVaridDeriv, OLowerVaridDeriv, PLowerVaridDeriv, QLowerVaridDeriv, RLowerVaridDeriv, SLowerVaridDeriv, TLowerVaridDeriv, ULowerVaridDeriv, VLowerVaridDeriv, WLowerVaridDeriv, XLowerVaridDeriv, YLowerVaridDeriv, ZLowerVaridDeriv),
	VaridForeiBase(EOPVaridForei, InnerSansAscVaridForei, ALowerVaridForei, BLowerVaridForei, CLowerVaridForei, DLowerVaridForei, ELowerVaridForei, FLowerVaridForei, GLowerVaridForei, HLowerVaridForei, ILowerVaridForei, JLowerVaridForei, KLowerVaridForei, LLowerVaridForei, MLowerVaridForei, NLowerVaridForei, OLowerVaridForei, PLowerVaridForei, QLowerVaridForei, RLowerVaridForei, SLowerVaridForei, TLowerVaridForei, ULowerVaridForei, VLowerVaridForei, WLowerVaridForei, XLowerVaridForei, YLowerVaridForei, ZLowerVaridForei),
	VaridImporBase(EOPVaridImpor, InnerSansAscVaridImpor, ALowerVaridImpor, BLowerVaridImpor, CLowerVaridImpor, DLowerVaridImpor, ELowerVaridImpor, FLowerVaridImpor, GLowerVaridImpor, HLowerVaridImpor, ILowerVaridImpor, JLowerVaridImpor, KLowerVaridImpor, LLowerVaridImpor, MLowerVaridImpor, NLowerVaridImpor, OLowerVaridImpor, PLowerVaridImpor, QLowerVaridImpor, RLowerVaridImpor, SLowerVaridImpor, TLowerVaridImpor, ULowerVaridImpor, VLowerVaridImpor, WLowerVaridImpor, XLowerVaridImpor, YLowerVaridImpor, ZLowerVaridImpor),
	VaridInfixBase(InnerSansAscVaridInfix, ALowerVaridInfix, BLowerVaridInfix, CLowerVaridInfix, DLowerVaridInfix, ELowerVaridInfix, FLowerVaridInfix, GLowerVaridInfix, HLowerVaridInfix, ILowerVaridInfix, JLowerVaridInfix, KLowerVaridInfix, LLowerVaridInfix, MLowerVaridInfix, NLowerVaridInfix, OLowerVaridInfix, PLowerVaridInfix, QLowerVaridInfix, RLowerVaridInfix, SLowerVaridInfix, TLowerVaridInfix, ULowerVaridInfix, VLowerVaridInfix, WLowerVaridInfix, XLowerVaridInfix, YLowerVaridInfix, ZLowerVaridInfix),
	VaridInstaBase(EOPVaridInsta, InnerSansAscVaridInsta, ALowerVaridInsta, BLowerVaridInsta, CLowerVaridInsta, DLowerVaridInsta, ELowerVaridInsta, FLowerVaridInsta, GLowerVaridInsta, HLowerVaridInsta, ILowerVaridInsta, JLowerVaridInsta, KLowerVaridInsta, LLowerVaridInsta, MLowerVaridInsta, NLowerVaridInsta, OLowerVaridInsta, PLowerVaridInsta, QLowerVaridInsta, RLowerVaridInsta, SLowerVaridInsta, TLowerVaridInsta, ULowerVaridInsta, VLowerVaridInsta, WLowerVaridInsta, XLowerVaridInsta, YLowerVaridInsta, ZLowerVaridInsta),
	VaridModulBase(EOPVaridModul, InnerSansAscVaridModul, ALowerVaridModul, BLowerVaridModul, CLowerVaridModul, DLowerVaridModul, ELowerVaridModul, FLowerVaridModul, GLowerVaridModul, HLowerVaridModul, ILowerVaridModul, JLowerVaridModul, KLowerVaridModul, LLowerVaridModul, MLowerVaridModul, NLowerVaridModul, OLowerVaridModul, PLowerVaridModul, QLowerVaridModul, RLowerVaridModul, SLowerVaridModul, TLowerVaridModul, ULowerVaridModul, VLowerVaridModul, WLowerVaridModul, XLowerVaridModul, YLowerVaridModul, ZLowerVaridModul),
	VaridNewtyBase(EOPVaridNewty, InnerSansAscVaridNewty, ALowerVaridNewty, BLowerVaridNewty, CLowerVaridNewty, DLowerVaridNewty, ELowerVaridNewty, FLowerVaridNewty, GLowerVaridNewty, HLowerVaridNewty, ILowerVaridNewty, JLowerVaridNewty, KLowerVaridNewty, LLowerVaridNewty, MLowerVaridNewty, NLowerVaridNewty, OLowerVaridNewty, PLowerVaridNewty, QLowerVaridNewty, RLowerVaridNewty, SLowerVaridNewty, TLowerVaridNewty, ULowerVaridNewty, VLowerVaridNewty, WLowerVaridNewty, XLowerVaridNewty, YLowerVaridNewty, ZLowerVaridNewty),
	VaridDefaulBase(EOPVaridDefaul, InnerSansAscVaridDefaul, ALowerVaridDefaul, BLowerVaridDefaul, CLowerVaridDefaul, DLowerVaridDefaul, ELowerVaridDefaul, FLowerVaridDefaul, GLowerVaridDefaul, HLowerVaridDefaul, ILowerVaridDefaul, JLowerVaridDefaul, KLowerVaridDefaul, LLowerVaridDefaul, MLowerVaridDefaul, NLowerVaridDefaul, OLowerVaridDefaul, PLowerVaridDefaul, QLowerVaridDefaul, RLowerVaridDefaul, SLowerVaridDefaul, TLowerVaridDefaul, ULowerVaridDefaul, VLowerVaridDefaul, WLowerVaridDefaul, XLowerVaridDefaul, YLowerVaridDefaul, ZLowerVaridDefaul),
	VaridDeriviBase(EOPVaridDerivi, InnerSansAscVaridDerivi, ALowerVaridDerivi, BLowerVaridDerivi, CLowerVaridDerivi, DLowerVaridDerivi, ELowerVaridDerivi, FLowerVaridDerivi, GLowerVaridDerivi, HLowerVaridDerivi, ILowerVaridDerivi, JLowerVaridDerivi, KLowerVaridDerivi, LLowerVaridDerivi, MLowerVaridDerivi, NLowerVaridDerivi, OLowerVaridDerivi, PLowerVaridDerivi, QLowerVaridDerivi, RLowerVaridDerivi, SLowerVaridDerivi, TLowerVaridDerivi, ULowerVaridDerivi, VLowerVaridDerivi, WLowerVaridDerivi, XLowerVaridDerivi, YLowerVaridDerivi, ZLowerVaridDerivi),
	VaridForeigBase(EOPVaridForeig, InnerSansAscVaridForeig, ALowerVaridForeig, BLowerVaridForeig, CLowerVaridForeig, DLowerVaridForeig, ELowerVaridForeig, FLowerVaridForeig, GLowerVaridForeig, HLowerVaridForeig, ILowerVaridForeig, JLowerVaridForeig, KLowerVaridForeig, LLowerVaridForeig, MLowerVaridForeig, NLowerVaridForeig, OLowerVaridForeig, PLowerVaridForeig, QLowerVaridForeig, RLowerVaridForeig, SLowerVaridForeig, TLowerVaridForeig, ULowerVaridForeig, VLowerVaridForeig, WLowerVaridForeig, XLowerVaridForeig, YLowerVaridForeig, ZLowerVaridForeig),
	VaridInstanBase(EOPVaridInstan, InnerSansAscVaridInstan, ALowerVaridInstan, BLowerVaridInstan, CLowerVaridInstan, DLowerVaridInstan, ELowerVaridInstan, FLowerVaridInstan, GLowerVaridInstan, HLowerVaridInstan, ILowerVaridInstan, JLowerVaridInstan, KLowerVaridInstan, LLowerVaridInstan, MLowerVaridInstan, NLowerVaridInstan, OLowerVaridInstan, PLowerVaridInstan, QLowerVaridInstan, RLowerVaridInstan, SLowerVaridInstan, TLowerVaridInstan, ULowerVaridInstan, VLowerVaridInstan, WLowerVaridInstan, XLowerVaridInstan, YLowerVaridInstan, ZLowerVaridInstan),
	VaridNewtypBase(EOPVaridNewtyp, InnerSansAscVaridNewtyp, ALowerVaridNewtyp, BLowerVaridNewtyp, CLowerVaridNewtyp, DLowerVaridNewtyp, ELowerVaridNewtyp, FLowerVaridNewtyp, GLowerVaridNewtyp, HLowerVaridNewtyp, ILowerVaridNewtyp, JLowerVaridNewtyp, KLowerVaridNewtyp, LLowerVaridNewtyp, MLowerVaridNewtyp, NLowerVaridNewtyp, OLowerVaridNewtyp, PLowerVaridNewtyp, QLowerVaridNewtyp, RLowerVaridNewtyp, SLowerVaridNewtyp, TLowerVaridNewtyp, ULowerVaridNewtyp, VLowerVaridNewtyp, WLowerVaridNewtyp, XLowerVaridNewtyp, YLowerVaridNewtyp, ZLowerVaridNewtyp),
	VaridDerivinBase(EOPVaridDerivin, InnerSansAscVaridDerivin, ALowerVaridDerivin, BLowerVaridDerivin, CLowerVaridDerivin, DLowerVaridDerivin, ELowerVaridDerivin, FLowerVaridDerivin, GLowerVaridDerivin, HLowerVaridDerivin, ILowerVaridDerivin, JLowerVaridDerivin, KLowerVaridDerivin, LLowerVaridDerivin, MLowerVaridDerivin, NLowerVaridDerivin, OLowerVaridDerivin, PLowerVaridDerivin, QLowerVaridDerivin, RLowerVaridDerivin, SLowerVaridDerivin, TLowerVaridDerivin, ULowerVaridDerivin, VLowerVaridDerivin, WLowerVaridDerivin, XLowerVaridDerivin, YLowerVaridDerivin, ZLowerVaridDerivin),
	VaridInstancBase(EOPVaridInstanc, InnerSansAscVaridInstanc, ALowerVaridInstanc, BLowerVaridInstanc, CLowerVaridInstanc, DLowerVaridInstanc, ELowerVaridInstanc, FLowerVaridInstanc, GLowerVaridInstanc, HLowerVaridInstanc, ILowerVaridInstanc, JLowerVaridInstanc, KLowerVaridInstanc, LLowerVaridInstanc, MLowerVaridInstanc, NLowerVaridInstanc, OLowerVaridInstanc, PLowerVaridInstanc, QLowerVaridInstanc, RLowerVaridInstanc, SLowerVaridInstanc, TLowerVaridInstanc, ULowerVaridInstanc, VLowerVaridInstanc, WLowerVaridInstanc, XLowerVaridInstanc, YLowerVaridInstanc, ZLowerVaridInstanc),

	SymbolSansAscBase(UnicodeNonspecialNonscorequoteSymbolSansAsc),
	UniSymbolSansSpecialishAscBase(UnicodeSymbolSansSpecialishAsc),
	SymbolSansHyphenBase(AsciiNonspecialSymbolSansHyphen, UnicodeNonspecialNonscorequoteSymbolSansHyphen),
	AscSymbolSansHyphenBase(ExclamationAsciiSymbolSansHyphen, HashAsciiSymbolSansHyphen, DollarAsciiSymbolSansHyphen, PercentAsciiSymbolSansHyphen, AmpersandAsciiSymbolSansHyphen, AsteriskAsciiSymbolSansHyphen, PlusAsciiSymbolSansHyphen, DotAsciiSymbolSansHyphen, SlashAsciiSymbolSansHyphen, LeftAngleBracketAsciiSymbolSansHyphen, EqualsAsciiSymbolSansHyphen, RightAngleBracketAsciiSymbolSansHyphen, QuestionMarkAsciiSymbolSansHyphen, AtAsciiSymbolSansHyphen, BackslashAsciiSymbolSansHyphen, CaretAsciiSymbolSansHyphen, PipeAsciiSymbolSansHyphen, TildeAsciiSymbolSansHyphen, ColonAsciiSymbolSansHyphen),
	UniSymbolSansSpecialishHyphenBase(UnicodeSymbolSansSpecialishHyphen),
	VarSymBase(UniVarSym, ExclamationVarSym, HashVarSym, DollarVarSym, PercentVarSym, AmpersandVarSym, AsteriskVarSym, PlusVarSym, DotVarSym, SlashVarSym, LeftAngleBracketVarSym, RightAngleBracketVarSym, QuestionMarkVarSym, CaretVarSym, HyphenVarSym, DotExclamationVarSym, DotHashVarSym, DotDollarVarSym, DotPercentVarSym, DotAmpersandVarSym, DotAsteriskVarSym, DotPlusVarSym, DotSlashVarSym, DotLeftAngleBracketVarSym, DotEqualsVarSym, DotRightAngleBracketVarSym, DotQuestionMarkVarSym, DotAtVarSym, DotBackslashVarSym, DotCaretVarSym, DotPipeVarSym, DotHyphenVarSym, DotTildeVarSym, DotColonVarSym, LeftAngleBracketExclamationVarSym, LeftAngleBracketHashVarSym, LeftAngleBracketDollarVarSym, LeftAngleBracketPercentVarSym, LeftAngleBracketAmpersandVarSym, LeftAngleBracketAsteriskVarSym, LeftAngleBracketPlusVarSym, LeftAngleBracketDotVarSym, LeftAngleBracketSlashVarSym, LeftAngleBracketLeftAngleBracketVarSym, LeftAngleBracketEqualsVarSym, LeftAngleBracketRightAngleBracketVarSym, LeftAngleBracketQuestionMarkVarSym, LeftAngleBracketAtVarSym, LeftAngleBracketBackslashVarSym, LeftAngleBracketCaretVarSym, LeftAngleBracketPipeVarSym, LeftAngleBracketTildeVarSym, LeftAngleBracketColonVarSym, EqualsExclamationVarSym, EqualsHashVarSym, EqualsDollarVarSym, EqualsPercentVarSym, EqualsAmpersandVarSym, EqualsAsteriskVarSym, EqualsPlusVarSym, EqualsDotVarSym, EqualsSlashVarSym, EqualsLeftAngleBracketVarSym, EqualsEqualsVarSym, EqualsQuestionMarkVarSym, EqualsAtVarSym, EqualsBackslashVarSym, EqualsCaretVarSym, EqualsPipeVarSym, EqualsHyphenVarSym, EqualsTildeMarkVarSym, EqualsColonMarkVarSym, AtVarSym, BackslashVarSym, PipeVarSym, HyphenExclamationVarSym, HyphenHashVarSym, HyphenDollarVarSym, HyphenPercentVarSym, HyphenAmpersandVarSym, HyphenAsteriskVarSym, HyphenPlusVarSym, HyphenDotVarSym, HyphenSlashVarSym, HyphenLeftAngleBracketVarSym, HyphenEqualsVarSym, HyphenQuestionMarkVarSym, HyphenAtVarSym, HyphenBackslashVarSym, HyphenCaretVarSym, HyphenPipeVarSym, HyphenTildeVarSym, HyphenColonVarSym, TildeVarSym, ColonExclamationVarSym, ColonHashVarSym, ColonDollarVarSym, ColonPercentVarSym, ColonAmpersandVarSym, ColonAsteriskVarSym, ColonPlusVarSym, ColonDotVarSym, ColonSlashVarSym, ColonLeftAngleBracketVarSym, ColonEqualsVarSym, ColonRightAngleBracketVarSym, ColonQuestionMarkVarSym, ColonAtVarSym, ColonBackslashVarSym, ColonCaretVarSym, ColonPipeVarSym, ColonHyphenVarSym, ColonTildeVarSym, DotDotVarSym, LeftAngleBracketHyphenVarSym, EqualsRightAngleBracketVarSym, HyphenRightAngleBracketVarSym, HyphenHyphenVarSym),

	SymbolSansColonBase(AsciiNonspecialSymbolSansColon, UnicodeNonspecialNonscorequoteSymbolSansColon),
	AscSymbolSansColonBase(ExclamationAsciiSymbolSansColon, HashAsciiSymbolSansColon, DollarAsciiSymbolSansColon, PercentAsciiSymbolSansColon, AmpersandAsciiSymbolSansColon, AsteriskAsciiSymbolSansColon, PlusAsciiSymbolSansColon, DotAsciiSymbolSansColon, SlashAsciiSymbolSansColon, LeftAngleBracketAsciiSymbolSansColon, EqualsAsciiSymbolSansColon, RightAngleBracketAsciiSymbolSansColon, QuestionMarkAsciiSymbolSansColon, AtAsciiSymbolSansColon, BackslashAsciiSymbolSansColon, CaretAsciiSymbolSansColon, PipeAsciiSymbolSansColon, HyphenAsciiSymbolSansColon, TildeAsciiSymbolSansColon),
	UniSymbolSansSpecialishColonBase(UnicodeSymbolSansSpecialishColon),
	ConSymBase(Len3ConSym, Len2ConSym),
) where

import Prelude ()

{-
 - ----------------------------------------------------------------
 - Base structures.
 - ----------------------------------------------------------------
 -}

-- § 2.4 Identifiers and Operators types.

-- Exclusion structures types.

-- | A restricted VaridInnerBase, without ascSmall, to help build 'VaridBase'.
data VaridInnerSansAscSmallUnderscoreBase smallSansAscSmallUnderscore large digit lexicalSingleQuote annotation fixpoint =
	  SmallVaridInnerSansAscSmallUnderscore            annotation smallSansAscSmallUnderscore
	| LargeVaridInnerSansAscSmallUnderscore            annotation large
	| DigitVaridInnerSansAscSmallUnderscore            annotation digit
	| SingleQuoteVaridInnerInnerSansAscSmallUnderscore annotation lexicalSingleQuote

-- | A lowercase character, except for those that are ASCII, and except underscores.
--
-- We'll build up ASCII characters more carefully in the restricted sets.
data SmallSansAscSmallUnderscoreBase ascSmall uniSmallSansAscSmallUnderscore lexicalUnderscore annotation fixpoint =
	UnicodeSmallSansAscSmallUnderscore annotation uniSmallSansAscSmallUnderscore

-- | A unicode lowercase character, except those that are ASCII.
--
-- We'll build up ASCII characters more carefully in the restricted sets.
data UniSmallSansAscBase lexicalUnicodeSmallSansAscUnderscore annotation fixpoint =
	UnicodeSmallUniSmallSansAsc annotation lexicalUnicodeSmallSansAscUnderscore

-- | A non-symbolic variable (lowercase-style) identifier name, excluding
-- reserved names.
--
-- The structures are set up specially to exclude reserved names.
--
-- Our approach to exclude reserved identifiers is to allow only and exactly
-- the construction of names that we know are not excluded.  We do it a long
-- way essentially through the pattern we use inside, of manually checking each
-- possible next character in the set (ascii lowercase) we are restricting.
--
-- This structure represents from the beginning of a 'varid' (0 parsed so far
-- for this token).
data VaridBase list smallSansAscSmallUnderscore varidInner lexicalALower lexicalBLower lexicalCLower varidC lexicalDLower varidD lexicalELower varidE lexicalFLower varidF lexicalGLower lexicalHLower lexicalILower varidI lexicalJLower lexicalKLower lexicalLLower varidL lexicalMLower varidM lexicalNLower varidN lexicalOLower varidO lexicalPLower lexicalQLower lexicalRLower lexicalSLower lexicalTLower varidT lexicalULower lexicalVLower lexicalWLower varidW lexicalXLower lexicalYLower lexicalZLower annotation fixpoint =
	-- (We could include this if empty identifiers are included, but they are not:)
	{-
	  EOPVarid          annotation lexicalEndOfParse
		-- ^ We're provided with a certification that no more characters will
		-- follow in whatever we're parsing, so this identifier is empty.
	-}

	  SmallSansAscVarid annotation smallSansAscSmallUnderscore (list varidInner)
		-- ^ Smalls we know aren't excluded are valid starts.  In fact in this
		-- case we need no further verification, since it's outside the set we
		-- are concerned with.

	| ALowerVarid       annotation lexicalALower (list varidInner)
		-- ^ ‘a’ doesn't start any excluded reservedid; the rest needs no
		-- further verification.

	| BLowerVarid       annotation lexicalBLower (list varidInner)
		-- ^ ‘a’ doesn't start any excluded reservedid; the rest needs no
		-- further verification.

	| CLowerVarid       annotation lexicalCLower varidC
		-- ^ ‘c’ starts 2 excluded identifiers.

	| DLowerVarid       annotation lexicalDLower varidD
		-- ^ ‘d’ starts 4 excluded identifiers.

	| ELowerVarid       annotation lexicalELower varidE
		-- ^ ‘e’ starts 1 excluded identifier.

	| FLowerVarid       annotation lexicalFLower varidF
		-- ^ ‘f’ starts 1 excluded identifier.

	| GLowerVarid       annotation lexicalGLower (list varidInner)
		-- ^ ‘g’ doesn't start any excluded reservedid; the rest needs no
		-- further verification.

	| HLowerVarid       annotation lexicalHLower (list varidInner)
		-- ^ ‘h’ doesn't start any excluded reservedid; the rest needs no
		-- further verification.

	| ILowerVarid       annotation lexicalILower varidI
		-- ‘i’ starts 7 excluded identifiers.

	| JLowerVarid       annotation lexicalJLower (list varidInner)
		-- ^ ‘j’ doesn't start any excluded reservedid; the rest needs no
		-- further verification.

	| KLowerVarid       annotation lexicalKLower (list varidInner)
		-- ^ ‘k’ doesn't start any excluded reservedid; the rest needs no
		-- further verification.

	| LLowerVarid       annotation lexicalLLower varidL
		-- ^ ‘l’ starts 7 excluded identifiers.

	| MLowerVarid       annotation lexicalMLower varidM
		-- ^ ‘m’ starts 1 excluded identifier.

	| NLowerVarid       annotation lexicalNLower varidN
		-- ^ ‘n’ starts 1 excluded identifier.

	| OLowerVarid       annotation lexicalOLower varidO
		-- ^ ‘n’ starts 1 excluded identifier.

	| PLowerVarid       annotation lexicalPLower (list varidInner)
		-- ^ ‘p’ doesn't start any excluded reservedid; the rest needs no
		-- further verification.

	| QLowerVarid       annotation lexicalQLower (list varidInner)
		-- ^ ‘q’ doesn't start any excluded reservedid; the rest needs no
		-- further verification.

	| RLowerVarid       annotation lexicalRLower (list varidInner)
		-- ^ ‘r’ doesn't start any excluded reservedid; the rest needs no
		-- further verification.

	| SLowerVarid       annotation lexicalSLower (list varidInner)
		-- ^ ‘s’ doesn't start any excluded reservedid; the rest needs no
		-- further verification.

	| TLowerVarid       annotation lexicalTLower varidT
		-- ^ ‘t’ starts 2 excluded identifiers.

	| ULowerVarid       annotation lexicalULower (list varidInner)
		-- ^ ‘u’ doesn't start any excluded reservedid; the rest needs no
		-- further verification.

	| VLowerVarid       annotation lexicalVLower (list varidInner)
		-- ^ ‘v’ doesn't start any excluded reservedid; the rest needs no
		-- further verification.

	| WLowerVarid       annotation lexicalWLower varidW
		-- ^ ‘w’ starts 1 excluded identifier.

	| XLowerVarid       annotation lexicalXLower (list varidInner)
		-- ^ ‘x’ doesn't start any excluded reservedid; the rest needs no
		-- further verification.

	| YLowerVarid       annotation lexicalYLower (list varidInner)
		-- ^ ‘y’ doesn't start any excluded reservedid; the rest needs no
		-- further verification.

	| ZLowerVarid       annotation lexicalZLower (list varidInner)
		-- ^ ‘z’ doesn't start any excluded reservedid; the rest needs no
		-- further verification.

-- | A non-symbolic variable (lowercase-style) identifier name, excluding
-- reserved names, after a ‘c’ was parsed from the beginning.
data VaridCBase list lexicalEndOfParse varidInnerSansAscSmallUnderscore varidInner lexicalALower varidCa lexicalBLower lexicalCLower lexicalDLower lexicalELower lexicalFLower lexicalGLower lexicalHLower lexicalILower lexicalJLower lexicalKLower lexicalLLower varidCl lexicalMLower lexicalNLower lexicalOLower lexicalPLower lexicalQLower lexicalRLower lexicalSLower lexicalTLower lexicalULower lexicalVLower lexicalWLower lexicalXLower lexicalYLower lexicalZLower annotation fixpoint =
	  EOPVaridC          annotation lexicalEndOfParse
		-- ^ We're provided with a certification that no more characters will
		-- follow in whatever we're parsing, and ‘c’ is not excluded.
	| InnerSansAscVaridC annotation varidInnerSansAscSmallUnderscore (list varidInner)
		-- ^ Characters we know aren't excluded are valid characters.
	| ALowerVaridC       annotation lexicalALower                    varidCa
		-- ^ ‘ca’ starts 1 excluded identifier.
	| BLowerVaridC       annotation lexicalBLower                    (list varidInner)
		-- ^ ‘cb’ starts no excluded identifier.
	| CLowerVaridC       annotation lexicalCLower                    (list varidInner)
		-- ^ ‘cc’ starts no excluded identifier.
	| DLowerVaridC       annotation lexicalDLower                    (list varidInner)
		-- ^ ‘cd’ starts no excluded identifier.
	| ELowerVaridC       annotation lexicalELower                    (list varidInner)
		-- ^ ‘ce’ starts no excluded identifier.
	| FLowerVaridC       annotation lexicalFLower                    (list varidInner)
		-- ^ ‘cf’ starts no excluded identifier.
	| GLowerVaridC       annotation lexicalGLower                    (list varidInner)
		-- ^ ‘cg’ starts no excluded identifier.
	| HLowerVaridC       annotation lexicalHLower                    (list varidInner)
		-- ^ ‘ch’ starts no excluded identifier.
	| ILowerVaridC       annotation lexicalILower                    (list varidInner)
		-- ^ ‘ci’ starts no excluded identifier.
	| JLowerVaridC       annotation lexicalJLower                    (list varidInner)
		-- ^ ‘cj’ starts no excluded identifier.
	| KLowerVaridC       annotation lexicalKLower                    (list varidInner)
		-- ^ ‘ck’ starts no excluded identifier.
	| LLowerVaridC       annotation lexicalLLower                    varidCl
		-- ^ ‘cl’ starts 1 excluded identifier.
	| MLowerVaridC       annotation lexicalMLower                    (list varidInner)
		-- ^ ‘cm’ starts no excluded identifier.
	| NLowerVaridC       annotation lexicalNLower                    (list varidInner)
		-- ^ ‘cn’ starts no excluded identifier.
	| OLowerVaridC       annotation lexicalOLower                    (list varidInner)
		-- ^ ‘co’ starts no excluded identifier.
	| PLowerVaridC       annotation lexicalPLower                    (list varidInner)
		-- ^ ‘cp’ starts no excluded identifier.
	| QLowerVaridC       annotation lexicalQLower                    (list varidInner)
		-- ^ ‘cq’ starts no excluded identifier.
	| RLowerVaridC       annotation lexicalRLower                    (list varidInner)
		-- ^ ‘cr’ starts no excluded identifier.
	| SLowerVaridC       annotation lexicalSLower                    (list varidInner)
		-- ^ ‘cs’ starts no excluded identifier.
	| TLowerVaridC       annotation lexicalTLower                    (list varidInner)
		-- ^ ‘ct’ starts no excluded identifier.
	| ULowerVaridC       annotation lexicalULower                    (list varidInner)
		-- ^ ‘cu’ starts no excluded identifier.
	| VLowerVaridC       annotation lexicalVLower                    (list varidInner)
		-- ^ ‘cv’ starts no excluded identifier.
	| WLowerVaridC       annotation lexicalWLower                    (list varidInner)
		-- ^ ‘cw’ starts no excluded identifier.
	| XLowerVaridC       annotation lexicalXLower                    (list varidInner)
		-- ^ ‘cx’ starts no excluded identifier.
	| YLowerVaridC       annotation lexicalYLower                    (list varidInner)
		-- ^ ‘cy’ starts no excluded identifier.
	| ZLowerVaridC       annotation lexicalZLower                    (list varidInner)
		-- ^ ‘cz’ starts no excluded identifier.

-- | A non-symbolic variable (lowercase-style) identifier name, excluding
-- reserved names, after a ‘d’ was parsed from the beginning.
data VaridDBase list lexicalEndOfParse varidInnerSansAscSmallUnderscore varidInner lexicalALower varidDa lexicalBLower lexicalCLower lexicalDLower varidDe lexicalELower lexicalFLower lexicalGLower lexicalHLower lexicalILower lexicalJLower lexicalKLower lexicalLLower lexicalMLower lexicalNLower lexicalOLower lexicalPLower lexicalQLower lexicalRLower lexicalSLower lexicalTLower lexicalULower lexicalVLower lexicalWLower lexicalXLower lexicalYLower lexicalZLower annotation fixpoint =
	  EOPVaridD          annotation lexicalEndOfParse
		-- ^ We're provided with a certification that no more characters will
		-- follow in whatever we're parsing, and ‘d’ is not excluded.
	| InnerSansAscVaridD annotation varidInnerSansAscSmallUnderscore (list varidInner)
		-- ^ Characters we know aren't excluded are valid characters.
	| ALowerVaridD       annotation lexicalALower                    varidDa
		-- ^ ‘da’ starts 1 excluded identifier.
	| BLowerVaridD       annotation lexicalBLower                    (list varidInner)
		-- ^ ‘db’ starts no excluded identifier.
	| CLowerVaridD       annotation lexicalCLower                    (list varidInner)
		-- ^ ‘dc’ starts no excluded identifier.
	| DLowerVaridD       annotation lexicalDLower                    (list varidInner)
		-- ^ ‘dd’ starts no excluded identifier.
	| ELowerVaridD       annotation lexicalELower                    varidDe
		-- ^ ‘de’ starts 2 excluded identifiers.
	| FLowerVaridD       annotation lexicalFLower                    (list varidInner)
		-- ^ ‘df’ starts no excluded identifier.
	| GLowerVaridD       annotation lexicalGLower                    (list varidInner)
		-- ^ ‘dg’ starts no excluded identifier.
	| HLowerVaridD       annotation lexicalHLower                    (list varidInner)
		-- ^ ‘dh’ starts no excluded identifier.
	| ILowerVaridD       annotation lexicalILower                    (list varidInner)
		-- ^ ‘di’ starts no excluded identifier.
	| JLowerVaridD       annotation lexicalJLower                    (list varidInner)
		-- ^ ‘dj’ starts no excluded identifier.
	| KLowerVaridD       annotation lexicalKLower                    (list varidInner)
		-- ^ ‘dk’ starts no excluded identifier.
	| LLowerVaridD       annotation lexicalLLower                    (list varidInner)
		-- ^ ‘dl’ starts no excluded identifier.
	| MLowerVaridD       annotation lexicalMLower                    (list varidInner)
		-- ^ ‘dm’ starts no excluded identifier.
	| NLowerVaridD       annotation lexicalNLower                    (list varidInner)
		-- ^ ‘dn’ starts no excluded identifier.
	| OLowerVaridD       annotation lexicalOLower                    varidInner        (list varidInner)
		-- ^ ‘do’ alone is excluded.  Additional characters is valid.
	| PLowerVaridD       annotation lexicalPLower                    (list varidInner)
		-- ^ ‘dp’ starts no excluded identifier.
	| QLowerVaridD       annotation lexicalQLower                    (list varidInner)
		-- ^ ‘dq’ starts no excluded identifier.
	| RLowerVaridD       annotation lexicalRLower                    (list varidInner)
		-- ^ ‘dr’ starts no excluded identifier.
	| SLowerVaridD       annotation lexicalSLower                    (list varidInner)
		-- ^ ‘ds’ starts no excluded identifier.
	| TLowerVaridD       annotation lexicalTLower                    (list varidInner)
		-- ^ ‘dt’ starts no excluded identifier.
	| ULowerVaridD       annotation lexicalULower                    (list varidInner)
		-- ^ ‘du’ starts no excluded identifier.
	| VLowerVaridD       annotation lexicalVLower                    (list varidInner)
		-- ^ ‘dv’ starts no excluded identifier.
	| WLowerVaridD       annotation lexicalWLower                    (list varidInner)
		-- ^ ‘dw’ starts no excluded identifier.
	| XLowerVaridD       annotation lexicalXLower                    (list varidInner)
		-- ^ ‘dx’ starts no excluded identifier.
	| YLowerVaridD       annotation lexicalYLower                    (list varidInner)
		-- ^ ‘dy’ starts no excluded identifier.
	| ZLowerVaridD       annotation lexicalZLower                    (list varidInner)
		-- ^ ‘dz’ starts no excluded identifier.

-- | A non-symbolic variable (lowercase-style) identifier name, excluding
-- reserved names, after a ‘e’ was parsed from the beginning.
data VaridEBase list lexicalEndOfParse varidInnerSansAscSmallUnderscore varidInner lexicalALower lexicalBLower lexicalCLower lexicalDLower lexicalELower lexicalFLower lexicalGLower lexicalHLower lexicalILower lexicalJLower lexicalKLower lexicalLLower varidEl lexicalMLower lexicalNLower lexicalOLower lexicalPLower lexicalQLower lexicalRLower lexicalSLower lexicalTLower lexicalULower lexicalVLower lexicalWLower lexicalXLower lexicalYLower lexicalZLower annotation fixpoint =
	  EOPVaridE          annotation lexicalEndOfParse
		-- ^ We're provided with a certification that no more characters will
		-- follow in whatever we're parsing, and ‘e’ is not excluded.
	| InnerSansAscVaridE annotation varidInnerSansAscSmallUnderscore (list varidInner)
		-- ^ Characters we know aren't excluded are valid characters.
	| ALowerVaridE       annotation lexicalALower                    (list varidInner)
		-- ^ ‘ea’ starts no excluded identifier.
	| BLowerVaridE       annotation lexicalBLower                    (list varidInner)
		-- ^ ‘eb’ starts no excluded identifier.
	| CLowerVaridE       annotation lexicalCLower                    (list varidInner)
		-- ^ ‘ec’ starts no excluded identifier.
	| DLowerVaridE       annotation lexicalDLower                    (list varidInner)
		-- ^ ‘ed’ starts no excluded identifier.
	| ELowerVaridE       annotation lexicalELower                    (list varidInner)
		-- ^ ‘ee’ starts no excluded identifier.
	| FLowerVaridE       annotation lexicalFLower                    (list varidInner)
		-- ^ ‘ef’ starts no excluded identifier.
	| GLowerVaridE       annotation lexicalGLower                    (list varidInner)
		-- ^ ‘eg’ starts no excluded identifier.
	| HLowerVaridE       annotation lexicalHLower                    (list varidInner)
		-- ^ ‘eh’ starts no excluded identifier.
	| ILowerVaridE       annotation lexicalILower                    (list varidInner)
		-- ^ ‘ei’ starts no excluded identifier.
	| JLowerVaridE       annotation lexicalJLower                    (list varidInner)
		-- ^ ‘ej’ starts no excluded identifier.
	| KLowerVaridE       annotation lexicalKLower                    (list varidInner)
		-- ^ ‘ek’ starts no excluded identifier.
	| LLowerVaridE       annotation lexicalLLower                    varidEl
		-- ^ ‘el’ starts 1 excluded identifier.
	| MLowerVaridE       annotation lexicalMLower                    (list varidInner)
		-- ^ ‘em’ starts no excluded identifier.
	| NLowerVaridE       annotation lexicalNLower                    (list varidInner)
		-- ^ ‘en’ starts no excluded identifier.
	| OLowerVaridE       annotation lexicalOLower                    (list varidInner)
		-- ^ ‘eo’ starts no excluded identifier.
	| PLowerVaridE       annotation lexicalPLower                    (list varidInner)
		-- ^ ‘ep’ starts no excluded identifier.
	| QLowerVaridE       annotation lexicalQLower                    (list varidInner)
		-- ^ ‘eq’ starts no excluded identifier.
	| RLowerVaridE       annotation lexicalRLower                    (list varidInner)
		-- ^ ‘er’ starts no excluded identifier.
	| SLowerVaridE       annotation lexicalSLower                    (list varidInner)
		-- ^ ‘es’ starts no excluded identifier.
	| TLowerVaridE       annotation lexicalTLower                    (list varidInner)
		-- ^ ‘et’ starts no excluded identifier.
	| ULowerVaridE       annotation lexicalULower                    (list varidInner)
		-- ^ ‘eu’ starts no excluded identifier.
	| VLowerVaridE       annotation lexicalVLower                    (list varidInner)
		-- ^ ‘ev’ starts no excluded identifier.
	| WLowerVaridE       annotation lexicalWLower                    (list varidInner)
		-- ^ ‘ew’ starts no excluded identifier.
	| XLowerVaridE       annotation lexicalXLower                    (list varidInner)
		-- ^ ‘ex’ starts no excluded identifier.
	| YLowerVaridE       annotation lexicalYLower                    (list varidInner)
		-- ^ ‘ey’ starts no excluded identifier.
	| ZLowerVaridE       annotation lexicalZLower                    (list varidInner)
		-- ^ ‘ez’ starts no excluded identifier.

-- | A non-symbolic variable (lowercase-style) identifier name, excluding
-- reserved names, after a ‘f’ was parsed from the beginning.
data VaridFBase list lexicalEndOfParse varidInnerSansAscSmallUnderscore varidInner lexicalALower lexicalBLower lexicalCLower lexicalDLower lexicalELower lexicalFLower lexicalGLower lexicalHLower lexicalILower lexicalJLower lexicalKLower lexicalLLower lexicalMLower lexicalNLower lexicalOLower varidFo lexicalPLower lexicalQLower lexicalRLower lexicalSLower lexicalTLower lexicalULower lexicalVLower lexicalWLower lexicalXLower lexicalYLower lexicalZLower annotation fixpoint =
	  EOPVaridF          annotation lexicalEndOfParse
		-- ^ We're provided with a certification that no more characters will
		-- follow in whatever we're parsing, and ‘f’ is not excluded.
	| InnerSansAscVaridF annotation varidInnerSansAscSmallUnderscore (list varidInner)
		-- ^ Characters we know aren't excluded are valid characters.
	| ALowerVaridF       annotation lexicalALower                    (list varidInner)
		-- ^ ‘fa’ starts no excluded identifier.
	| BLowerVaridF       annotation lexicalBLower                    (list varidInner)
		-- ^ ‘fb’ starts no excluded identifier.
	| CLowerVaridF       annotation lexicalCLower                    (list varidInner)
		-- ^ ‘fc’ starts no excluded identifier.
	| DLowerVaridF       annotation lexicalDLower                    (list varidInner)
		-- ^ ‘fd’ starts no excluded identifier.
	| ELowerVaridF       annotation lexicalELower                    (list varidInner)
		-- ^ ‘fe’ starts no excluded identifier.
	| FLowerVaridF       annotation lexicalFLower                    (list varidInner)
		-- ^ ‘ff’ starts no excluded identifier.
	| GLowerVaridF       annotation lexicalGLower                    (list varidInner)
		-- ^ ‘fg’ starts no excluded identifier.
	| HLowerVaridF       annotation lexicalHLower                    (list varidInner)
		-- ^ ‘fh’ starts no excluded identifier.
	| ILowerVaridF       annotation lexicalILower                    (list varidInner)
		-- ^ ‘fi’ starts no excluded identifier.
	| JLowerVaridF       annotation lexicalJLower                    (list varidInner)
		-- ^ ‘fj’ starts no excluded identifier.
	| KLowerVaridF       annotation lexicalKLower                    (list varidInner)
		-- ^ ‘fk’ starts no excluded identifier.
	| LLowerVaridF       annotation lexicalLLower                    (list varidInner)
		-- ^ ‘fl’ starts no excluded identifier.
	| MLowerVaridF       annotation lexicalMLower                    (list varidInner)
		-- ^ ‘fm’ starts no excluded identifier.
	| NLowerVaridF       annotation lexicalNLower                    (list varidInner)
		-- ^ ‘fn’ starts no excluded identifier.
	| OLowerVaridF       annotation lexicalOLower                    varidFo
		-- ^ ‘fo’ starts 1 excluded identifier.
	| PLowerVaridF       annotation lexicalPLower                    (list varidInner)
		-- ^ ‘fp’ starts no excluded identifier.
	| QLowerVaridF       annotation lexicalQLower                    (list varidInner)
		-- ^ ‘fq’ starts no excluded identifier.
	| RLowerVaridF       annotation lexicalRLower                    (list varidInner)
		-- ^ ‘fr’ starts no excluded identifier.
	| SLowerVaridF       annotation lexicalSLower                    (list varidInner)
		-- ^ ‘fs’ starts no excluded identifier.
	| TLowerVaridF       annotation lexicalTLower                    (list varidInner)
		-- ^ ‘ft’ starts no excluded identifier.
	| ULowerVaridF       annotation lexicalULower                    (list varidInner)
		-- ^ ‘fu’ starts no excluded identifier.
	| VLowerVaridF       annotation lexicalVLower                    (list varidInner)
		-- ^ ‘fv’ starts no excluded identifier.
	| WLowerVaridF       annotation lexicalWLower                    (list varidInner)
		-- ^ ‘fw’ starts no excluded identifier.
	| XLowerVaridF       annotation lexicalXLower                    (list varidInner)
		-- ^ ‘fx’ starts no excluded identifier.
	| YLowerVaridF       annotation lexicalYLower                    (list varidInner)
		-- ^ ‘fy’ starts no excluded identifier.
	| ZLowerVaridF       annotation lexicalZLower                    (list varidInner)
		-- ^ ‘fz’ starts no excluded identifier.

-- | A non-symbolic variable (lowercase-style) identifier name, excluding
-- reserved names, after a ‘i’ was parsed from the beginning.
data VaridIBase list lexicalEndOfParse varidInnerSansAscSmallUnderscore varidInner lexicalALower lexicalBLower lexicalCLower lexicalDLower lexicalELower lexicalFLower lexicalGLower lexicalHLower lexicalILower lexicalJLower lexicalKLower lexicalLLower lexicalMLower varidIm lexicalNLower varidIn lexicalOLower lexicalPLower lexicalQLower lexicalRLower lexicalSLower lexicalTLower lexicalULower lexicalVLower lexicalWLower lexicalXLower lexicalYLower lexicalZLower annotation fixpoint =
	  EOPVaridI          annotation lexicalEndOfParse
		-- ^ We're provided with a certification that no more characters will
		-- follow in whatever we're parsing, and ‘i’ is not excluded.
	| InnerSansAscVaridI annotation varidInnerSansAscSmallUnderscore (list varidInner)
		-- ^ Characters we know aren't excluded are valid characters.
	| ALowerVaridI       annotation lexicalALower                    (list varidInner)
		-- ^ ‘ia’ starts no excluded identifier.
	| BLowerVaridI       annotation lexicalBLower                    (list varidInner)
		-- ^ ‘ib’ starts no excluded identifier.
	| CLowerVaridI       annotation lexicalCLower                    (list varidInner)
		-- ^ ‘ic’ starts no excluded identifier.
	| DLowerVaridI       annotation lexicalDLower                    (list varidInner)
		-- ^ ‘id’ starts no excluded identifier.
	| ELowerVaridI       annotation lexicalELower                    (list varidInner)
		-- ^ ‘ie’ starts no excluded identifier.
	| FLowerVaridI       annotation lexicalFLower                    (list varidInner)
		-- ^ ‘if’ alone is excluded.  Additional characters is valid.
	| GLowerVaridI       annotation lexicalGLower                    (list varidInner)
		-- ^ ‘ig’ starts no excluded identifier.
	| HLowerVaridI       annotation lexicalHLower                    (list varidInner)
		-- ^ ‘ih’ starts no excluded identifier.
	| ILowerVaridI       annotation lexicalILower                    (list varidInner)
		-- ^ ‘ii’ starts no excluded identifier.
	| JLowerVaridI       annotation lexicalJLower                    (list varidInner)
		-- ^ ‘ij’ starts no excluded identifier.
	| KLowerVaridI       annotation lexicalKLower                    (list varidInner)
		-- ^ ‘ik’ starts no excluded identifier.
	| LLowerVaridI       annotation lexicalLLower                    (list varidInner)
		-- ^ ‘il’ starts no excluded identifier.
	| MLowerVaridI       annotation lexicalMLower                    varidIm
		-- ^ ‘im’ starts 1 excluded identifier.
	| NLowerVaridI       annotation lexicalNLower                    varidIn
		-- ^ ‘in’ starts 5 excluded identifiers.
	| OLowerVaridI       annotation lexicalOLower                    (list varidInner)
		-- ^ ‘io’ starts no excluded identifier.
	| PLowerVaridI       annotation lexicalPLower                    (list varidInner)
		-- ^ ‘ip’ starts no excluded identifier.
	| QLowerVaridI       annotation lexicalQLower                    (list varidInner)
		-- ^ ‘iq’ starts no excluded identifier.
	| RLowerVaridI       annotation lexicalRLower                    (list varidInner)
		-- ^ ‘ir’ starts no excluded identifier.
	| SLowerVaridI       annotation lexicalSLower                    (list varidInner)
		-- ^ ‘is’ starts no excluded identifier.
	| TLowerVaridI       annotation lexicalTLower                    (list varidInner)
		-- ^ ‘it’ starts no excluded identifier.
	| ULowerVaridI       annotation lexicalULower                    (list varidInner)
		-- ^ ‘iu’ starts no excluded identifier.
	| VLowerVaridI       annotation lexicalVLower                    (list varidInner)
		-- ^ ‘iv’ starts no excluded identifier.
	| WLowerVaridI       annotation lexicalWLower                    (list varidInner)
		-- ^ ‘iw’ starts no excluded identifier.
	| XLowerVaridI       annotation lexicalXLower                    (list varidInner)
		-- ^ ‘ix’ starts no excluded identifier.
	| YLowerVaridI       annotation lexicalYLower                    (list varidInner)
		-- ^ ‘iy’ starts no excluded identifier.
	| ZLowerVaridI       annotation lexicalZLower                    (list varidInner)
		-- ^ ‘iz’ starts no excluded identifier.

-- | A non-symbolic variable (lowercase-style) identifier name, excluding
-- reserved names, after an ‘l’ was parsed from the beginning.
data VaridLBase list lexicalEndOfParse varidInnerSansAscSmallUnderscore varidInner lexicalALower lexicalBLower lexicalCLower lexicalDLower lexicalELower varidLe lexicalFLower lexicalGLower lexicalHLower lexicalILower lexicalJLower lexicalKLower lexicalLLower lexicalMLower lexicalNLower lexicalOLower lexicalPLower lexicalQLower lexicalRLower lexicalSLower lexicalTLower lexicalULower lexicalVLower lexicalWLower lexicalXLower lexicalYLower lexicalZLower annotation fixpoint =
	  EOPVaridL          annotation lexicalEndOfParse
		-- ^ We're provided with a certification that no more characters will
		-- follow in whatever we're parsing, and ‘l’ is not excluded.
	| InnerSansAscVaridL annotation varidInnerSansAscSmallUnderscore (list varidInner)
		-- ^ Characters we know aren't excluded are valid characters.
	| ALowerVaridL       annotation lexicalALower                    (list varidInner)
		-- ^ ‘la’ starts no excluded identifier.
	| BLowerVaridL       annotation lexicalBLower                    (list varidInner)
		-- ^ ‘lb’ starts no excluded identifier.
	| CLowerVaridL       annotation lexicalCLower                    (list varidInner)
		-- ^ ‘lc’ starts no excluded identifier.
	| DLowerVaridL       annotation lexicalDLower                    (list varidInner)
		-- ^ ‘ld’ starts no excluded identifier.
	| ELowerVaridL       annotation lexicalELower                    varidLe
		-- ^ ‘le’ starts 1 excluded identifier.
	| FLowerVaridL       annotation lexicalFLower                    (list varidInner)
		-- ^ ‘lf’ starts no excluded identifier.
	| GLowerVaridL       annotation lexicalGLower                    (list varidInner)
		-- ^ ‘lg’ starts no excluded identifier.
	| HLowerVaridL       annotation lexicalHLower                    (list varidInner)
		-- ^ ‘lh’ starts no excluded identifier.
	| ILowerVaridL       annotation lexicalILower                    (list varidInner)
		-- ^ ‘li’ starts no excluded identifier.
	| JLowerVaridL       annotation lexicalJLower                    (list varidInner)
		-- ^ ‘lj’ starts no excluded identifier.
	| KLowerVaridL       annotation lexicalKLower                    (list varidInner)
		-- ^ ‘lk’ starts no excluded identifier.
	| LLowerVaridL       annotation lexicalLLower                    (list varidInner)
		-- ^ ‘ll’ starts no excluded identifier.
	| MLowerVaridL       annotation lexicalMLower                    (list varidInner)
		-- ^ ‘lm’ starts no excluded identifier.
	| NLowerVaridL       annotation lexicalNLower                    (list varidInner)
		-- ^ ‘ln’ starts no excluded identifier.
	| OLowerVaridL       annotation lexicalOLower                    (list varidInner)
		-- ^ ‘lo’ starts no excluded identifier.
	| PLowerVaridL       annotation lexicalPLower                    (list varidInner)
		-- ^ ‘lp’ starts no excluded identifier.
	| QLowerVaridL       annotation lexicalQLower                    (list varidInner)
		-- ^ ‘lq’ starts no excluded identifier.
	| RLowerVaridL       annotation lexicalRLower                    (list varidInner)
		-- ^ ‘lr’ starts no excluded identifier.
	| SLowerVaridL       annotation lexicalSLower                    (list varidInner)
		-- ^ ‘ls’ starts no excluded identifier.
	| TLowerVaridL       annotation lexicalTLower                    (list varidInner)
		-- ^ ‘lt’ starts no excluded identifier.
	| ULowerVaridL       annotation lexicalULower                    (list varidInner)
		-- ^ ‘lu’ starts no excluded identifier.
	| VLowerVaridL       annotation lexicalVLower                    (list varidInner)
		-- ^ ‘lv’ starts no excluded identifier.
	| WLowerVaridL       annotation lexicalWLower                    (list varidInner)
		-- ^ ‘lw’ starts no excluded identifier.
	| XLowerVaridL       annotation lexicalXLower                    (list varidInner)
		-- ^ ‘lx’ starts no excluded identifier.
	| YLowerVaridL       annotation lexicalYLower                    (list varidInner)
		-- ^ ‘ly’ starts no excluded identifier.
	| ZLowerVaridL       annotation lexicalZLower                    (list varidInner)
		-- ^ ‘lz’ starts no excluded identifier.

-- | A non-symbolic variable (lowercase-style) identifier name, excluding
-- reserved names, after an ‘m’ was parsed from the beginning.
data VaridMBase list lexicalEndOfParse varidInnerSansAscSmallUnderscore varidInner lexicalALower lexicalBLower lexicalCLower lexicalDLower lexicalELower lexicalFLower lexicalGLower lexicalHLower lexicalILower lexicalJLower lexicalKLower lexicalLLower lexicalMLower lexicalNLower lexicalOLower varidMo lexicalPLower lexicalQLower lexicalRLower lexicalSLower lexicalTLower lexicalULower lexicalVLower lexicalWLower lexicalXLower lexicalYLower lexicalZLower annotation fixpoint =
	  EOPVaridM          annotation lexicalEndOfParse
		-- ^ We're provided with a certification that no more characters will
		-- follow in whatever we're parsing, and ‘m’ is not excluded.
	| InnerSansAscVaridM annotation varidInnerSansAscSmallUnderscore (list varidInner)
		-- ^ Characters we know aren't excluded are valid characters.
	| ALowerVaridM       annotation lexicalALower                    (list varidInner)
		-- ^ ‘ma’ starts no excluded identifier.
	| BLowerVaridM       annotation lexicalBLower                    (list varidInner)
		-- ^ ‘mb’ starts no excluded identifier.
	| CLowerVaridM       annotation lexicalCLower                    (list varidInner)
		-- ^ ‘mc’ starts no excluded identifier.
	| DLowerVaridM       annotation lexicalDLower                    (list varidInner)
		-- ^ ‘md’ starts no excluded identifier.
	| ELowerVaridM       annotation lexicalELower                    (list varidInner)
		-- ^ ‘me’ starts no excluded identifier.
	| FLowerVaridM       annotation lexicalFLower                    (list varidInner)
		-- ^ ‘mf’ starts no excluded identifier.
	| GLowerVaridM       annotation lexicalGLower                    (list varidInner)
		-- ^ ‘mg’ starts no excluded identifier.
	| HLowerVaridM       annotation lexicalHLower                    (list varidInner)
		-- ^ ‘mh’ starts no excluded identifier.
	| ILowerVaridM       annotation lexicalILower                    (list varidInner)
		-- ^ ‘mi’ starts no excluded identifier.
	| JLowerVaridM       annotation lexicalJLower                    (list varidInner)
		-- ^ ‘mj’ starts no excluded identifier.
	| KLowerVaridM       annotation lexicalKLower                    (list varidInner)
		-- ^ ‘mk’ starts no excluded identifier.
	| LLowerVaridM       annotation lexicalLLower                    (list varidInner)
		-- ^ ‘ml’ starts no excluded identifier.
	| MLowerVaridM       annotation lexicalMLower                    (list varidInner)
		-- ^ ‘mm’ starts no excluded identifier.
	| NLowerVaridM       annotation lexicalNLower                    (list varidInner)
		-- ^ ‘mn’ starts no excluded identifier.
	| OLowerVaridM       annotation lexicalOLower                    varidMo
		-- ^ ‘mo’ starts 1 excluded identifier.
	| PLowerVaridM       annotation lexicalPLower                    (list varidInner)
		-- ^ ‘mp’ starts no excluded identifier.
	| QLowerVaridM       annotation lexicalQLower                    (list varidInner)
		-- ^ ‘mq’ starts no excluded identifier.
	| RLowerVaridM       annotation lexicalRLower                    (list varidInner)
		-- ^ ‘mr’ starts no excluded identifier.
	| SLowerVaridM       annotation lexicalSLower                    (list varidInner)
		-- ^ ‘ms’ starts no excluded identifier.
	| TLowerVaridM       annotation lexicalTLower                    (list varidInner)
		-- ^ ‘mt’ starts no excluded identifier.
	| ULowerVaridM       annotation lexicalULower                    (list varidInner)
		-- ^ ‘mu’ starts no excluded identifier.
	| VLowerVaridM       annotation lexicalVLower                    (list varidInner)
		-- ^ ‘mv’ starts no excluded identifier.
	| WLowerVaridM       annotation lexicalWLower                    (list varidInner)
		-- ^ ‘mw’ starts no excluded identifier.
	| XLowerVaridM       annotation lexicalXLower                    (list varidInner)
		-- ^ ‘mx’ starts no excluded identifier.
	| YLowerVaridM       annotation lexicalYLower                    (list varidInner)
		-- ^ ‘my’ starts no excluded identifier.
	| ZLowerVaridM       annotation lexicalZLower                    (list varidInner)
		-- ^ ‘mz’ starts no excluded identifier.

-- | A non-symbolic variable (lowercase-style) identifier name, excluding
-- reserved names, after an ‘n’ was parsed from the beginning.
data VaridNBase list lexicalEndOfParse varidInnerSansAscSmallUnderscore varidInner lexicalALower lexicalBLower lexicalCLower lexicalDLower lexicalELower varidNe lexicalFLower lexicalGLower lexicalHLower lexicalILower lexicalJLower lexicalKLower lexicalLLower lexicalMLower lexicalNLower lexicalOLower lexicalPLower lexicalQLower lexicalRLower lexicalSLower lexicalTLower lexicalULower lexicalVLower lexicalWLower lexicalXLower lexicalYLower lexicalZLower annotation fixpoint =
	  EOPVaridN          annotation lexicalEndOfParse
		-- ^ We're provided with a certification that no more characters will
		-- follow in whatever we're parsing, and ‘n’ is not excluded.
	| InnerSansAscVaridN annotation varidInnerSansAscSmallUnderscore (list varidInner)
		-- ^ Characters we know aren't excluded are valid characters.
	| ALowerVaridN       annotation lexicalALower                    (list varidInner)
		-- ^ ‘na’ starts no excluded identifier.
	| BLowerVaridN       annotation lexicalBLower                    (list varidInner)
		-- ^ ‘nb’ starts no excluded identifier.
	| CLowerVaridN       annotation lexicalCLower                    (list varidInner)
		-- ^ ‘nc’ starts no excluded identifier.
	| DLowerVaridN       annotation lexicalDLower                    (list varidInner)
		-- ^ ‘nd’ starts no excluded identifier.
	| ELowerVaridN       annotation lexicalELower                    varidNe
		-- ^ ‘ne’ starts 1 excluded identifier.
	| FLowerVaridN       annotation lexicalFLower                    (list varidInner)
		-- ^ ‘nf’ starts no excluded identifier.
	| GLowerVaridN       annotation lexicalGLower                    (list varidInner)
		-- ^ ‘ng’ starts no excluded identifier.
	| HLowerVaridN       annotation lexicalHLower                    (list varidInner)
		-- ^ ‘nh’ starts no excluded identifier.
	| ILowerVaridN       annotation lexicalILower                    (list varidInner)
		-- ^ ‘ni’ starts no excluded identifier.
	| JLowerVaridN       annotation lexicalJLower                    (list varidInner)
		-- ^ ‘nj’ starts no excluded identifier.
	| KLowerVaridN       annotation lexicalKLower                    (list varidInner)
		-- ^ ‘nk’ starts no excluded identifier.
	| LLowerVaridN       annotation lexicalLLower                    (list varidInner)
		-- ^ ‘nl’ starts no excluded identifier.
	| MLowerVaridN       annotation lexicalMLower                    (list varidInner)
		-- ^ ‘nm’ starts no excluded identifier.
	| NLowerVaridN       annotation lexicalNLower                    (list varidInner)
		-- ^ ‘nn’ starts no excluded identifier.
	| OLowerVaridN       annotation lexicalOLower                    (list varidInner)
		-- ^ ‘no’ starts no excluded identifier.
	| PLowerVaridN       annotation lexicalPLower                    (list varidInner)
		-- ^ ‘np’ starts no excluded identifier.
	| QLowerVaridN       annotation lexicalQLower                    (list varidInner)
		-- ^ ‘nq’ starts no excluded identifier.
	| RLowerVaridN       annotation lexicalRLower                    (list varidInner)
		-- ^ ‘nr’ starts no excluded identifier.
	| SLowerVaridN       annotation lexicalSLower                    (list varidInner)
		-- ^ ‘ns’ starts no excluded identifier.
	| TLowerVaridN       annotation lexicalTLower                    (list varidInner)
		-- ^ ‘nt’ starts no excluded identifier.
	| ULowerVaridN       annotation lexicalULower                    (list varidInner)
		-- ^ ‘nu’ starts no excluded identifier.
	| VLowerVaridN       annotation lexicalVLower                    (list varidInner)
		-- ^ ‘nv’ starts no excluded identifier.
	| WLowerVaridN       annotation lexicalWLower                    (list varidInner)
		-- ^ ‘nw’ starts no excluded identifier.
	| XLowerVaridN       annotation lexicalXLower                    (list varidInner)
		-- ^ ‘nx’ starts no excluded identifier.
	| YLowerVaridN       annotation lexicalYLower                    (list varidInner)
		-- ^ ‘ny’ starts no excluded identifier.
	| ZLowerVaridN       annotation lexicalZLower                    (list varidInner)
		-- ^ ‘nz’ starts no excluded identifier.

-- | A non-symbolic variable (lowercase-style) identifier name, excluding
-- reserved names, after a ‘d’ was parsed from the beginning.
data VaridOBase list lexicalEndOfParse varidInnerSansAscSmallUnderscore varidInner lexicalALower lexicalBLower lexicalCLower lexicalDLower lexicalELower lexicalFLower lexicalGLower lexicalHLower lexicalILower lexicalJLower lexicalKLower lexicalLLower lexicalMLower lexicalNLower lexicalOLower lexicalPLower lexicalQLower lexicalRLower lexicalSLower lexicalTLower lexicalULower lexicalVLower lexicalWLower lexicalXLower lexicalYLower lexicalZLower annotation fixpoint =
	  EOPVaridO          annotation lexicalEndOfParse
		-- ^ We're provided with a certification that no more characters will
		-- follow in whatever we're parsing, and ‘o’ is not excluded.
	| InnerSansAscVaridO annotation varidInnerSansAscSmallUnderscore (list varidInner)
		-- ^ Characters we know aren't excluded are valid characters.
	| ALowerVaridO       annotation lexicalALower                    (list varidInner)
		-- ^ ‘oa’ starts no excluded identifier.
	| BLowerVaridO       annotation lexicalBLower                    (list varidInner)
		-- ^ ‘ob’ starts no excluded identifier.
	| CLowerVaridO       annotation lexicalCLower                    (list varidInner)
		-- ^ ‘oc’ starts no excluded identifier.
	| DLowerVaridO       annotation lexicalDLower                    (list varidInner)
		-- ^ ‘od’ starts no excluded identifier.
	| ELowerVaridO       annotation lexicalELower                    (list varidInner)
		-- ^ ‘oe’ starts no excluded identifier.
	| FLowerVaridO       annotation lexicalFLower                    varidInner        (list varidInner)
		-- ^ ‘of’ alone is excluded.  Additional characters is valid.
	| GLowerVaridO       annotation lexicalGLower                    (list varidInner)
		-- ^ ‘og’ starts no excluded identifier.
	| HLowerVaridO       annotation lexicalHLower                    (list varidInner)
		-- ^ ‘oh’ starts no excluded identifier.
	| ILowerVaridO       annotation lexicalILower                    (list varidInner)
		-- ^ ‘oi’ starts no excluded identifier.
	| JLowerVaridO       annotation lexicalJLower                    (list varidInner)
		-- ^ ‘oj’ starts no excluded identifier.
	| KLowerVaridO       annotation lexicalKLower                    (list varidInner)
		-- ^ ‘ok’ starts no excluded identifier.
	| LLowerVaridO       annotation lexicalLLower                    (list varidInner)
		-- ^ ‘ol’ starts no excluded identifier.
	| MLowerVaridO       annotation lexicalMLower                    (list varidInner)
		-- ^ ‘om’ starts no excluded identifier.
	| NLowerVaridO       annotation lexicalNLower                    (list varidInner)
		-- ^ ‘on’ starts no excluded identifier.
	| OLowerVaridO       annotation lexicalOLower                    (list varidInner)
		-- ^ ‘oo’ starts no excluded identifier.
	| PLowerVaridO       annotation lexicalPLower                    (list varidInner)
		-- ^ ‘op’ starts no excluded identifier.
	| QLowerVaridO       annotation lexicalQLower                    (list varidInner)
		-- ^ ‘oq’ starts no excluded identifier.
	| RLowerVaridO       annotation lexicalRLower                    (list varidInner)
		-- ^ ‘or’ starts no excluded identifier.
	| SLowerVaridO       annotation lexicalSLower                    (list varidInner)
		-- ^ ‘os’ starts no excluded identifier.
	| TLowerVaridO       annotation lexicalTLower                    (list varidInner)
		-- ^ ‘ot’ starts no excluded identifier.
	| ULowerVaridO       annotation lexicalULower                    (list varidInner)
		-- ^ ‘ou’ starts no excluded identifier.
	| VLowerVaridO       annotation lexicalVLower                    (list varidInner)
		-- ^ ‘ov’ starts no excluded identifier.
	| WLowerVaridO       annotation lexicalWLower                    (list varidInner)
		-- ^ ‘ow’ starts no excluded identifier.
	| XLowerVaridO       annotation lexicalXLower                    (list varidInner)
		-- ^ ‘ox’ starts no excluded identifier.
	| YLowerVaridO       annotation lexicalYLower                    (list varidInner)
		-- ^ ‘oy’ starts no excluded identifier.
	| ZLowerVaridO       annotation lexicalZLower                    (list varidInner)
		-- ^ ‘oz’ starts no excluded identifier.

-- | A non-symbolic variable (lowercase-style) identifier name, excluding
-- reserved names, after a ‘t’ was parsed from the beginning.
data VaridTBase list lexicalEndOfParse varidInnerSansAscSmallUnderscore varidInner lexicalALower lexicalBLower lexicalCLower lexicalDLower lexicalELower lexicalFLower lexicalGLower lexicalHLower varidTh lexicalILower lexicalJLower lexicalKLower lexicalLLower lexicalMLower lexicalNLower lexicalOLower lexicalPLower lexicalQLower lexicalRLower lexicalSLower lexicalTLower lexicalULower lexicalVLower lexicalWLower lexicalXLower lexicalYLower varidTy lexicalZLower annotation fixpoint =
	  EOPVaridT          annotation lexicalEndOfParse
		-- ^ We're provided with a certification that no more characters will
		-- follow in whatever we're parsing, and ‘t’ is not excluded.
	| InnerSansAscVaridT annotation varidInnerSansAscSmallUnderscore (list varidInner)
		-- ^ Characters we know aren't excluded are valid characters.
	| ALowerVaridT       annotation lexicalALower                    (list varidInner)
		-- ^ ‘ta’ starts no excluded identifier.
	| BLowerVaridT       annotation lexicalBLower                    (list varidInner)
		-- ^ ‘tb’ starts no excluded identifier.
	| CLowerVaridT       annotation lexicalCLower                    (list varidInner)
		-- ^ ‘tc’ starts no excluded identifier.
	| DLowerVaridT       annotation lexicalDLower                    (list varidInner)
		-- ^ ‘td’ starts no excluded identifier.
	| ELowerVaridT       annotation lexicalELower                    (list varidInner)
		-- ^ ‘te’ starts no excluded identifier.
	| FLowerVaridT       annotation lexicalFLower                    (list varidInner)
		-- ^ ‘tf’ starts no excluded identifier.
	| GLowerVaridT       annotation lexicalGLower                    (list varidInner)
		-- ^ ‘tg’ starts no excluded identifier.
	| HLowerVaridT       annotation lexicalHLower                    varidTh
		-- ^ ‘th’ starts 1 excluded identifier.
	| ILowerVaridT       annotation lexicalILower                    (list varidInner)
		-- ^ ‘ti’ starts no excluded identifier.
	| JLowerVaridT       annotation lexicalJLower                    (list varidInner)
		-- ^ ‘tj’ starts no excluded identifier.
	| KLowerVaridT       annotation lexicalKLower                    (list varidInner)
		-- ^ ‘tk’ starts no excluded identifier.
	| LLowerVaridT       annotation lexicalLLower                    (list varidInner)
		-- ^ ‘tl’ starts no excluded identifier.
	| MLowerVaridT       annotation lexicalMLower                    (list varidInner)
		-- ^ ‘tm’ starts no excluded identifier.
	| NLowerVaridT       annotation lexicalNLower                    (list varidInner)
		-- ^ ‘tn’ starts no excluded identifier.
	| OLowerVaridT       annotation lexicalOLower                    (list varidInner)
		-- ^ ‘to’ starts no excluded identifier.
	| PLowerVaridT       annotation lexicalPLower                    (list varidInner)
		-- ^ ‘tp’ starts no excluded identifier.
	| QLowerVaridT       annotation lexicalQLower                    (list varidInner)
		-- ^ ‘tq’ starts no excluded identifier.
	| RLowerVaridT       annotation lexicalRLower                    (list varidInner)
		-- ^ ‘tr’ starts no excluded identifier.
	| SLowerVaridT       annotation lexicalSLower                    (list varidInner)
		-- ^ ‘ts’ starts no excluded identifier.
	| TLowerVaridT       annotation lexicalTLower                    (list varidInner)
		-- ^ ‘tt’ starts no excluded identifier.
	| ULowerVaridT       annotation lexicalULower                    (list varidInner)
		-- ^ ‘tu’ starts no excluded identifier.
	| VLowerVaridT       annotation lexicalVLower                    (list varidInner)
		-- ^ ‘tv’ starts no excluded identifier.
	| WLowerVaridT       annotation lexicalWLower                    (list varidInner)
		-- ^ ‘tw’ starts no excluded identifier.
	| XLowerVaridT       annotation lexicalXLower                    (list varidInner)
		-- ^ ‘tx’ starts no excluded identifier.
	| YLowerVaridT       annotation lexicalYLower                    varidTy
		-- ^ ‘ty’ starts 1 excluded identifier.
	| ZLowerVaridT       annotation lexicalZLower                    (list varidInner)
		-- ^ ‘tz’ starts no excluded identifier.

-- | A non-symbolic variable (lowercase-style) identifier name, excluding
-- reserved names, after a ‘w’ was parsed from the beginning.
data VaridWBase list lexicalEndOfParse varidInnerSansAscSmallUnderscore varidInner lexicalALower lexicalBLower lexicalCLower lexicalDLower lexicalELower lexicalFLower lexicalGLower lexicalHLower varidWh lexicalILower lexicalJLower lexicalKLower lexicalLLower lexicalMLower lexicalNLower lexicalOLower lexicalPLower lexicalQLower lexicalRLower lexicalSLower lexicalTLower lexicalULower lexicalVLower lexicalWLower lexicalXLower lexicalYLower lexicalZLower annotation fixpoint =
	  EOPVaridW          annotation lexicalEndOfParse
		-- ^ We're provided with a certification that no more characters will
		-- follow in whatever we're parsing, and ‘w’ is not excluded.
	| InnerSansAscVaridW annotation varidInnerSansAscSmallUnderscore (list varidInner)
		-- ^ Characters we know aren't excluded are valid characters.
	| ALowerVaridW       annotation lexicalALower                    (list varidInner)
		-- ^ ‘wa’ starts no excluded identifier.
	| BLowerVaridW       annotation lexicalBLower                    (list varidInner)
		-- ^ ‘wb’ starts no excluded identifier.
	| CLowerVaridW       annotation lexicalCLower                    (list varidInner)
		-- ^ ‘wc’ starts no excluded identifier.
	| DLowerVaridW       annotation lexicalDLower                    (list varidInner)
		-- ^ ‘wd’ starts no excluded identifier.
	| ELowerVaridW       annotation lexicalELower                    (list varidInner)
		-- ^ ‘we’ starts no excluded identifier.
	| FLowerVaridW       annotation lexicalFLower                    (list varidInner)
		-- ^ ‘wf’ starts no excluded identifier.
	| GLowerVaridW       annotation lexicalGLower                    (list varidInner)
		-- ^ ‘wg’ starts no excluded identifier.
	| HLowerVaridW       annotation lexicalHLower                    varidWh
		-- ^ ‘wh’ starts 1 excluded identifier.
	| ILowerVaridW       annotation lexicalILower                    (list varidInner)
		-- ^ ‘wi’ starts no excluded identifier.
	| JLowerVaridW       annotation lexicalJLower                    (list varidInner)
		-- ^ ‘wj’ starts no excluded identifier.
	| KLowerVaridW       annotation lexicalKLower                    (list varidInner)
		-- ^ ‘wk’ starts no excluded identifier.
	| LLowerVaridW       annotation lexicalLLower                    (list varidInner)
		-- ^ ‘wl’ starts no excluded identifier.
	| MLowerVaridW       annotation lexicalMLower                    (list varidInner)
		-- ^ ‘wm’ starts no excluded identifier.
	| NLowerVaridW       annotation lexicalNLower                    (list varidInner)
		-- ^ ‘wn’ starts no excluded identifier.
	| OLowerVaridW       annotation lexicalOLower                    (list varidInner)
		-- ^ ‘wo’ starts no excluded identifier.
	| PLowerVaridW       annotation lexicalPLower                    (list varidInner)
		-- ^ ‘wp’ starts no excluded identifier.
	| QLowerVaridW       annotation lexicalQLower                    (list varidInner)
		-- ^ ‘wq’ starts no excluded identifier.
	| RLowerVaridW       annotation lexicalRLower                    (list varidInner)
		-- ^ ‘wr’ starts no excluded identifier.
	| SLowerVaridW       annotation lexicalSLower                    (list varidInner)
		-- ^ ‘ws’ starts no excluded identifier.
	| TLowerVaridW       annotation lexicalTLower                    (list varidInner)
		-- ^ ‘wt’ starts no excluded identifier.
	| ULowerVaridW       annotation lexicalULower                    (list varidInner)
		-- ^ ‘wu’ starts no excluded identifier.
	| VLowerVaridW       annotation lexicalVLower                    (list varidInner)
		-- ^ ‘wv’ starts no excluded identifier.
	| WLowerVaridW       annotation lexicalWLower                    (list varidInner)
		-- ^ ‘ww’ starts no excluded identifier.
	| XLowerVaridW       annotation lexicalXLower                    (list varidInner)
		-- ^ ‘wx’ starts no excluded identifier.
	| YLowerVaridW       annotation lexicalYLower                    (list varidInner)
		-- ^ ‘wy’ starts no excluded identifier.
	| ZLowerVaridW       annotation lexicalZLower                    (list varidInner)
		-- ^ ‘wz’ starts no excluded identifier.

-- | A non-symbolic variable (lowercase-style) identifier name, excluding
-- reserved names, after a ‘ca’ was parsed from the beginning.
data VaridCaBase list lexicalEndOfParse varidInnerSansAscSmallUnderscore varidInner lexicalALower lexicalBLower lexicalCLower lexicalDLower lexicalELower lexicalFLower lexicalGLower lexicalHLower lexicalILower lexicalJLower lexicalKLower lexicalLLower lexicalMLower lexicalNLower lexicalOLower lexicalPLower lexicalQLower lexicalRLower lexicalSLower varidCas lexicalTLower lexicalULower lexicalVLower lexicalWLower lexicalXLower lexicalYLower lexicalZLower annotation fixpoint =
	  EOPVaridCa          annotation lexicalEndOfParse
		-- ^ We're provided with a certification that no more characters will
		-- follow in whatever we're parsing, and ‘ca’ is not excluded.
	| InnerSansAscVaridCa annotation varidInnerSansAscSmallUnderscore (list varidInner)
		-- ^ Characters we know aren't excluded are valid characters.
	| ALowerVaridCa       annotation lexicalALower                    (list varidInner)
		-- ^ ‘caa’ starts no excluded identifier.
	| BLowerVaridCa       annotation lexicalBLower                    (list varidInner)
		-- ^ ‘cab’ starts no excluded identifier.
	| CLowerVaridCa       annotation lexicalCLower                    (list varidInner)
		-- ^ ‘cac’ starts no excluded identifier.
	| DLowerVaridCa       annotation lexicalDLower                    (list varidInner)
		-- ^ ‘cad’ starts no excluded identifier.
	| ELowerVaridCa       annotation lexicalELower                    (list varidInner)
		-- ^ ‘cae’ starts no excluded identifier.
	| FLowerVaridCa       annotation lexicalFLower                    (list varidInner)
		-- ^ ‘caf’ starts no excluded identifier.
	| GLowerVaridCa       annotation lexicalGLower                    (list varidInner)
		-- ^ ‘cag’ starts no excluded identifier.
	| HLowerVaridCa       annotation lexicalHLower                    (list varidInner)
		-- ^ ‘cah’ starts no excluded identifier.
	| ILowerVaridCa       annotation lexicalILower                    (list varidInner)
		-- ^ ‘cai’ starts no excluded identifier.
	| JLowerVaridCa       annotation lexicalJLower                    (list varidInner)
		-- ^ ‘caj’ starts no excluded identifier.
	| KLowerVaridCa       annotation lexicalKLower                    (list varidInner)
		-- ^ ‘cak’ starts no excluded identifier.
	| LLowerVaridCa       annotation lexicalLLower                    (list varidInner)
		-- ^ ‘cal’ starts no excluded identifier.
	| MLowerVaridCa       annotation lexicalMLower                    (list varidInner)
		-- ^ ‘cam’ starts no excluded identifier.
	| NLowerVaridCa       annotation lexicalNLower                    (list varidInner)
		-- ^ ‘can’ starts no excluded identifier.
	| OLowerVaridCa       annotation lexicalOLower                    (list varidInner)
		-- ^ ‘cao’ starts no excluded identifier.
	| PLowerVaridCa       annotation lexicalPLower                    (list varidInner)
		-- ^ ‘cap’ starts no excluded identifier.
	| QLowerVaridCa       annotation lexicalQLower                    (list varidInner)
		-- ^ ‘caq’ starts no excluded identifier.
	| RLowerVaridCa       annotation lexicalRLower                    (list varidInner)
		-- ^ ‘car’ starts no excluded identifier.
	| SLowerVaridCa       annotation lexicalSLower                    varidCas
		-- ^ ‘cas’ starts 1 excluded identifier.
	| TLowerVaridCa       annotation lexicalTLower                    (list varidInner)
		-- ^ ‘cat’ starts no excluded identifier.
	| ULowerVaridCa       annotation lexicalULower                    (list varidInner)
		-- ^ ‘cau’ starts no excluded identifier.
	| VLowerVaridCa       annotation lexicalVLower                    (list varidInner)
		-- ^ ‘cav’ starts no excluded identifier.
	| WLowerVaridCa       annotation lexicalWLower                    (list varidInner)
		-- ^ ‘caw’ starts no excluded identifier.
	| XLowerVaridCa       annotation lexicalXLower                    (list varidInner)
		-- ^ ‘cax’ starts no excluded identifier.
	| YLowerVaridCa       annotation lexicalYLower                    (list varidInner)
		-- ^ ‘cay’ starts no excluded identifier.
	| ZLowerVaridCa       annotation lexicalZLower                    (list varidInner)
		-- ^ ‘caz’ starts no excluded identifier.

-- | A non-symbolic variable (lowercase-style) identifier name, excluding
-- reserved names, after a ‘cl’ was parsed from the beginning.
data VaridClBase list lexicalEndOfParse varidInnerSansAscSmallUnderscore varidInner lexicalALower varidCla lexicalBLower lexicalCLower lexicalDLower lexicalELower lexicalFLower lexicalGLower lexicalHLower lexicalILower lexicalJLower lexicalKLower lexicalLLower lexicalMLower lexicalNLower lexicalOLower lexicalPLower lexicalQLower lexicalRLower lexicalSLower lexicalTLower lexicalULower lexicalVLower lexicalWLower lexicalXLower lexicalYLower lexicalZLower annotation fixpoint =
	  EOPVaridCl          annotation lexicalEndOfParse
		-- ^ We're provided with a certification that no more characters will
		-- follow in whatever we're parsing, and ‘cl’ is not excluded.
	| InnerSansAscVaridCl annotation varidInnerSansAscSmallUnderscore (list varidInner)
		-- ^ Characters we know aren't excluded are valid characters.
	| ALowerVaridCl       annotation lexicalALower                    varidCla
		-- ^ ‘cla’ starts 1 excluded identifier.
	| BLowerVaridCl       annotation lexicalBLower                    (list varidInner)
		-- ^ ‘clb’ starts no excluded identifier.
	| CLowerVaridCl       annotation lexicalCLower                    (list varidInner)
		-- ^ ‘clc’ starts no excluded identifier.
	| DLowerVaridCl       annotation lexicalDLower                    (list varidInner)
		-- ^ ‘cld’ starts no excluded identifier.
	| ELowerVaridCl       annotation lexicalELower                    (list varidInner)
		-- ^ ‘cle’ starts no excluded identifier.
	| FLowerVaridCl       annotation lexicalFLower                    (list varidInner)
		-- ^ ‘clf’ starts no excluded identifier.
	| GLowerVaridCl       annotation lexicalGLower                    (list varidInner)
		-- ^ ‘clg’ starts no excluded identifier.
	| HLowerVaridCl       annotation lexicalHLower                    (list varidInner)
		-- ^ ‘clh’ starts no excluded identifier.
	| ILowerVaridCl       annotation lexicalILower                    (list varidInner)
		-- ^ ‘cli’ starts no excluded identifier.
	| JLowerVaridCl       annotation lexicalJLower                    (list varidInner)
		-- ^ ‘clj’ starts no excluded identifier.
	| KLowerVaridCl       annotation lexicalKLower                    (list varidInner)
		-- ^ ‘clk’ starts no excluded identifier.
	| LLowerVaridCl       annotation lexicalLLower                    (list varidInner)
		-- ^ ‘cll’ starts no excluded identifier.
	| MLowerVaridCl       annotation lexicalMLower                    (list varidInner)
		-- ^ ‘clm’ starts no excluded identifier.
	| NLowerVaridCl       annotation lexicalNLower                    (list varidInner)
		-- ^ ‘cln’ starts no excluded identifier.
	| OLowerVaridCl       annotation lexicalOLower                    (list varidInner)
		-- ^ ‘clo’ starts no excluded identifier.
	| PLowerVaridCl       annotation lexicalPLower                    (list varidInner)
		-- ^ ‘clp’ starts no excluded identifier.
	| QLowerVaridCl       annotation lexicalQLower                    (list varidInner)
		-- ^ ‘clq’ starts no excluded identifier.
	| RLowerVaridCl       annotation lexicalRLower                    (list varidInner)
		-- ^ ‘clr’ starts no excluded identifier.
	| SLowerVaridCl       annotation lexicalSLower                    (list varidInner)
		-- ^ ‘cls’ starts no excluded identifier.
	| TLowerVaridCl       annotation lexicalTLower                    (list varidInner)
		-- ^ ‘clt’ starts no excluded identifier.
	| ULowerVaridCl       annotation lexicalULower                    (list varidInner)
		-- ^ ‘clu’ starts no excluded identifier.
	| VLowerVaridCl       annotation lexicalVLower                    (list varidInner)
		-- ^ ‘clv’ starts no excluded identifier.
	| WLowerVaridCl       annotation lexicalWLower                    (list varidInner)
		-- ^ ‘clw’ starts no excluded identifier.
	| XLowerVaridCl       annotation lexicalXLower                    (list varidInner)
		-- ^ ‘clx’ starts no excluded identifier.
	| YLowerVaridCl       annotation lexicalYLower                    (list varidInner)
		-- ^ ‘cly’ starts no excluded identifier.
	| ZLowerVaridCl       annotation lexicalZLower                    (list varidInner)
		-- ^ ‘clz’ starts no excluded identifier.

-- | A non-symbolic variable (lowercase-style) identifier name, excluding
-- reserved names, after a ‘da’ was parsed from the beginning.
data VaridDaBase list lexicalEndOfParse varidInnerSansAscSmallUnderscore varidInner lexicalALower lexicalBLower lexicalCLower lexicalDLower lexicalELower lexicalFLower lexicalGLower lexicalHLower lexicalILower lexicalJLower lexicalKLower lexicalLLower lexicalMLower lexicalNLower lexicalOLower lexicalPLower lexicalQLower lexicalRLower lexicalSLower lexicalTLower varidDat lexicalULower lexicalVLower lexicalWLower lexicalXLower lexicalYLower lexicalZLower annotation fixpoint =
	  EOPVaridDa          annotation lexicalEndOfParse
		-- ^ We're provided with a certification that no more characters will
		-- follow in whatever we're parsing, and ‘da’ is not excluded.
	| InnerSansAscVaridDa annotation varidInnerSansAscSmallUnderscore (list varidInner)
		-- ^ Characters we know aren't excluded are valid characters.
	| ALowerVaridDa       annotation lexicalALower                    (list varidInner)
		-- ^ ‘daa’ starts no excluded identifier.
	| BLowerVaridDa       annotation lexicalBLower                    (list varidInner)
		-- ^ ‘dab’ starts no excluded identifier.
	| CLowerVaridDa       annotation lexicalCLower                    (list varidInner)
		-- ^ ‘dac’ starts no excluded identifier.
	| DLowerVaridDa       annotation lexicalDLower                    (list varidInner)
		-- ^ ‘dad’ starts no excluded identifier.
	| ELowerVaridDa       annotation lexicalELower                    (list varidInner)
		-- ^ ‘dae’ starts no excluded identifier.
	| FLowerVaridDa       annotation lexicalFLower                    (list varidInner)
		-- ^ ‘daf’ starts no excluded identifier.
	| GLowerVaridDa       annotation lexicalGLower                    (list varidInner)
		-- ^ ‘dag’ starts no excluded identifier.
	| HLowerVaridDa       annotation lexicalHLower                    (list varidInner)
		-- ^ ‘dah’ starts no excluded identifier.
	| ILowerVaridDa       annotation lexicalILower                    (list varidInner)
		-- ^ ‘dai’ starts no excluded identifier.
	| JLowerVaridDa       annotation lexicalJLower                    (list varidInner)
		-- ^ ‘daj’ starts no excluded identifier.
	| KLowerVaridDa       annotation lexicalKLower                    (list varidInner)
		-- ^ ‘dak’ starts no excluded identifier.
	| LLowerVaridDa       annotation lexicalLLower                    (list varidInner)
		-- ^ ‘dal’ starts no excluded identifier.
	| MLowerVaridDa       annotation lexicalMLower                    (list varidInner)
		-- ^ ‘dam’ starts no excluded identifier.
	| NLowerVaridDa       annotation lexicalNLower                    (list varidInner)
		-- ^ ‘dan’ starts no excluded identifier.
	| OLowerVaridDa       annotation lexicalOLower                    (list varidInner)
		-- ^ ‘dao’ starts no excluded identifier.
	| PLowerVaridDa       annotation lexicalPLower                    (list varidInner)
		-- ^ ‘dap’ starts no excluded identifier.
	| QLowerVaridDa       annotation lexicalQLower                    (list varidInner)
		-- ^ ‘daq’ starts no excluded identifier.
	| RLowerVaridDa       annotation lexicalRLower                    (list varidInner)
		-- ^ ‘dar’ starts no excluded identifier.
	| SLowerVaridDa       annotation lexicalSLower                    (list varidInner)
		-- ^ ‘das’ starts no excluded identifier.
	| TLowerVaridDa       annotation lexicalTLower                    varidDat
		-- ^ ‘dat’ starts 1 excluded identifier.
	| ULowerVaridDa       annotation lexicalULower                    (list varidInner)
		-- ^ ‘dau’ starts no excluded identifier.
	| VLowerVaridDa       annotation lexicalVLower                    (list varidInner)
		-- ^ ‘dav’ starts no excluded identifier.
	| WLowerVaridDa       annotation lexicalWLower                    (list varidInner)
		-- ^ ‘daw’ starts no excluded identifier.
	| XLowerVaridDa       annotation lexicalXLower                    (list varidInner)
		-- ^ ‘dax’ starts no excluded identifier.
	| YLowerVaridDa       annotation lexicalYLower                    (list varidInner)
		-- ^ ‘day’ starts no excluded identifier.
	| ZLowerVaridDa       annotation lexicalZLower                    (list varidInner)
		-- ^ ‘daz’ starts no excluded identifier.

-- | A non-symbolic variable (lowercase-style) identifier name, excluding
-- reserved names, after a ‘de’ was parsed from the beginning.
data VaridDeBase list lexicalEndOfParse varidInnerSansAscSmallUnderscore varidInner lexicalALower lexicalBLower lexicalCLower lexicalDLower lexicalELower lexicalFLower varidDef lexicalGLower lexicalHLower lexicalILower lexicalJLower lexicalKLower lexicalLLower lexicalMLower lexicalNLower lexicalOLower lexicalPLower lexicalQLower lexicalRLower varidDer lexicalSLower lexicalTLower lexicalULower lexicalVLower lexicalWLower lexicalXLower lexicalYLower lexicalZLower annotation fixpoint =
	  EOPVaridDe          annotation lexicalEndOfParse
		-- ^ We're provided with a certification that no more characters will
		-- follow in whatever we're parsing, and ‘de’ is not excluded.
	| InnerSansAscVaridDe annotation varidInnerSansAscSmallUnderscore (list varidInner)
		-- ^ Characters we know aren't excluded are valid characters.
	| ALowerVaridDe       annotation lexicalALower                    (list varidInner)
		-- ^ ‘dea’ starts no excluded identifier.
	| BLowerVaridDe       annotation lexicalBLower                    (list varidInner)
		-- ^ ‘deb’ starts no excluded identifier.
	| CLowerVaridDe       annotation lexicalCLower                    (list varidInner)
		-- ^ ‘dec’ starts no excluded identifier.
	| DLowerVaridDe       annotation lexicalDLower                    (list varidInner)
		-- ^ ‘ded’ starts no excluded identifier.
	| ELowerVaridDe       annotation lexicalELower                    (list varidInner)
		-- ^ ‘dee’ starts no excluded identifier.
	| FLowerVaridDe       annotation lexicalFLower                    varidDef
		-- ^ ‘def’ starts no excluded identifier.
	| GLowerVaridDe       annotation lexicalGLower                    (list varidInner)
		-- ^ ‘deg’ starts no excluded identifier.
	| HLowerVaridDe       annotation lexicalHLower                    (list varidInner)
		-- ^ ‘deh’ starts no excluded identifier.
	| ILowerVaridDe       annotation lexicalILower                    (list varidInner)
		-- ^ ‘dei’ starts no excluded identifier.
	| JLowerVaridDe       annotation lexicalJLower                    (list varidInner)
		-- ^ ‘dej’ starts no excluded identifier.
	| KLowerVaridDe       annotation lexicalKLower                    (list varidInner)
		-- ^ ‘dek’ starts no excluded identifier.
	| LLowerVaridDe       annotation lexicalLLower                    (list varidInner)
		-- ^ ‘del’ starts no excluded identifier.
	| MLowerVaridDe       annotation lexicalMLower                    (list varidInner)
		-- ^ ‘dem’ starts no excluded identifier.
	| NLowerVaridDe       annotation lexicalNLower                    (list varidInner)
		-- ^ ‘den’ starts no excluded identifier.
	| OLowerVaridDe       annotation lexicalOLower                    (list varidInner)
		-- ^ ‘deo’ starts no excluded identifier.
	| PLowerVaridDe       annotation lexicalPLower                    (list varidInner)
		-- ^ ‘dep’ starts no excluded identifier.
	| QLowerVaridDe       annotation lexicalQLower                    (list varidInner)
		-- ^ ‘deq’ starts no excluded identifier.
	| RLowerVaridDe       annotation lexicalRLower                    varidDer
		-- ^ ‘der’ starts 1 excluded identifier.
	| SLowerVaridDe       annotation lexicalSLower                    (list varidInner)
		-- ^ ‘des’ starts no excluded identifier.
	| TLowerVaridDe       annotation lexicalTLower                    (list varidInner)
		-- ^ ‘det’ starts no excluded identifier.
	| ULowerVaridDe       annotation lexicalULower                    (list varidInner)
		-- ^ ‘deu’ starts no excluded identifier.
	| VLowerVaridDe       annotation lexicalVLower                    (list varidInner)
		-- ^ ‘dev’ starts no excluded identifier.
	| WLowerVaridDe       annotation lexicalWLower                    (list varidInner)
		-- ^ ‘dew’ starts no excluded identifier.
	| XLowerVaridDe       annotation lexicalXLower                    (list varidInner)
		-- ^ ‘dex’ starts no excluded identifier.
	| YLowerVaridDe       annotation lexicalYLower                    (list varidInner)
		-- ^ ‘dey’ starts no excluded identifier.
	| ZLowerVaridDe       annotation lexicalZLower                    (list varidInner)
		-- ^ ‘dez’ starts no excluded identifier.

-- | A non-symbolic variable (lowercase-style) identifier name, excluding
-- reserved names, after an ‘el’ was parsed from the beginning.
data VaridElBase list lexicalEndOfParse varidInnerSansAscSmallUnderscore varidInner lexicalALower lexicalBLower lexicalCLower lexicalDLower lexicalELower varidEls lexicalFLower lexicalGLower lexicalHLower lexicalILower lexicalJLower lexicalKLower lexicalLLower lexicalMLower lexicalNLower lexicalOLower lexicalPLower lexicalQLower lexicalRLower lexicalSLower lexicalTLower lexicalULower lexicalVLower lexicalWLower lexicalXLower lexicalYLower lexicalZLower annotation fixpoint =
	  EOPVaridEl          annotation lexicalEndOfParse
		-- ^ We're provided with a certification that no more characters will
		-- follow in whatever we're parsing, and ‘el’ is not excluded.
	| InnerSansAscVaridEl annotation varidInnerSansAscSmallUnderscore (list varidInner)
		-- ^ Characters we know aren't excluded are valid characters.
	| ALowerVaridEl       annotation lexicalALower                    (list varidInner)
		-- ^ ‘ela’ starts no excluded identifier.
	| BLowerVaridEl       annotation lexicalBLower                    (list varidInner)
		-- ^ ‘elb’ starts no excluded identifier.
	| CLowerVaridEl       annotation lexicalCLower                    (list varidInner)
		-- ^ ‘elc’ starts no excluded identifier.
	| DLowerVaridEl       annotation lexicalDLower                    (list varidInner)
		-- ^ ‘eld’ starts no excluded identifier.
	| ELowerVaridEl       annotation lexicalELower                    (list varidInner)
		-- ^ ‘ele’ starts no excluded identifier.
	| FLowerVaridEl       annotation lexicalFLower                    (list varidInner)
		-- ^ ‘elf’ starts no excluded identifier.
	| GLowerVaridEl       annotation lexicalGLower                    (list varidInner)
		-- ^ ‘elg’ starts no excluded identifier.
	| HLowerVaridEl       annotation lexicalHLower                    (list varidInner)
		-- ^ ‘elh’ starts no excluded identifier.
	| ILowerVaridEl       annotation lexicalILower                    (list varidInner)
		-- ^ ‘eli’ starts no excluded identifier.
	| JLowerVaridEl       annotation lexicalJLower                    (list varidInner)
		-- ^ ‘elj’ starts no excluded identifier.
	| KLowerVaridEl       annotation lexicalKLower                    (list varidInner)
		-- ^ ‘elk’ starts no excluded identifier.
	| LLowerVaridEl       annotation lexicalLLower                    (list varidInner)
		-- ^ ‘ell’ starts no excluded identifier.
	| MLowerVaridEl       annotation lexicalMLower                    (list varidInner)
		-- ^ ‘elm’ starts no excluded identifier.
	| NLowerVaridEl       annotation lexicalNLower                    (list varidInner)
		-- ^ ‘eln’ starts no excluded identifier.
	| OLowerVaridEl       annotation lexicalOLower                    (list varidInner)
		-- ^ ‘elo’ starts no excluded identifier.
	| PLowerVaridEl       annotation lexicalPLower                    (list varidInner)
		-- ^ ‘elp’ starts no excluded identifier.
	| QLowerVaridEl       annotation lexicalQLower                    (list varidInner)
		-- ^ ‘elq’ starts no excluded identifier.
	| RLowerVaridEl       annotation lexicalRLower                    (list varidInner)
		-- ^ ‘elr’ starts no excluded identifier.
	| SLowerVaridEl       annotation lexicalSLower                    varidEls
		-- ^ ‘els’ starts 1 excluded identifier.
	| TLowerVaridEl       annotation lexicalTLower                    (list varidInner)
		-- ^ ‘elt’ starts no excluded identifier.
	| ULowerVaridEl       annotation lexicalULower                    (list varidInner)
		-- ^ ‘elu’ starts no excluded identifier.
	| VLowerVaridEl       annotation lexicalVLower                    (list varidInner)
		-- ^ ‘elv’ starts no excluded identifier.
	| WLowerVaridEl       annotation lexicalWLower                    (list varidInner)
		-- ^ ‘elw’ starts no excluded identifier.
	| XLowerVaridEl       annotation lexicalXLower                    (list varidInner)
		-- ^ ‘elx’ starts no excluded identifier.
	| YLowerVaridEl       annotation lexicalYLower                    (list varidInner)
		-- ^ ‘ely’ starts no excluded identifier.
	| ZLowerVaridEl       annotation lexicalZLower                    (list varidInner)
		-- ^ ‘elz’ starts no excluded identifier.

-- | A non-symbolic variable (lowercase-style) identifier name, excluding
-- reserved names, after an ‘fo’ was parsed from the beginning.
data VaridFoBase list lexicalEndOfParse varidInnerSansAscSmallUnderscore varidInner lexicalALower lexicalBLower lexicalCLower lexicalDLower lexicalELower lexicalFLower lexicalGLower lexicalHLower lexicalILower lexicalJLower lexicalKLower lexicalLLower lexicalMLower lexicalNLower lexicalOLower lexicalPLower lexicalQLower lexicalRLower varidFor lexicalSLower lexicalTLower lexicalULower lexicalVLower lexicalWLower lexicalXLower lexicalYLower lexicalZLower annotation fixpoint =
	  EOPVaridFo          annotation lexicalEndOfParse
		-- ^ We're provided with a certification that no more characters will
		-- follow in whatever we're parsing, and ‘fo’ is not excluded.
	| InnerSansAscVaridFo annotation varidInnerSansAscSmallUnderscore (list varidInner)
		-- ^ Characters we know aren't excluded are valid characters.
	| ALowerVaridFo       annotation lexicalALower                    (list varidInner)
		-- ^ ‘foa’ starts no excluded identifier.
	| BLowerVaridFo       annotation lexicalBLower                    (list varidInner)
		-- ^ ‘fob’ starts no excluded identifier.
	| CLowerVaridFo       annotation lexicalCLower                    (list varidInner)
		-- ^ ‘foc’ starts no excluded identifier.
	| DLowerVaridFo       annotation lexicalDLower                    (list varidInner)
		-- ^ ‘fod’ starts no excluded identifier.
	| ELowerVaridFo       annotation lexicalELower                    (list varidInner)
		-- ^ ‘foe’ starts no excluded identifier.
	| FLowerVaridFo       annotation lexicalFLower                    (list varidInner)
		-- ^ ‘fof’ starts no excluded identifier.
	| GLowerVaridFo       annotation lexicalGLower                    (list varidInner)
		-- ^ ‘fog’ starts no excluded identifier.
	| HLowerVaridFo       annotation lexicalHLower                    (list varidInner)
		-- ^ ‘foh’ starts no excluded identifier.
	| ILowerVaridFo       annotation lexicalILower                    (list varidInner)
		-- ^ ‘foi’ starts no excluded identifier.
	| JLowerVaridFo       annotation lexicalJLower                    (list varidInner)
		-- ^ ‘foj’ starts no excluded identifier.
	| KLowerVaridFo       annotation lexicalKLower                    (list varidInner)
		-- ^ ‘fok’ starts no excluded identifier.
	| LLowerVaridFo       annotation lexicalLLower                    (list varidInner)
		-- ^ ‘fol’ starts no excluded identifier.
	| MLowerVaridFo       annotation lexicalMLower                    (list varidInner)
		-- ^ ‘fom’ starts no excluded identifier.
	| NLowerVaridFo       annotation lexicalNLower                    (list varidInner)
		-- ^ ‘fon’ starts no excluded identifier.
	| OLowerVaridFo       annotation lexicalOLower                    (list varidInner)
		-- ^ ‘foo’ starts no excluded identifier.
	| PLowerVaridFo       annotation lexicalPLower                    (list varidInner)
		-- ^ ‘fop’ starts no excluded identifier.
	| QLowerVaridFo       annotation lexicalQLower                    (list varidInner)
		-- ^ ‘foq’ starts no excluded identifier.
	| RLowerVaridFo       annotation lexicalRLower                    varidFor
		-- ^ ‘for’ starts 1 excluded identifier.
	| SLowerVaridFo       annotation lexicalSLower                    (list varidInner)
		-- ^ ‘fos’ starts no excluded identifier.
	| TLowerVaridFo       annotation lexicalTLower                    (list varidInner)
		-- ^ ‘fot’ starts no excluded identifier.
	| ULowerVaridFo       annotation lexicalULower                    (list varidInner)
		-- ^ ‘fou’ starts no excluded identifier.
	| VLowerVaridFo       annotation lexicalVLower                    (list varidInner)
		-- ^ ‘fov’ starts no excluded identifier.
	| WLowerVaridFo       annotation lexicalWLower                    (list varidInner)
		-- ^ ‘fow’ starts no excluded identifier.
	| XLowerVaridFo       annotation lexicalXLower                    (list varidInner)
		-- ^ ‘fox’ starts no excluded identifier.
	| YLowerVaridFo       annotation lexicalYLower                    (list varidInner)
		-- ^ ‘foy’ starts no excluded identifier.
	| ZLowerVaridFo       annotation lexicalZLower                    (list varidInner)
		-- ^ ‘foz’ starts no excluded identifier.

-- | A non-symbolic variable (lowercase-style) identifier name, excluding
-- reserved names, after an ‘im’ was parsed from the beginning.
data VaridImBase list lexicalEndOfParse varidInnerSansAscSmallUnderscore varidInner lexicalALower lexicalBLower lexicalCLower lexicalDLower lexicalELower lexicalFLower lexicalGLower lexicalHLower lexicalILower varidImp lexicalJLower lexicalKLower lexicalLLower lexicalMLower lexicalNLower lexicalOLower lexicalPLower lexicalQLower lexicalRLower lexicalSLower lexicalTLower lexicalULower lexicalVLower lexicalWLower lexicalXLower lexicalYLower lexicalZLower annotation fixpoint =
	  EOPVaridIm          annotation lexicalEndOfParse
		-- ^ We're provided with a certification that no more characters will
		-- follow in whatever we're parsing, and ‘im’ is not excluded.
	| InnerSansAscVaridIm annotation varidInnerSansAscSmallUnderscore (list varidInner)
		-- ^ Characters we know aren't excluded are valid characters.
	| ALowerVaridIm       annotation lexicalALower                    (list varidInner)
		-- ^ ‘ima’ starts no excluded identifier.
	| BLowerVaridIm       annotation lexicalBLower                    (list varidInner)
		-- ^ ‘imb’ starts no excluded identifier.
	| CLowerVaridIm       annotation lexicalCLower                    (list varidInner)
		-- ^ ‘imc’ starts no excluded identifier.
	| DLowerVaridIm       annotation lexicalDLower                    (list varidInner)
		-- ^ ‘imd’ starts no excluded identifier.
	| ELowerVaridIm       annotation lexicalELower                    (list varidInner)
		-- ^ ‘ime’ starts no excluded identifier.
	| FLowerVaridIm       annotation lexicalFLower                    (list varidInner)
		-- ^ ‘imf’ starts no excluded identifier.
	| GLowerVaridIm       annotation lexicalGLower                    (list varidInner)
		-- ^ ‘img’ starts no excluded identifier.
	| HLowerVaridIm       annotation lexicalHLower                    (list varidInner)
		-- ^ ‘imh’ starts no excluded identifier.
	| ILowerVaridIm       annotation lexicalILower                    (list varidInner)
		-- ^ ‘imi’ starts no excluded identifier.
	| JLowerVaridIm       annotation lexicalJLower                    (list varidInner)
		-- ^ ‘imj’ starts no excluded identifier.
	| KLowerVaridIm       annotation lexicalKLower                    (list varidInner)
		-- ^ ‘imk’ starts no excluded identifier.
	| LLowerVaridIm       annotation lexicalLLower                    (list varidInner)
		-- ^ ‘iml’ starts no excluded identifier.
	| MLowerVaridIm       annotation lexicalMLower                    (list varidInner)
		-- ^ ‘imm’ starts no excluded identifier.
	| NLowerVaridIm       annotation lexicalNLower                    (list varidInner)
		-- ^ ‘imn’ starts no excluded identifier.
	| OLowerVaridIm       annotation lexicalOLower                    (list varidInner)
		-- ^ ‘imo’ starts no excluded identifier.
	| PLowerVaridIm       annotation lexicalPLower                    varidImp
		-- ^ ‘imp’ starts 1 excluded identifier.
	| QLowerVaridIm       annotation lexicalQLower                    (list varidInner)
		-- ^ ‘fiq’ starts no excluded identifier.
	| RLowerVaridIm       annotation lexicalRLower                    (list varidInner)
		-- ^ ‘imr’ starts no excluded identifier.
	| SLowerVaridIm       annotation lexicalSLower                    (list varidInner)
		-- ^ ‘ims’ starts no excluded identifier.
	| TLowerVaridIm       annotation lexicalTLower                    (list varidInner)
		-- ^ ‘imt’ starts no excluded identifier.
	| ULowerVaridIm       annotation lexicalULower                    (list varidInner)
		-- ^ ‘imu’ starts no excluded identifier.
	| VLowerVaridIm       annotation lexicalVLower                    (list varidInner)
		-- ^ ‘imv’ starts no excluded identifier.
	| WLowerVaridIm       annotation lexicalWLower                    (list varidInner)
		-- ^ ‘imw’ starts no excluded identifier.
	| XLowerVaridIm       annotation lexicalXLower                    (list varidInner)
		-- ^ ‘imx’ starts no excluded identifier.
	| YLowerVaridIm       annotation lexicalYLower                    (list varidInner)
		-- ^ ‘imy’ starts no excluded identifier.
	| ZLowerVaridIm       annotation lexicalZLower                    (list varidInner)
		-- ^ ‘imz’ starts no excluded identifier.

-- | A non-symbolic variable (lowercase-style) identifier name, excluding
-- reserved names, after an ‘in’ was parsed from the beginning.
data VaridInBase list lexicalEndOfParse varidInnerSansAscSmallUnderscore varidInner lexicalALower lexicalBLower lexicalCLower lexicalDLower lexicalELower lexicalFLower varidInf lexicalGLower lexicalHLower lexicalILower lexicalJLower lexicalKLower lexicalLLower lexicalMLower lexicalNLower lexicalOLower lexicalPLower lexicalQLower lexicalRLower lexicalSLower varidIns lexicalTLower lexicalULower lexicalVLower lexicalWLower lexicalXLower lexicalYLower lexicalZLower annotation fixpoint =
	-- (We could include this if ‘in’ identifiers are included, but they are not:)
	{-
	  EOPVaridIn          annotation lexicalEndOfParse
		-- ^ We're provided with a certification that no more characters will
		-- follow in whatever we're parsing.
	-}
	  InnerSansAscVaridIn annotation varidInnerSansAscSmallUnderscore (list varidInner)
		-- ^ Characters we know aren't excluded are valid characters.
	| ALowerVaridIn       annotation lexicalALower                    (list varidInner)
		-- ^ ‘ina’ starts no excluded identifier.
	| BLowerVaridIn       annotation lexicalBLower                    (list varidInner)
		-- ^ ‘inb’ starts no excluded identifier.
	| CLowerVaridIn       annotation lexicalCLower                    (list varidInner)
		-- ^ ‘inc’ starts no excluded identifier.
	| DLowerVaridIn       annotation lexicalDLower                    (list varidInner)
		-- ^ ‘ind’ starts no excluded identifier.
	| ELowerVaridIn       annotation lexicalELower                    (list varidInner)
		-- ^ ‘ine’ starts no excluded identifier.
	| FLowerVaridIn       annotation lexicalFLower                    varidInf
		-- ^ ‘inf’ starts 3 excluded identifiers.
	| GLowerVaridIn       annotation lexicalGLower                    (list varidInner)
		-- ^ ‘ing’ starts no excluded identifier.
	| HLowerVaridIn       annotation lexicalHLower                    (list varidInner)
		-- ^ ‘inh’ starts no excluded identifier.
	| ILowerVaridIn       annotation lexicalILower                    (list varidInner)
		-- ^ ‘ini’ starts no excluded identifier.
	| JLowerVaridIn       annotation lexicalJLower                    (list varidInner)
		-- ^ ‘inj’ starts no excluded identifier.
	| KLowerVaridIn       annotation lexicalKLower                    (list varidInner)
		-- ^ ‘ink’ starts no excluded identifier.
	| LLowerVaridIn       annotation lexicalLLower                    (list varidInner)
		-- ^ ‘inl’ starts no excluded identifier.
	| MLowerVaridIn       annotation lexicalMLower                    (list varidInner)
		-- ^ ‘inm’ starts no excluded identifier.
	| NLowerVaridIn       annotation lexicalNLower                    (list varidInner)
		-- ^ ‘inn’ starts no excluded identifier.
	| OLowerVaridIn       annotation lexicalOLower                    (list varidInner)
		-- ^ ‘ino’ starts no excluded identifier.
	| PLowerVaridIn       annotation lexicalPLower                    (list varidInner)
		-- ^ ‘inp’ starts no excluded identifier.
	| QLowerVaridIn       annotation lexicalQLower                    (list varidInner)
		-- ^ ‘fnq’ starts no excluded identifier.
	| RLowerVaridIn       annotation lexicalRLower                    (list varidInner)
		-- ^ ‘inr’ starts no excluded identifier.
	| SLowerVaridIn       annotation lexicalSLower                    varidIns
		-- ^ ‘ins’ starts 1 excluded identifier.
	| TLowerVaridIn       annotation lexicalTLower                    (list varidInner)
		-- ^ ‘int’ starts no excluded identifier.
	| ULowerVaridIn       annotation lexicalULower                    (list varidInner)
		-- ^ ‘inu’ starts no excluded identifier.
	| VLowerVaridIn       annotation lexicalVLower                    (list varidInner)
		-- ^ ‘inv’ starts no excluded identifier.
	| WLowerVaridIn       annotation lexicalWLower                    (list varidInner)
		-- ^ ‘inw’ starts no excluded identifier.
	| XLowerVaridIn       annotation lexicalXLower                    (list varidInner)
		-- ^ ‘inx’ starts no excluded identifier.
	| YLowerVaridIn       annotation lexicalYLower                    (list varidInner)
		-- ^ ‘iny’ starts no excluded identifier.
	| ZLowerVaridIn       annotation lexicalZLower                    (list varidInner)
		-- ^ ‘inz’ starts no excluded identifier.

-- | A non-symbolic variable (lowercase-style) identifier name, excluding
-- reserved names, after an ‘le’ was parsed from the beginning.
data VaridLeBase list lexicalEndOfParse varidInnerSansAscSmallUnderscore varidInner lexicalALower lexicalBLower lexicalCLower lexicalDLower lexicalELower lexicalFLower lexicalGLower lexicalHLower lexicalILower lexicalJLower lexicalKLower lexicalLLower lexicalMLower lexicalNLower lexicalOLower lexicalPLower lexicalQLower lexicalRLower lexicalSLower lexicalTLower lexicalULower lexicalVLower lexicalWLower lexicalXLower lexicalYLower lexicalZLower annotation fixpoint =
	  EOPVaridLe          annotation lexicalEndOfParse
		-- ^ We're provided with a certification that no more characters will
		-- follow in whatever we're parsing, and ‘le’ is not excluded.
	| InnerSansAscVaridLe annotation varidInnerSansAscSmallUnderscore (list varidInner)
		-- ^ Characters we know aren't excluded are valid characters.
	| ALowerVaridLe       annotation lexicalALower                    (list varidInner)
		-- ^ ‘lea’ starts no excluded identifier.
	| BLowerVaridLe       annotation lexicalBLower                    (list varidInner)
		-- ^ ‘leb’ starts no excluded identifier.
	| CLowerVaridLe       annotation lexicalCLower                    (list varidInner)
		-- ^ ‘lec’ starts no excluded identifier.
	| DLowerVaridLe       annotation lexicalDLower                    (list varidInner)
		-- ^ ‘led’ starts no excluded identifier.
	| ELowerVaridLe       annotation lexicalELower                    (list varidInner)
		-- ^ ‘lee’ starts no excluded identifier.
	| FLowerVaridLe       annotation lexicalFLower                    (list varidInner)
		-- ^ ‘lef’ starts no excluded identifier.
	| GLowerVaridLe       annotation lexicalGLower                    (list varidInner)
		-- ^ ‘leg’ starts no excluded identifier.
	| HLowerVaridLe       annotation lexicalHLower                    (list varidInner)
		-- ^ ‘leh’ starts no excluded identifier.
	| ILowerVaridLe       annotation lexicalILower                    (list varidInner)
		-- ^ ‘lei’ starts no excluded identifier.
	| JLowerVaridLe       annotation lexicalJLower                    (list varidInner)
		-- ^ ‘lej’ starts no excluded identifier.
	| KLowerVaridLe       annotation lexicalKLower                    (list varidInner)
		-- ^ ‘lek’ starts no excluded identifier.
	| LLowerVaridLe       annotation lexicalLLower                    (list varidInner)
		-- ^ ‘lel’ starts no excluded identifier.
	| MLowerVaridLe       annotation lexicalMLower                    (list varidInner)
		-- ^ ‘lem’ starts no excluded identifier.
	| NLowerVaridLe       annotation lexicalNLower                    (list varidInner)
		-- ^ ‘len’ starts no excluded identifier.
	| OLowerVaridLe       annotation lexicalOLower                    (list varidInner)
		-- ^ ‘leo’ starts no excluded identifier.
	| PLowerVaridLe       annotation lexicalPLower                    (list varidInner)
		-- ^ ‘lep’ starts no excluded identifier.
	| QLowerVaridLe       annotation lexicalQLower                    (list varidInner)
		-- ^ ‘lem’ starts no excluded identifier.
	| RLowerVaridLe       annotation lexicalRLower                    (list varidInner)
		-- ^ ‘leq’ starts no excluded identifier.
	| SLowerVaridLe       annotation lexicalSLower                    (list varidInner)
		-- ^ ‘les’ starts no excluded identifier.
	| TLowerVaridLe       annotation lexicalTLower                    varidInner        (list varidInner)
		-- ^ ‘let’ starts no excluded identifier.
	| ULowerVaridLe       annotation lexicalULower                    (list varidInner)
		-- ^ ‘leu’ starts no excluded identifier.
	| VLowerVaridLe       annotation lexicalVLower                    (list varidInner)
		-- ^ ‘lev’ starts no excluded identifier.
	| WLowerVaridLe       annotation lexicalWLower                    (list varidInner)
		-- ^ ‘lew’ starts no excluded identifier.
	| XLowerVaridLe       annotation lexicalXLower                    (list varidInner)
		-- ^ ‘lex’ starts no excluded identifier.
	| YLowerVaridLe       annotation lexicalYLower                    (list varidInner)
		-- ^ ‘ley’ starts no excluded identifier.
	| ZLowerVaridLe       annotation lexicalZLower                    (list varidInner)
		-- ^ ‘lez’ starts no excluded identifier.

-- | A non-symbolic variable (lowercase-style) identifier name, excluding
-- reserved names, after an ‘mo’ was parsed from the beginning.
data VaridMoBase list lexicalEndOfParse varidInnerSansAscSmallUnderscore varidInner lexicalALower lexicalBLower lexicalCLower lexicalDLower varidMod lexicalELower lexicalFLower lexicalGLower lexicalHLower lexicalILower lexicalJLower lexicalKLower lexicalLLower lexicalMLower lexicalNLower lexicalOLower lexicalPLower lexicalQLower lexicalRLower lexicalSLower lexicalTLower lexicalULower lexicalVLower lexicalWLower lexicalXLower lexicalYLower lexicalZLower annotation fixpoint =
	  EOPVaridMo          annotation lexicalEndOfParse
		-- ^ We're provided with a certification that no more characters will
		-- follow in whatever we're parsing, and ‘mo’ is not excluded.
	| InnerSansAscVaridMo annotation varidInnerSansAscSmallUnderscore (list varidInner)
		-- ^ Characters we know aren't excluded are valid characters.
	| ALowerVaridMo       annotation lexicalALower                    (list varidInner)
		-- ^ ‘moa’ starts no excluded identifier.
	| BLowerVaridMo       annotation lexicalBLower                    (list varidInner)
		-- ^ ‘mob’ starts no excluded identifier.
	| CLowerVaridMo       annotation lexicalCLower                    (list varidInner)
		-- ^ ‘moc’ starts no excluded identifier.
	| DLowerVaridMo       annotation lexicalDLower                    varidMod
		-- ^ ‘mod’ starts 1 excluded identifier.
	| ELowerVaridMo       annotation lexicalELower                    (list varidInner)
		-- ^ ‘moe’ starts no excluded identifier.
	| FLowerVaridMo       annotation lexicalFLower                    (list varidInner)
		-- ^ ‘mof’ starts no excluded identifier.
	| GLowerVaridMo       annotation lexicalGLower                    (list varidInner)
		-- ^ ‘mog’ starts no excluded identifier.
	| HLowerVaridMo       annotation lexicalHLower                    (list varidInner)
		-- ^ ‘moh’ starts no excluded identifier.
	| ILowerVaridMo       annotation lexicalILower                    (list varidInner)
		-- ^ ‘moi’ starts no excluded identifier.
	| JLowerVaridMo       annotation lexicalJLower                    (list varidInner)
		-- ^ ‘moj’ starts no excluded identifier.
	| KLowerVaridMo       annotation lexicalKLower                    (list varidInner)
		-- ^ ‘mok’ starts no excluded identifier.
	| LLowerVaridMo       annotation lexicalLLower                    (list varidInner)
		-- ^ ‘mol’ starts no excluded identifier.
	| MLowerVaridMo       annotation lexicalMLower                    (list varidInner)
		-- ^ ‘mom’ starts no excluded identifier.
	| NLowerVaridMo       annotation lexicalNLower                    (list varidInner)
		-- ^ ‘mon’ starts no excluded identifier.
	| OLowerVaridMo       annotation lexicalOLower                    (list varidInner)
		-- ^ ‘moo’ starts no excluded identifier.
	| PLowerVaridMo       annotation lexicalPLower                    (list varidInner)
		-- ^ ‘mop’ starts no excluded identifier.
	| QLowerVaridMo       annotation lexicalQLower                    (list varidInner)
		-- ^ ‘moq’ starts no excluded identifier.
	| RLowerVaridMo       annotation lexicalRLower                    (list varidInner)
		-- ^ ‘mor’ starts no excluded identifier.
	| SLowerVaridMo       annotation lexicalSLower                    (list varidInner)
		-- ^ ‘mos’ starts no excluded identifier.
	| TLowerVaridMo       annotation lexicalTLower                    (list varidInner)
		-- ^ ‘mot’ starts no excluded identifier.
	| ULowerVaridMo       annotation lexicalULower                    (list varidInner)
		-- ^ ‘mou’ starts no excluded identifier.
	| VLowerVaridMo       annotation lexicalVLower                    (list varidInner)
		-- ^ ‘mov’ starts no excluded identifier.
	| WLowerVaridMo       annotation lexicalWLower                    (list varidInner)
		-- ^ ‘mow’ starts no excluded identifier.
	| XLowerVaridMo       annotation lexicalXLower                    (list varidInner)
		-- ^ ‘mox’ starts no excluded identifier.
	| YLowerVaridMo       annotation lexicalYLower                    (list varidInner)
		-- ^ ‘moy’ starts no excluded identifier.
	| ZLowerVaridMo       annotation lexicalZLower                    (list varidInner)
		-- ^ ‘moz’ starts no excluded identifier.

-- | A non-symbolic variable (lowercase-style) identifier name, excluding
-- reserved names, after an ‘ne’ was parsed from the beginning.
data VaridNeBase list lexicalEndOfParse varidInnerSansAscSmallUnderscore varidInner lexicalALower lexicalBLower lexicalCLower lexicalDLower lexicalELower lexicalFLower lexicalGLower lexicalHLower lexicalILower lexicalJLower lexicalKLower lexicalLLower lexicalMLower lexicalNLower lexicalOLower lexicalPLower lexicalQLower lexicalRLower lexicalSLower lexicalTLower lexicalULower lexicalVLower lexicalWLower varidNew lexicalXLower lexicalYLower lexicalZLower annotation fixpoint =
	  EOPVaridNe          annotation lexicalEndOfParse
		-- ^ We're provided with a certification that no more characters will
		-- follow in whatever we're parsing, and ‘ne’ is not excluded.
	| InnerSansAscVaridNe annotation varidInnerSansAscSmallUnderscore (list varidInner)
		-- ^ Characters we know aren't excluded are valid characters.
	| ALowerVaridNe       annotation lexicalALower                    (list varidInner)
		-- ^ ‘nea’ starts no excluded identifier.
	| BLowerVaridNe       annotation lexicalBLower                    (list varidInner)
		-- ^ ‘neb’ starts no excluded identifier.
	| CLowerVaridNe       annotation lexicalCLower                    (list varidInner)
		-- ^ ‘nec’ starts no excluded identifier.
	| DLowerVaridNe       annotation lexicalDLower                    (list varidInner)
		-- ^ ‘ned’ starts no excluded identifier.
	| ELowerVaridNe       annotation lexicalELower                    (list varidInner)
		-- ^ ‘nee’ starts no excluded identifier.
	| FLowerVaridNe       annotation lexicalFLower                    (list varidInner)
		-- ^ ‘nef’ starts no excluded identifier.
	| GLowerVaridNe       annotation lexicalGLower                    (list varidInner)
		-- ^ ‘neg’ starts no excluded identifier.
	| HLowerVaridNe       annotation lexicalHLower                    (list varidInner)
		-- ^ ‘neh’ starts no excluded identifier.
	| ILowerVaridNe       annotation lexicalILower                    (list varidInner)
		-- ^ ‘nei’ starts no excluded identifier.
	| JLowerVaridNe       annotation lexicalJLower                    (list varidInner)
		-- ^ ‘nej’ starts no excluded identifier.
	| KLowerVaridNe       annotation lexicalKLower                    (list varidInner)
		-- ^ ‘nek’ starts no excluded identifier.
	| LLowerVaridNe       annotation lexicalLLower                    (list varidInner)
		-- ^ ‘nel’ starts no excluded identifier.
	| MLowerVaridNe       annotation lexicalMLower                    (list varidInner)
		-- ^ ‘nem’ starts no excluded identifier.
	| NLowerVaridNe       annotation lexicalNLower                    (list varidInner)
		-- ^ ‘nen’ starts no excluded identifier.
	| OLowerVaridNe       annotation lexicalOLower                    (list varidInner)
		-- ^ ‘neo’ starts no excluded identifier.
	| PLowerVaridNe       annotation lexicalPLower                    (list varidInner)
		-- ^ ‘nep’ starts no excluded identifier.
	| QLowerVaridNe       annotation lexicalQLower                    (list varidInner)
		-- ^ ‘neq’ starts no excluded identifier.
	| RLowerVaridNe       annotation lexicalRLower                    (list varidInner)
		-- ^ ‘ner’ starts no excluded identifier.
	| SLowerVaridNe       annotation lexicalSLower                    (list varidInner)
		-- ^ ‘nes’ starts no excluded identifier.
	| TLowerVaridNe       annotation lexicalTLower                    (list varidInner)
		-- ^ ‘net’ starts no excluded identifier.
	| ULowerVaridNe       annotation lexicalULower                    (list varidInner)
		-- ^ ‘neu’ starts no excluded identifier.
	| VLowerVaridNe       annotation lexicalVLower                    (list varidInner)
		-- ^ ‘nev’ starts no excluded identifier.
	| WLowerVaridNe       annotation lexicalWLower                    varidNew
		-- ^ ‘new’ starts 1 excluded identifier.
	| XLowerVaridNe       annotation lexicalXLower                    (list varidInner)
		-- ^ ‘nex’ starts no excluded identifier.
	| YLowerVaridNe       annotation lexicalYLower                    (list varidInner)
		-- ^ ‘ney’ starts no excluded identifier.
	| ZLowerVaridNe       annotation lexicalZLower                    (list varidInner)
		-- ^ ‘nez’ starts no excluded identifier.

-- | A non-symbolic variable (lowercase-style) identifier name, excluding
-- reserved names, after an ‘th’ was parsed from the beginning.
data VaridThBase list lexicalEndOfParse varidInnerSansAscSmallUnderscore varidInner lexicalALower lexicalBLower lexicalCLower lexicalDLower lexicalELower varidThe lexicalFLower lexicalGLower lexicalHLower lexicalILower lexicalJLower lexicalKLower lexicalLLower lexicalMLower lexicalNLower lexicalOLower lexicalPLower lexicalQLower lexicalRLower lexicalSLower lexicalTLower lexicalULower lexicalVLower lexicalWLower lexicalXLower lexicalYLower lexicalZLower annotation fixpoint =
	  EOPVaridTh          annotation lexicalEndOfParse
		-- ^ We're provided with a certification that no more characters will
		-- follow in whatever we're parsing, and ‘th’ is not excluded.
	| InnerSansAscVaridTh annotation varidInnerSansAscSmallUnderscore (list varidInner)
		-- ^ Characters we know aren't excluded are valid characters.
	| ALowerVaridTh       annotation lexicalALower                    (list varidInner)
		-- ^ ‘tha’ starts no excluded identifier.
	| BLowerVaridTh       annotation lexicalBLower                    (list varidInner)
		-- ^ ‘thb’ starts no excluded identifier.
	| CLowerVaridTh       annotation lexicalCLower                    (list varidInner)
		-- ^ ‘thc’ starts no excluded identifier.
	| DLowerVaridTh       annotation lexicalDLower                    (list varidInner)
		-- ^ ‘thd’ starts no excluded identifier.
	| ELowerVaridTh       annotation lexicalELower                    varidThe
		-- ^ ‘the’ starts 1 excluded identifier.
	| FLowerVaridTh       annotation lexicalFLower                    (list varidInner)
		-- ^ ‘thf’ starts no excluded identifier.
	| GLowerVaridTh       annotation lexicalGLower                    (list varidInner)
		-- ^ ‘thg’ starts no excluded identifier.
	| HLowerVaridTh       annotation lexicalHLower                    (list varidInner)
		-- ^ ‘thh’ starts no excluded identifier.
	| ILowerVaridTh       annotation lexicalILower                    (list varidInner)
		-- ^ ‘thi’ starts no excluded identifier.
	| JLowerVaridTh       annotation lexicalJLower                    (list varidInner)
		-- ^ ‘thj’ starts no excluded identifier.
	| KLowerVaridTh       annotation lexicalKLower                    (list varidInner)
		-- ^ ‘thk’ starts no excluded identifier.
	| LLowerVaridTh       annotation lexicalLLower                    (list varidInner)
		-- ^ ‘thl’ starts no excluded identifier.
	| MLowerVaridTh       annotation lexicalMLower                    (list varidInner)
		-- ^ ‘thm’ starts no excluded identifier.
	| NLowerVaridTh       annotation lexicalNLower                    (list varidInner)
		-- ^ ‘thn’ starts no excluded identifier.
	| OLowerVaridTh       annotation lexicalOLower                    (list varidInner)
		-- ^ ‘tho’ starts no excluded identifier.
	| PLowerVaridTh       annotation lexicalPLower                    (list varidInner)
		-- ^ ‘thp’ starts no excluded identifier.
	| QLowerVaridTh       annotation lexicalQLower                    (list varidInner)
		-- ^ ‘thq’ starts no excluded identifier.
	| RLowerVaridTh       annotation lexicalRLower                    (list varidInner)
		-- ^ ‘thr’ starts no excluded identifier.
	| SLowerVaridTh       annotation lexicalSLower                    (list varidInner)
		-- ^ ‘ths’ starts no excluded identifier.
	| TLowerVaridTh       annotation lexicalTLower                    (list varidInner)
		-- ^ ‘tht’ starts no excluded identifier.
	| ULowerVaridTh       annotation lexicalULower                    (list varidInner)
		-- ^ ‘thu’ starts no excluded identifier.
	| VLowerVaridTh       annotation lexicalVLower                    (list varidInner)
		-- ^ ‘thv’ starts no excluded identifier.
	| WLowerVaridTh       annotation lexicalWLower                    (list varidInner)
		-- ^ ‘thw’ starts no excluded identifier.
	| XLowerVaridTh       annotation lexicalXLower                    (list varidInner)
		-- ^ ‘thx’ starts no excluded identifier.
	| YLowerVaridTh       annotation lexicalYLower                    (list varidInner)
		-- ^ ‘thy’ starts no excluded identifier.
	| ZLowerVaridTh       annotation lexicalZLower                    (list varidInner)
		-- ^ ‘thz’ starts no excluded identifier.

-- | A non-symbolic variable (lowercase-style) identifier name, excluding
-- reserved names, after an ‘ty’ was parsed from the beginning.
data VaridTyBase list lexicalEndOfParse varidInnerSansAscSmallUnderscore varidInner lexicalALower lexicalBLower lexicalCLower lexicalDLower lexicalELower lexicalFLower lexicalGLower lexicalHLower lexicalILower lexicalJLower lexicalKLower lexicalLLower lexicalMLower lexicalNLower lexicalOLower lexicalPLower lexicalQLower lexicalRLower lexicalSLower lexicalTLower lexicalULower lexicalVLower lexicalWLower lexicalXLower lexicalYLower varidTyp lexicalZLower annotation fixpoint =
	  EOPVaridTy          annotation lexicalEndOfParse
		-- ^ We're provided with a certification that no more characters will
		-- follow in whatever we're parsing, and ‘ty’ is not excluded.
	| InnerSansAscVaridTy annotation varidInnerSansAscSmallUnderscore (list varidInner)
		-- ^ Characters we know aren't excluded are valid characters.
	| ALowerVaridTy       annotation lexicalALower                    (list varidInner)
		-- ^ ‘tya’ starts no excluded identifier.
	| BLowerVaridTy       annotation lexicalBLower                    (list varidInner)
		-- ^ ‘tyb’ starts no excluded identifier.
	| CLowerVaridTy       annotation lexicalCLower                    (list varidInner)
		-- ^ ‘tyc’ starts no excluded identifier.
	| DLowerVaridTy       annotation lexicalDLower                    (list varidInner)
		-- ^ ‘tyd’ starts no excluded identifier.
	| ELowerVaridTy       annotation lexicalELower                    (list varidInner)
		-- ^ ‘tye’ starts no excluded identifier.
	| FLowerVaridTy       annotation lexicalFLower                    (list varidInner)
		-- ^ ‘tyf’ starts no excluded identifier.
	| GLowerVaridTy       annotation lexicalGLower                    (list varidInner)
		-- ^ ‘tyg’ starts no excluded identifier.
	| HLowerVaridTy       annotation lexicalHLower                    (list varidInner)
		-- ^ ‘tyh’ starts no excluded identifier.
	| ILowerVaridTy       annotation lexicalILower                    (list varidInner)
		-- ^ ‘tyi’ starts no excluded identifier.
	| JLowerVaridTy       annotation lexicalJLower                    (list varidInner)
		-- ^ ‘tyj’ starts no excluded identifier.
	| KLowerVaridTy       annotation lexicalKLower                    (list varidInner)
		-- ^ ‘tyk’ starts no excluded identifier.
	| LLowerVaridTy       annotation lexicalLLower                    (list varidInner)
		-- ^ ‘tyl’ starts no excluded identifier.
	| MLowerVaridTy       annotation lexicalMLower                    (list varidInner)
		-- ^ ‘tym’ starts no excluded identifier.
	| NLowerVaridTy       annotation lexicalNLower                    (list varidInner)
		-- ^ ‘tyn’ starts no excluded identifier.
	| OLowerVaridTy       annotation lexicalOLower                    (list varidInner)
		-- ^ ‘tyo’ starts no excluded identifier.
	| PLowerVaridTy       annotation lexicalPLower                    varidTyp
		-- ^ ‘typ’ starts 1 excluded identifier.
	| QLowerVaridTy       annotation lexicalQLower                    (list varidInner)
		-- ^ ‘tyq’ starts no excluded identifier.
	| RLowerVaridTy       annotation lexicalRLower                    (list varidInner)
		-- ^ ‘tyr’ starts no excluded identifier.
	| SLowerVaridTy       annotation lexicalSLower                    (list varidInner)
		-- ^ ‘tys’ starts no excluded identifier.
	| TLowerVaridTy       annotation lexicalTLower                    (list varidInner)
		-- ^ ‘tyt’ starts no excluded identifier.
	| ULowerVaridTy       annotation lexicalULower                    (list varidInner)
		-- ^ ‘tyu’ starts no excluded identifier.
	| VLowerVaridTy       annotation lexicalVLower                    (list varidInner)
		-- ^ ‘tyv’ starts no excluded identifier.
	| WLowerVaridTy       annotation lexicalWLower                    (list varidInner)
		-- ^ ‘tyw’ starts no excluded identifier.
	| XLowerVaridTy       annotation lexicalXLower                    (list varidInner)
		-- ^ ‘tyx’ starts no excluded identifier.
	| YLowerVaridTy       annotation lexicalYLower                    (list varidInner)
		-- ^ ‘tyy’ starts no excluded identifier.
	| ZLowerVaridTy       annotation lexicalZLower                    (list varidInner)
		-- ^ ‘tyz’ starts no excluded identifier.

-- | A non-symbolic variable (lowercase-style) identifier name, excluding
-- reserved names, after an ‘cas’ was parsed from the beginning.
data VaridCasBase list lexicalEndOfParse varidInnerSansAscSmallUnderscore varidInner lexicalALower lexicalBLower lexicalCLower lexicalDLower lexicalELower lexicalFLower lexicalGLower lexicalHLower lexicalILower lexicalJLower lexicalKLower lexicalLLower lexicalMLower lexicalNLower lexicalOLower lexicalPLower lexicalQLower lexicalRLower lexicalSLower lexicalTLower lexicalULower lexicalVLower lexicalWLower lexicalXLower lexicalYLower lexicalZLower annotation fixpoint =
	  EOPVaridCas          annotation lexicalEndOfParse
		-- ^ We're provided with a certification that no more characters will
		-- follow in whatever we're parsing, and ‘cas’ is not excluded.
	| InnerSansAscVaridCas annotation varidInnerSansAscSmallUnderscore (list varidInner)
		-- ^ Characters we know aren't excluded are valid characters.
	| ALowerVaridCas       annotation lexicalALower                    (list varidInner)
		-- ^ ‘casa’ starts no excluded identifier.
	| BLowerVaridCas       annotation lexicalBLower                    (list varidInner)
		-- ^ ‘casb’ starts no excluded identifier.
	| CLowerVaridCas       annotation lexicalCLower                    (list varidInner)
		-- ^ ‘casc’ starts no excluded identifier.
	| DLowerVaridCas       annotation lexicalDLower                    (list varidInner)
		-- ^ ‘casd’ starts no excluded identifier.
	| ELowerVaridCas       annotation lexicalELower                    varidInner        (list varidInner)
		-- ^ ‘case’ alone is excluded.  Additional characters is valid.
	| FLowerVaridCas       annotation lexicalFLower                    (list varidInner)
		-- ^ ‘casf’ starts no excluded identifier.
	| GLowerVaridCas       annotation lexicalGLower                    (list varidInner)
		-- ^ ‘casg’ starts no excluded identifier.
	| HLowerVaridCas       annotation lexicalHLower                    (list varidInner)
		-- ^ ‘cash’ starts no excluded identifier.
	| ILowerVaridCas       annotation lexicalILower                    (list varidInner)
		-- ^ ‘casi’ starts no excluded identifier.
	| JLowerVaridCas       annotation lexicalJLower                    (list varidInner)
		-- ^ ‘casj’ starts no excluded identifier.
	| KLowerVaridCas       annotation lexicalKLower                    (list varidInner)
		-- ^ ‘cask’ starts no excluded identifier.
	| LLowerVaridCas       annotation lexicalLLower                    (list varidInner)
		-- ^ ‘casl’ starts no excluded identifier.
	| MLowerVaridCas       annotation lexicalMLower                    (list varidInner)
		-- ^ ‘casm’ starts no excluded identifier.
	| NLowerVaridCas       annotation lexicalNLower                    (list varidInner)
		-- ^ ‘casn’ starts no excluded identifier.
	| OLowerVaridCas       annotation lexicalOLower                    (list varidInner)
		-- ^ ‘caso’ starts no excluded identifier.
	| PLowerVaridCas       annotation lexicalPLower                    (list varidInner)
		-- ^ ‘casp’ starts no excluded identifier.
	| QLowerVaridCas       annotation lexicalQLower                    (list varidInner)
		-- ^ ‘casq’ starts no excluded identifier.
	| RLowerVaridCas       annotation lexicalRLower                    (list varidInner)
		-- ^ ‘casr’ starts no excluded identifier.
	| SLowerVaridCas       annotation lexicalSLower                    (list varidInner)
		-- ^ ‘cass’ starts no excluded identifier.
	| TLowerVaridCas       annotation lexicalTLower                    (list varidInner)
		-- ^ ‘cast’ starts no excluded identifier.
	| ULowerVaridCas       annotation lexicalULower                    (list varidInner)
		-- ^ ‘casu’ starts no excluded identifier.
	| VLowerVaridCas       annotation lexicalVLower                    (list varidInner)
		-- ^ ‘casv’ starts no excluded identifier.
	| WLowerVaridCas       annotation lexicalWLower                    (list varidInner)
		-- ^ ‘casw’ starts no excluded identifier.
	| XLowerVaridCas       annotation lexicalXLower                    (list varidInner)
		-- ^ ‘casx’ starts no excluded identifier.
	| YLowerVaridCas       annotation lexicalYLower                    (list varidInner)
		-- ^ ‘casy’ starts no excluded identifier.
	| ZLowerVaridCas       annotation lexicalZLower                    (list varidInner)
		-- ^ ‘casz’ starts no excluded identifier.

-- | A non-symbolic variable (lowercase-style) identifier name, excluding
-- reserved names, after an ‘cla’ was parsed from the beginning.
data VaridClaBase list lexicalEndOfParse varidInnerSansAscSmallUnderscore varidInner lexicalALower lexicalBLower lexicalCLower lexicalDLower lexicalELower lexicalFLower lexicalGLower lexicalHLower lexicalILower lexicalJLower lexicalKLower lexicalLLower lexicalMLower lexicalNLower lexicalOLower lexicalPLower lexicalQLower lexicalRLower lexicalSLower varidClas lexicalTLower lexicalULower lexicalVLower lexicalWLower lexicalXLower lexicalYLower lexicalZLower annotation fixpoint =
	  EOPVaridCla          annotation lexicalEndOfParse
		-- ^ We're provided with a certification that no more characters will
		-- follow in whatever we're parsing, and ‘cla’ is not excluded.
	| InnerSansAscVaridCla annotation varidInnerSansAscSmallUnderscore (list varidInner)
		-- ^ Characters we know aren't excluded are valid characters.
	| ALowerVaridCla       annotation lexicalALower                    (list varidInner)
		-- ^ ‘claa’ starts no excluded identifier.
	| BLowerVaridCla       annotation lexicalBLower                    (list varidInner)
		-- ^ ‘clab’ starts no excluded identifier.
	| CLowerVaridCla       annotation lexicalCLower                    (list varidInner)
		-- ^ ‘clac’ starts no excluded identifier.
	| DLowerVaridCla       annotation lexicalDLower                    (list varidInner)
		-- ^ ‘clad’ starts no excluded identifier.
	| ELowerVaridCla       annotation lexicalELower                    (list varidInner)
		-- ^ ‘clae’ starts no excluded identifier.
	| FLowerVaridCla       annotation lexicalFLower                    (list varidInner)
		-- ^ ‘claf’ starts no excluded identifier.
	| GLowerVaridCla       annotation lexicalGLower                    (list varidInner)
		-- ^ ‘clag’ starts no excluded identifier.
	| HLowerVaridCla       annotation lexicalHLower                    (list varidInner)
		-- ^ ‘clah’ starts no excluded identifier.
	| ILowerVaridCla       annotation lexicalILower                    (list varidInner)
		-- ^ ‘clai’ starts no excluded identifier.
	| JLowerVaridCla       annotation lexicalJLower                    (list varidInner)
		-- ^ ‘claj’ starts no excluded identifier.
	| KLowerVaridCla       annotation lexicalKLower                    (list varidInner)
		-- ^ ‘clak’ starts no excluded identifier.
	| LLowerVaridCla       annotation lexicalLLower                    (list varidInner)
		-- ^ ‘clal’ starts no excluded identifier.
	| MLowerVaridCla       annotation lexicalMLower                    (list varidInner)
		-- ^ ‘clam’ starts no excluded identifier.
	| NLowerVaridCla       annotation lexicalNLower                    (list varidInner)
		-- ^ ‘clan’ starts no excluded identifier.
	| OLowerVaridCla       annotation lexicalOLower                    (list varidInner)
		-- ^ ‘clao’ starts no excluded identifier.
	| PLowerVaridCla       annotation lexicalPLower                    (list varidInner)
		-- ^ ‘clap’ starts no excluded identifier.
	| QLowerVaridCla       annotation lexicalQLower                    (list varidInner)
		-- ^ ‘claq’ starts no excluded identifier.
	| RLowerVaridCla       annotation lexicalRLower                    (list varidInner)
		-- ^ ‘clar’ starts no excluded identifier.
	| SLowerVaridCla       annotation lexicalSLower                    varidClas
		-- ^ ‘clas’ starts 1 excluded identifier.
	| TLowerVaridCla       annotation lexicalTLower                    (list varidInner)
		-- ^ ‘clat’ starts no excluded identifier.
	| ULowerVaridCla       annotation lexicalULower                    (list varidInner)
		-- ^ ‘clau’ starts no excluded identifier.
	| VLowerVaridCla       annotation lexicalVLower                    (list varidInner)
		-- ^ ‘clav’ starts no excluded identifier.
	| WLowerVaridCla       annotation lexicalWLower                    (list varidInner)
		-- ^ ‘claw’ starts no excluded identifier.
	| XLowerVaridCla       annotation lexicalXLower                    (list varidInner)
		-- ^ ‘clax’ starts no excluded identifier.
	| YLowerVaridCla       annotation lexicalYLower                    (list varidInner)
		-- ^ ‘clay’ starts no excluded identifier.
	| ZLowerVaridCla       annotation lexicalZLower                    (list varidInner)
		-- ^ ‘claz’ starts no excluded identifier.

-- | A non-symbolic variable (lowercase-style) identifier name, excluding
-- reserved names, after an ‘dat’ was parsed from the beginning.
data VaridDatBase list lexicalEndOfParse varidInnerSansAscSmallUnderscore varidInner lexicalALower lexicalBLower lexicalCLower lexicalDLower lexicalELower lexicalFLower lexicalGLower lexicalHLower lexicalILower lexicalJLower lexicalKLower lexicalLLower lexicalMLower lexicalNLower lexicalOLower lexicalPLower lexicalQLower lexicalRLower lexicalSLower lexicalTLower lexicalULower lexicalVLower lexicalWLower lexicalXLower lexicalYLower lexicalZLower annotation fixpoint =
	  EOPVaridDat          annotation lexicalEndOfParse
		-- ^ We're provided with a certification that no more characters will
		-- follow in whatever we're parsing, and ‘dat’ is not excluded.
	| InnerSansAscVaridDat annotation varidInnerSansAscSmallUnderscore (list varidInner)
		-- ^ Characters we know aren't excluded are valid characters.
	| ALowerVaridDat       annotation lexicalALower                    varidInner        (list varidInner)
		-- ^ ‘data’ alone is excluded.  Additional characters is valid.
	| BLowerVaridDat       annotation lexicalBLower                    (list varidInner)
		-- ^ ‘datb’ starts no excluded identifier.
	| CLowerVaridDat       annotation lexicalCLower                    (list varidInner)
		-- ^ ‘datc’ starts no excluded identifier.
	| DLowerVaridDat       annotation lexicalDLower                    (list varidInner)
		-- ^ ‘datd’ starts no excluded identifier.
	| ELowerVaridDat       annotation lexicalELower                    (list varidInner)
		-- ^ ‘date’ starts no excluded identifier
	| FLowerVaridDat       annotation lexicalFLower                    (list varidInner)
		-- ^ ‘datf’ starts no excluded identifier.
	| GLowerVaridDat       annotation lexicalGLower                    (list varidInner)
		-- ^ ‘datg’ starts no excluded identifier.
	| HLowerVaridDat       annotation lexicalHLower                    (list varidInner)
		-- ^ ‘dath’ starts no excluded identifier.
	| ILowerVaridDat       annotation lexicalILower                    (list varidInner)
		-- ^ ‘dati’ starts no excluded identifier.
	| JLowerVaridDat       annotation lexicalJLower                    (list varidInner)
		-- ^ ‘datj’ starts no excluded identifier.
	| KLowerVaridDat       annotation lexicalKLower                    (list varidInner)
		-- ^ ‘datk’ starts no excluded identifier.
	| LLowerVaridDat       annotation lexicalLLower                    (list varidInner)
		-- ^ ‘datl’ starts no excluded identifier.
	| MLowerVaridDat       annotation lexicalMLower                    (list varidInner)
		-- ^ ‘datm’ starts no excluded identifier.
	| NLowerVaridDat       annotation lexicalNLower                    (list varidInner)
		-- ^ ‘datn’ starts no excluded identifier.
	| OLowerVaridDat       annotation lexicalOLower                    (list varidInner)
		-- ^ ‘dato’ starts no excluded identifier.
	| PLowerVaridDat       annotation lexicalPLower                    (list varidInner)
		-- ^ ‘datp’ starts no excluded identifier.
	| QLowerVaridDat       annotation lexicalQLower                    (list varidInner)
		-- ^ ‘datq’ starts no excluded identifier.
	| RLowerVaridDat       annotation lexicalRLower                    (list varidInner)
		-- ^ ‘datr’ starts no excluded identifier.
	| SLowerVaridDat       annotation lexicalSLower                    (list varidInner)
		-- ^ ‘dats’ starts no excluded identifier.
	| TLowerVaridDat       annotation lexicalTLower                    (list varidInner)
		-- ^ ‘datt’ starts no excluded identifier.
	| ULowerVaridDat       annotation lexicalULower                    (list varidInner)
		-- ^ ‘datu’ starts no excluded identifier.
	| VLowerVaridDat       annotation lexicalVLower                    (list varidInner)
		-- ^ ‘datv’ starts no excluded identifier.
	| WLowerVaridDat       annotation lexicalWLower                    (list varidInner)
		-- ^ ‘datw’ starts no excluded identifier.
	| XLowerVaridDat       annotation lexicalXLower                    (list varidInner)
		-- ^ ‘datx’ starts no excluded identifier.
	| YLowerVaridDat       annotation lexicalYLower                    (list varidInner)
		-- ^ ‘daty’ starts no excluded identifier.
	| ZLowerVaridDat       annotation lexicalZLower                    (list varidInner)
		-- ^ ‘datz’ starts no excluded identifier.

-- | A non-symbolic variable (lowercase-style) identifier name, excluding
-- reserved names, after an ‘def’ was parsed from the beginning.
data VaridDefBase list lexicalEndOfParse varidInnerSansAscSmallUnderscore varidInner lexicalALower varidDefa lexicalBLower lexicalCLower lexicalDLower lexicalELower lexicalFLower lexicalGLower lexicalHLower lexicalILower lexicalJLower lexicalKLower lexicalLLower lexicalMLower lexicalNLower lexicalOLower lexicalPLower lexicalQLower lexicalRLower lexicalSLower lexicalTLower lexicalULower lexicalVLower lexicalWLower lexicalXLower lexicalYLower lexicalZLower annotation fixpoint =
	  EOPVaridDef          annotation lexicalEndOfParse
		-- ^ We're provided with a certification that no more characters will
		-- follow in whatever we're parsing, and ‘def’ is not excluded.
	| InnerSansAscVaridDef annotation varidInnerSansAscSmallUnderscore (list varidInner)
		-- ^ Characters we know aren't excluded are valid characters.
	| ALowerVaridDef       annotation lexicalALower                    varidDefa
		-- ^ ‘defa’ starts 1 excluded identifier.
	| BLowerVaridDef       annotation lexicalBLower                    (list varidInner)
		-- ^ ‘defb’ starts no excluded identifier.
	| CLowerVaridDef       annotation lexicalCLower                    (list varidInner)
		-- ^ ‘defc’ starts no excluded identifier.
	| DLowerVaridDef       annotation lexicalDLower                    (list varidInner)
		-- ^ ‘defd’ starts no excluded identifier.
	| ELowerVaridDef       annotation lexicalELower                    (list varidInner)
		-- ^ ‘defe’ starts no excluded identifier.
	| FLowerVaridDef       annotation lexicalFLower                    (list varidInner)
		-- ^ ‘deff’ starts no excluded identifier.
	| GLowerVaridDef       annotation lexicalGLower                    (list varidInner)
		-- ^ ‘defg’ starts no excluded identifier.
	| HLowerVaridDef       annotation lexicalHLower                    (list varidInner)
		-- ^ ‘defh’ starts no excluded identifier.
	| ILowerVaridDef       annotation lexicalILower                    (list varidInner)
		-- ^ ‘defi’ starts no excluded identifier.
	| JLowerVaridDef       annotation lexicalJLower                    (list varidInner)
		-- ^ ‘defj’ starts no excluded identifier.
	| KLowerVaridDef       annotation lexicalKLower                    (list varidInner)
		-- ^ ‘defk’ starts no excluded identifier.
	| LLowerVaridDef       annotation lexicalLLower                    (list varidInner)
		-- ^ ‘defl’ starts no excluded identifier.
	| MLowerVaridDef       annotation lexicalMLower                    (list varidInner)
		-- ^ ‘defm’ starts no excluded identifier.
	| NLowerVaridDef       annotation lexicalNLower                    (list varidInner)
		-- ^ ‘defn’ starts no excluded identifier.
	| OLowerVaridDef       annotation lexicalOLower                    (list varidInner)
		-- ^ ‘defo’ starts no excluded identifier.
	| PLowerVaridDef       annotation lexicalPLower                    (list varidInner)
		-- ^ ‘defp’ starts no excluded identifier.
	| QLowerVaridDef       annotation lexicalQLower                    (list varidInner)
		-- ^ ‘defq’ starts no excluded identifier.
	| RLowerVaridDef       annotation lexicalRLower                    (list varidInner)
		-- ^ ‘defr’ starts no excluded identifier.
	| SLowerVaridDef       annotation lexicalSLower                    (list varidInner)
		-- ^ ‘defs’ starts no excluded identifier.
	| TLowerVaridDef       annotation lexicalTLower                    (list varidInner)
		-- ^ ‘deft’ starts no excluded identifier.
	| ULowerVaridDef       annotation lexicalULower                    (list varidInner)
		-- ^ ‘defu’ starts no excluded identifier.
	| VLowerVaridDef       annotation lexicalVLower                    (list varidInner)
		-- ^ ‘defv’ starts no excluded identifier.
	| WLowerVaridDef       annotation lexicalWLower                    (list varidInner)
		-- ^ ‘defw’ starts no excluded identifier.
	| XLowerVaridDef       annotation lexicalXLower                    (list varidInner)
		-- ^ ‘defx’ starts no excluded identifier.
	| YLowerVaridDef       annotation lexicalYLower                    (list varidInner)
		-- ^ ‘defy’ starts no excluded identifier.
	| ZLowerVaridDef       annotation lexicalZLower                    (list varidInner)
		-- ^ ‘defz’ starts no excluded identifier.

-- | A non-symbolic variable (lowercase-style) identifier name, excluding
-- reserved names, after an ‘der’ was parsed from the beginning.
data VaridDerBase list lexicalEndOfParse varidInnerSansAscSmallUnderscore varidInner lexicalALower lexicalBLower lexicalCLower lexicalDLower lexicalELower lexicalFLower lexicalGLower lexicalHLower lexicalILower varidDeri lexicalJLower lexicalKLower lexicalLLower lexicalMLower lexicalNLower lexicalOLower lexicalPLower lexicalQLower lexicalRLower lexicalSLower lexicalTLower lexicalULower lexicalVLower lexicalWLower lexicalXLower lexicalYLower lexicalZLower annotation fixpoint =
	  EOPVaridDer          annotation lexicalEndOfParse
		-- ^ We're provided with a certification that no more characters will
		-- follow in whatever we're parsing, and ‘der’ is not excluded.
	| InnerSansAscVaridDer annotation varidInnerSansAscSmallUnderscore (list varidInner)
		-- ^ Characters we know aren't excluded are valid characters.
	| ALowerVaridDer       annotation lexicalALower                    (list varidInner)
		-- ^ ‘dera’ starts no excluded identifier.
	| BLowerVaridDer       annotation lexicalBLower                    (list varidInner)
		-- ^ ‘derb’ starts no excluded identifier.
	| CLowerVaridDer       annotation lexicalCLower                    (list varidInner)
		-- ^ ‘derc’ starts no excluded identifier.
	| DLowerVaridDer       annotation lexicalDLower                    (list varidInner)
		-- ^ ‘derd’ starts no excluded identifier.
	| ELowerVaridDer       annotation lexicalELower                    (list varidInner)
		-- ^ ‘dere’ starts no excluded identifier.
	| FLowerVaridDer       annotation lexicalFLower                    (list varidInner)
		-- ^ ‘derf’ starts no excluded identifier.
	| GLowerVaridDer       annotation lexicalGLower                    (list varidInner)
		-- ^ ‘derg’ starts no excluded identifier.
	| HLowerVaridDer       annotation lexicalHLower                    (list varidInner)
		-- ^ ‘derh’ starts no excluded identifier.
	| ILowerVaridDer       annotation lexicalILower                    varidDeri
		-- ^ ‘deri’ starts 1 excluded identifier.
	| JLowerVaridDer       annotation lexicalJLower                    (list varidInner)
		-- ^ ‘derj’ starts no excluded identifier.
	| KLowerVaridDer       annotation lexicalKLower                    (list varidInner)
		-- ^ ‘derk’ starts no excluded identifier.
	| LLowerVaridDer       annotation lexicalLLower                    (list varidInner)
		-- ^ ‘derl’ starts no excluded identifier.
	| MLowerVaridDer       annotation lexicalMLower                    (list varidInner)
		-- ^ ‘derm’ starts no excluded identifier.
	| NLowerVaridDer       annotation lexicalNLower                    (list varidInner)
		-- ^ ‘dern’ starts no excluded identifier.
	| OLowerVaridDer       annotation lexicalOLower                    (list varidInner)
		-- ^ ‘dero’ starts no excluded identifier.
	| PLowerVaridDer       annotation lexicalPLower                    (list varidInner)
		-- ^ ‘derp’ starts no excluded identifier.
	| QLowerVaridDer       annotation lexicalQLower                    (list varidInner)
		-- ^ ‘derq’ starts no excluded identifier.
	| RLowerVaridDer       annotation lexicalRLower                    (list varidInner)
		-- ^ ‘derr’ starts no excluded identifier.
	| SLowerVaridDer       annotation lexicalSLower                    (list varidInner)
		-- ^ ‘ders’ starts no excluded identifier.
	| TLowerVaridDer       annotation lexicalTLower                    (list varidInner)
		-- ^ ‘dert’ starts no excluded identifier.
	| ULowerVaridDer       annotation lexicalULower                    (list varidInner)
		-- ^ ‘deru’ starts no excluded identifier.
	| VLowerVaridDer       annotation lexicalVLower                    (list varidInner)
		-- ^ ‘derv’ starts no excluded identifier.
	| WLowerVaridDer       annotation lexicalWLower                    (list varidInner)
		-- ^ ‘derw’ starts no excluded identifier.
	| XLowerVaridDer       annotation lexicalXLower                    (list varidInner)
		-- ^ ‘derx’ starts no excluded identifier.
	| YLowerVaridDer       annotation lexicalYLower                    (list varidInner)
		-- ^ ‘dery’ starts no excluded identifier.
	| ZLowerVaridDer       annotation lexicalZLower                    (list varidInner)
		-- ^ ‘derz’ starts no excluded identifier.

-- | A non-symbolic variable (lowercase-style) identifier name, excluding
-- reserved names, after an ‘els’ was parsed from the beginning.
data VaridElsBase list lexicalEndOfParse varidInnerSansAscSmallUnderscore varidInner lexicalALower lexicalBLower lexicalCLower lexicalDLower lexicalELower lexicalFLower lexicalGLower lexicalHLower lexicalILower lexicalJLower lexicalKLower lexicalLLower lexicalMLower lexicalNLower lexicalOLower lexicalPLower lexicalQLower lexicalRLower lexicalSLower lexicalTLower lexicalULower lexicalVLower lexicalWLower lexicalXLower lexicalYLower lexicalZLower annotation fixpoint =
	  EOPVaridEls          annotation lexicalEndOfParse
		-- ^ We're provided with a certification that no more characters will
		-- follow in whatever we're parsing, and ‘els’ is not excluded.
	| InnerSansAscVaridEls annotation varidInnerSansAscSmallUnderscore (list varidInner)
		-- ^ Characters we know aren't excluded are valid characters.
	| ALowerVaridEls       annotation lexicalALower                    (list varidInner)
		-- ^ ‘elsa’ starts no excluded identifier.
	| BLowerVaridEls       annotation lexicalBLower                    (list varidInner)
		-- ^ ‘elsb’ starts no excluded identifier.
	| CLowerVaridEls       annotation lexicalCLower                    (list varidInner)
		-- ^ ‘elsc’ starts no excluded identifier.
	| DLowerVaridEls       annotation lexicalDLower                    (list varidInner)
		-- ^ ‘elsd’ starts no excluded identifier.
	| ELowerVaridEls       annotation lexicalELower                    varidInner        (list varidInner)
		-- ^ ‘else’ alone is excluded.  Additional characters is valid.
	| FLowerVaridEls       annotation lexicalFLower                    (list varidInner)
		-- ^ ‘elsf’ starts no excluded identifier.
	| GLowerVaridEls       annotation lexicalGLower                    (list varidInner)
		-- ^ ‘elsg’ starts no excluded identifier.
	| HLowerVaridEls       annotation lexicalHLower                    (list varidInner)
		-- ^ ‘elsh’ starts no excluded identifier.
	| ILowerVaridEls       annotation lexicalILower                    (list varidInner)
		-- ^ ‘elsi’ starts no excluded identifier.
	| JLowerVaridEls       annotation lexicalJLower                    (list varidInner)
		-- ^ ‘elsj’ starts no excluded identifier.
	| KLowerVaridEls       annotation lexicalKLower                    (list varidInner)
		-- ^ ‘elsk’ starts no excluded identifier.
	| LLowerVaridEls       annotation lexicalLLower                    (list varidInner)
		-- ^ ‘elsl’ starts no excluded identifier.
	| MLowerVaridEls       annotation lexicalMLower                    (list varidInner)
		-- ^ ‘elsm’ starts no excluded identifier.
	| NLowerVaridEls       annotation lexicalNLower                    (list varidInner)
		-- ^ ‘elsn’ starts no excluded identifier.
	| OLowerVaridEls       annotation lexicalOLower                    (list varidInner)
		-- ^ ‘elso’ starts no excluded identifier.
	| PLowerVaridEls       annotation lexicalPLower                    (list varidInner)
		-- ^ ‘elsp’ starts no excluded identifier.
	| QLowerVaridEls       annotation lexicalQLower                    (list varidInner)
		-- ^ ‘elsq’ starts no excluded identifier.
	| RLowerVaridEls       annotation lexicalRLower                    (list varidInner)
		-- ^ ‘elsr’ starts no excluded identifier.
	| SLowerVaridEls       annotation lexicalSLower                    (list varidInner)
		-- ^ ‘elss’ starts no excluded identifier.
	| TLowerVaridEls       annotation lexicalTLower                    (list varidInner)
		-- ^ ‘elst’ starts no excluded identifier.
	| ULowerVaridEls       annotation lexicalULower                    (list varidInner)
		-- ^ ‘elsu’ starts no excluded identifier.
	| VLowerVaridEls       annotation lexicalVLower                    (list varidInner)
		-- ^ ‘elsv’ starts no excluded identifier.
	| WLowerVaridEls       annotation lexicalWLower                    (list varidInner)
		-- ^ ‘elsw’ starts no excluded identifier.
	| XLowerVaridEls       annotation lexicalXLower                    (list varidInner)
		-- ^ ‘elsx’ starts no excluded identifier.
	| YLowerVaridEls       annotation lexicalYLower                    (list varidInner)
		-- ^ ‘elsy’ starts no excluded identifier.
	| ZLowerVaridEls       annotation lexicalZLower                    (list varidInner)
		-- ^ ‘elsz’ starts no excluded identifier.

-- | A non-symbolic variable (lowercase-style) identifier name, excluding
-- reserved names, after an ‘for’ was parsed from the beginning.
data VaridForBase list lexicalEndOfParse varidInnerSansAscSmallUnderscore varidInner lexicalALower lexicalBLower lexicalCLower lexicalDLower lexicalELower varidFore lexicalFLower lexicalGLower lexicalHLower lexicalILower lexicalJLower lexicalKLower lexicalLLower lexicalMLower lexicalNLower lexicalOLower lexicalPLower lexicalQLower lexicalRLower lexicalSLower lexicalTLower lexicalULower lexicalVLower lexicalWLower lexicalXLower lexicalYLower lexicalZLower annotation fixpoint =
	  EOPVaridFor          annotation lexicalEndOfParse
		-- ^ We're provided with a certification that no more characters will
		-- follow in whatever we're parsing, and ‘for’ is not excluded.
	| InnerSansAscVaridFor annotation varidInnerSansAscSmallUnderscore (list varidInner)
		-- ^ Characters we know aren't excluded are valid characters.
	| ALowerVaridFor       annotation lexicalALower                    (list varidInner)
		-- ^ ‘fora’ starts no excluded identifier.
	| BLowerVaridFor       annotation lexicalBLower                    (list varidInner)
		-- ^ ‘forb’ starts no excluded identifier.
	| CLowerVaridFor       annotation lexicalCLower                    (list varidInner)
		-- ^ ‘forc’ starts no excluded identifier.
	| DLowerVaridFor       annotation lexicalDLower                    (list varidInner)
		-- ^ ‘ford’ starts no excluded identifier.
	| ELowerVaridFor       annotation lexicalELower                    varidFore
		-- ^ ‘fore’ starts 1 excluded identifier.
	| FLowerVaridFor       annotation lexicalFLower                    (list varidInner)
		-- ^ ‘forf’ starts no excluded identifier.
	| GLowerVaridFor       annotation lexicalGLower                    (list varidInner)
		-- ^ ‘forg’ starts no excluded identifier.
	| HLowerVaridFor       annotation lexicalHLower                    (list varidInner)
		-- ^ ‘forh’ starts no excluded identifier.
	| ILowerVaridFor       annotation lexicalILower                    (list varidInner)
		-- ^ ‘fori’ starts no excluded identifier.
	| JLowerVaridFor       annotation lexicalJLower                    (list varidInner)
		-- ^ ‘forj’ starts no excluded identifier.
	| KLowerVaridFor       annotation lexicalKLower                    (list varidInner)
		-- ^ ‘fork’ starts no excluded identifier.
	| LLowerVaridFor       annotation lexicalLLower                    (list varidInner)
		-- ^ ‘forl’ starts no excluded identifier.
	| MLowerVaridFor       annotation lexicalMLower                    (list varidInner)
		-- ^ ‘form’ starts no excluded identifier.
	| NLowerVaridFor       annotation lexicalNLower                    (list varidInner)
		-- ^ ‘forn’ starts no excluded identifier.
	| OLowerVaridFor       annotation lexicalOLower                    (list varidInner)
		-- ^ ‘foro’ starts no excluded identifier.
	| PLowerVaridFor       annotation lexicalPLower                    (list varidInner)
		-- ^ ‘forp’ starts no excluded identifier.
	| QLowerVaridFor       annotation lexicalQLower                    (list varidInner)
		-- ^ ‘forq’ starts no excluded identifier.
	| RLowerVaridFor       annotation lexicalRLower                    (list varidInner)
		-- ^ ‘forr’ starts no excluded identifier.
	| SLowerVaridFor       annotation lexicalSLower                    (list varidInner)
		-- ^ ‘fors’ starts no excluded identifier.
	| TLowerVaridFor       annotation lexicalTLower                    (list varidInner)
		-- ^ ‘fort’ starts no excluded identifier.
	| ULowerVaridFor       annotation lexicalULower                    (list varidInner)
		-- ^ ‘foru’ starts no excluded identifier.
	| VLowerVaridFor       annotation lexicalVLower                    (list varidInner)
		-- ^ ‘forv’ starts no excluded identifier.
	| WLowerVaridFor       annotation lexicalWLower                    (list varidInner)
		-- ^ ‘forw’ starts no excluded identifier.
	| XLowerVaridFor       annotation lexicalXLower                    (list varidInner)
		-- ^ ‘forx’ starts no excluded identifier.
	| YLowerVaridFor       annotation lexicalYLower                    (list varidInner)
		-- ^ ‘fory’ starts no excluded identifier.
	| ZLowerVaridFor       annotation lexicalZLower                    (list varidInner)
		-- ^ ‘forz’ starts no excluded identifier.

-- | A non-symbolic variable (lowercase-style) identifier name, excluding
-- reserved names, after an ‘imp’ was parsed from the beginning.
data VaridImpBase list lexicalEndOfParse varidInnerSansAscSmallUnderscore varidInner lexicalALower lexicalBLower lexicalCLower lexicalDLower lexicalELower lexicalFLower lexicalGLower lexicalHLower lexicalILower lexicalJLower lexicalKLower lexicalLLower lexicalMLower lexicalNLower lexicalOLower varidImpo lexicalPLower lexicalQLower lexicalRLower lexicalSLower lexicalTLower lexicalULower lexicalVLower lexicalWLower lexicalXLower lexicalYLower lexicalZLower annotation fixpoint =
	  EOPVaridImp          annotation lexicalEndOfParse
		-- ^ We're provided with a certification that no more characters will
		-- follow in whatever we're parsing, and ‘imp’ is not excluded.
	| InnerSansAscVaridImp annotation varidInnerSansAscSmallUnderscore (list varidInner)
		-- ^ Characters we know aren't excluded are valid characters.
	| ALowerVaridImp       annotation lexicalALower                    (list varidInner)
		-- ^ ‘impa’ starts no excluded identifier.
	| BLowerVaridImp       annotation lexicalBLower                    (list varidInner)
		-- ^ ‘impb’ starts no excluded identifier.
	| CLowerVaridImp       annotation lexicalCLower                    (list varidInner)
		-- ^ ‘impc’ starts no excluded identifier.
	| DLowerVaridImp       annotation lexicalDLower                    (list varidInner)
		-- ^ ‘impd’ starts no excluded identifier.
	| ELowerVaridImp       annotation lexicalELower                    (list varidInner)
		-- ^ ‘impe’ starts no excluded identifier.
	| FLowerVaridImp       annotation lexicalFLower                    (list varidInner)
		-- ^ ‘impf’ starts no excluded identifier.
	| GLowerVaridImp       annotation lexicalGLower                    (list varidInner)
		-- ^ ‘impg’ starts no excluded identifier.
	| HLowerVaridImp       annotation lexicalHLower                    (list varidInner)
		-- ^ ‘imph’ starts no excluded identifier.
	| ILowerVaridImp       annotation lexicalILower                    (list varidInner)
		-- ^ ‘impi’ starts no excluded identifier.
	| JLowerVaridImp       annotation lexicalJLower                    (list varidInner)
		-- ^ ‘impj’ starts no excluded identifier.
	| KLowerVaridImp       annotation lexicalKLower                    (list varidInner)
		-- ^ ‘impk’ starts no excluded identifier.
	| LLowerVaridImp       annotation lexicalLLower                    (list varidInner)
		-- ^ ‘impl’ starts no excluded identifier.
	| MLowerVaridImp       annotation lexicalMLower                    (list varidInner)
		-- ^ ‘impm’ starts no excluded identifier.
	| NLowerVaridImp       annotation lexicalNLower                    (list varidInner)
		-- ^ ‘impn’ starts no excluded identifier.
	| OLowerVaridImp       annotation lexicalOLower                    varidImpo
		-- ^ ‘impo’ starts 1 excluded identifier.
	| PLowerVaridImp       annotation lexicalPLower                    (list varidInner)
		-- ^ ‘impp’ starts no excluded identifier.
	| QLowerVaridImp       annotation lexicalQLower                    (list varidInner)
		-- ^ ‘impq’ starts no excluded identifier.
	| RLowerVaridImp       annotation lexicalRLower                    (list varidInner)
		-- ^ ‘impr’ starts no excluded identifier.
	| SLowerVaridImp       annotation lexicalSLower                    (list varidInner)
		-- ^ ‘imps’ starts no excluded identifier.
	| TLowerVaridImp       annotation lexicalTLower                    (list varidInner)
		-- ^ ‘impt’ starts no excluded identifier.
	| ULowerVaridImp       annotation lexicalULower                    (list varidInner)
		-- ^ ‘impu’ starts no excluded identifier.
	| VLowerVaridImp       annotation lexicalVLower                    (list varidInner)
		-- ^ ‘impv’ starts no excluded identifier.
	| WLowerVaridImp       annotation lexicalWLower                    (list varidInner)
		-- ^ ‘impw’ starts no excluded identifier.
	| XLowerVaridImp       annotation lexicalXLower                    (list varidInner)
		-- ^ ‘impx’ starts no excluded identifier.
	| YLowerVaridImp       annotation lexicalYLower                    (list varidInner)
		-- ^ ‘impy’ starts no excluded identifier.
	| ZLowerVaridImp       annotation lexicalZLower                    (list varidInner)
		-- ^ ‘impz’ starts no excluded identifier.

-- | A non-symbolic variable (lowercase-style) identifier name, excluding
-- reserved names, after an ‘inf’ was parsed from the beginning.
data VaridInfBase list lexicalEndOfParse varidInnerSansAscSmallUnderscore varidInner lexicalALower lexicalBLower lexicalCLower lexicalDLower lexicalELower lexicalFLower lexicalGLower lexicalHLower lexicalILower varidInfi lexicalJLower lexicalKLower lexicalLLower lexicalMLower lexicalNLower lexicalOLower lexicalPLower lexicalQLower lexicalRLower lexicalSLower lexicalTLower lexicalULower lexicalVLower lexicalWLower lexicalXLower lexicalYLower lexicalZLower annotation fixpoint =
	  EOPVaridInf          annotation lexicalEndOfParse
		-- ^ We're provided with a certification that no more characters will
		-- follow in whatever we're parsing, and ‘inf’ is not excluded.
	| InnerSansAscVaridInf annotation varidInnerSansAscSmallUnderscore (list varidInner)
		-- ^ Characters we know aren't excluded are valid characters.
	| ALowerVaridInf       annotation lexicalALower                    (list varidInner)
		-- ^ ‘infa’ starts no excluded identifier.
	| BLowerVaridInf       annotation lexicalBLower                    (list varidInner)
		-- ^ ‘infb’ starts no excluded identifier.
	| CLowerVaridInf       annotation lexicalCLower                    (list varidInner)
		-- ^ ‘infc’ starts no excluded identifier.
	| DLowerVaridInf       annotation lexicalDLower                    (list varidInner)
		-- ^ ‘infd’ starts no excluded identifier.
	| ELowerVaridInf       annotation lexicalELower                    (list varidInner)
		-- ^ ‘infe’ starts no excluded identifier.
	| FLowerVaridInf       annotation lexicalFLower                    (list varidInner)
		-- ^ ‘inff’ starts no excluded identifier.
	| GLowerVaridInf       annotation lexicalGLower                    (list varidInner)
		-- ^ ‘infg’ starts no excluded identifier.
	| HLowerVaridInf       annotation lexicalHLower                    (list varidInner)
		-- ^ ‘infh’ starts no excluded identifier.
	| ILowerVaridInf       annotation lexicalILower                    varidInfi
		-- ^ ‘infi’ starts 3 excluded identifiers.
	| JLowerVaridInf       annotation lexicalJLower                    (list varidInner)
		-- ^ ‘infj’ starts no excluded identifier.
	| KLowerVaridInf       annotation lexicalKLower                    (list varidInner)
		-- ^ ‘infk’ starts no excluded identifier.
	| LLowerVaridInf       annotation lexicalLLower                    (list varidInner)
		-- ^ ‘infl’ starts no excluded identifier.
	| MLowerVaridInf       annotation lexicalMLower                    (list varidInner)
		-- ^ ‘infm’ starts no excluded identifier.
	| NLowerVaridInf       annotation lexicalNLower                    (list varidInner)
		-- ^ ‘infn’ starts no excluded identifier.
	| OLowerVaridInf       annotation lexicalOLower                    (list varidInner)
		-- ^ ‘info’ starts no excluded identifier.
	| PLowerVaridInf       annotation lexicalPLower                    (list varidInner)
		-- ^ ‘infp’ starts no excluded identifier.
	| QLowerVaridInf       annotation lexicalQLower                    (list varidInner)
		-- ^ ‘infq’ starts no excluded identifier.
	| RLowerVaridInf       annotation lexicalRLower                    (list varidInner)
		-- ^ ‘infr’ starts no excluded identifier.
	| SLowerVaridInf       annotation lexicalSLower                    (list varidInner)
		-- ^ ‘infs’ starts no excluded identifier.
	| TLowerVaridInf       annotation lexicalTLower                    (list varidInner)
		-- ^ ‘inft’ starts no excluded identifier.
	| ULowerVaridInf       annotation lexicalULower                    (list varidInner)
		-- ^ ‘infu’ starts no excluded identifier.
	| VLowerVaridInf       annotation lexicalVLower                    (list varidInner)
		-- ^ ‘infv’ starts no excluded identifier.
	| WLowerVaridInf       annotation lexicalWLower                    (list varidInner)
		-- ^ ‘infw’ starts no excluded identifier.
	| XLowerVaridInf       annotation lexicalXLower                    (list varidInner)
		-- ^ ‘infx’ starts no excluded identifier.
	| YLowerVaridInf       annotation lexicalYLower                    (list varidInner)
		-- ^ ‘infy’ starts no excluded identifier.
	| ZLowerVaridInf       annotation lexicalZLower                    (list varidInner)
		-- ^ ‘infz’ starts no excluded identifier.

-- | A non-symbolic variable (lowercase-style) identifier name, excluding
-- reserved names, after an ‘ins’ was parsed from the beginning.
data VaridInsBase list lexicalEndOfParse varidInnerSansAscSmallUnderscore varidInner lexicalALower lexicalBLower lexicalCLower lexicalDLower lexicalELower lexicalFLower lexicalGLower lexicalHLower lexicalILower lexicalJLower lexicalKLower lexicalLLower lexicalMLower lexicalNLower lexicalOLower lexicalPLower lexicalQLower lexicalRLower lexicalSLower lexicalTLower varidInst lexicalULower lexicalVLower lexicalWLower lexicalXLower lexicalYLower lexicalZLower annotation fixpoint =
	  EOPVaridIns          annotation lexicalEndOfParse
		-- ^ We're provided with a certification that no more characters will
		-- follow in whatever we're parsing, and ‘ins’ is not excluded.
	| InnerSansAscVaridIns annotation varidInnerSansAscSmallUnderscore (list varidInner)
		-- ^ Characters we know aren't excluded are valid characters.
	| ALowerVaridIns       annotation lexicalALower                    (list varidInner)
		-- ^ ‘insa’ starts no excluded identifier.
	| BLowerVaridIns       annotation lexicalBLower                    (list varidInner)
		-- ^ ‘insb’ starts no excluded identifier.
	| CLowerVaridIns       annotation lexicalCLower                    (list varidInner)
		-- ^ ‘insc’ starts no excluded identifier.
	| DLowerVaridIns       annotation lexicalDLower                    (list varidInner)
		-- ^ ‘insd’ starts no excluded identifier.
	| ELowerVaridIns       annotation lexicalELower                    (list varidInner)
		-- ^ ‘inse’ starts no excluded identifier.
	| FLowerVaridIns       annotation lexicalFLower                    (list varidInner)
		-- ^ ‘insf’ starts no excluded identifier.
	| GLowerVaridIns       annotation lexicalGLower                    (list varidInner)
		-- ^ ‘insg’ starts no excluded identifier.
	| HLowerVaridIns       annotation lexicalHLower                    (list varidInner)
		-- ^ ‘insh’ starts no excluded identifier.
	| ILowerVaridIns       annotation lexicalILower                    (list varidInner)
		-- ^ ‘insi’ starts no excluded identifier.
	| JLowerVaridIns       annotation lexicalJLower                    (list varidInner)
		-- ^ ‘insj’ starts no excluded identifier.
	| KLowerVaridIns       annotation lexicalKLower                    (list varidInner)
		-- ^ ‘insk’ starts no excluded identifier.
	| LLowerVaridIns       annotation lexicalLLower                    (list varidInner)
		-- ^ ‘insl’ starts no excluded identifier.
	| MLowerVaridIns       annotation lexicalMLower                    (list varidInner)
		-- ^ ‘insm’ starts no excluded identifier.
	| NLowerVaridIns       annotation lexicalNLower                    (list varidInner)
		-- ^ ‘insn’ starts no excluded identifier.
	| OLowerVaridIns       annotation lexicalOLower                    (list varidInner)
		-- ^ ‘inso’ starts no excluded identifier.
	| PLowerVaridIns       annotation lexicalPLower                    (list varidInner)
		-- ^ ‘insp’ starts no excluded identifier.
	| QLowerVaridIns       annotation lexicalQLower                    (list varidInner)
		-- ^ ‘insq’ starts no excluded identifier.
	| RLowerVaridIns       annotation lexicalRLower                    (list varidInner)
		-- ^ ‘insr’ starts no excluded identifier.
	| SLowerVaridIns       annotation lexicalSLower                    (list varidInner)
		-- ^ ‘inss’ starts no excluded identifier.
	| TLowerVaridIns       annotation lexicalTLower                    varidInst
		-- ^ ‘inst’ starts 1 excluded identifier.
	| ULowerVaridIns       annotation lexicalULower                    (list varidInner)
		-- ^ ‘insu’ starts no excluded identifier.
	| VLowerVaridIns       annotation lexicalVLower                    (list varidInner)
		-- ^ ‘insv’ starts no excluded identifier.
	| WLowerVaridIns       annotation lexicalWLower                    (list varidInner)
		-- ^ ‘insw’ starts no excluded identifier.
	| XLowerVaridIns       annotation lexicalXLower                    (list varidInner)
		-- ^ ‘insx’ starts no excluded identifier.
	| YLowerVaridIns       annotation lexicalYLower                    (list varidInner)
		-- ^ ‘insy’ starts no excluded identifier.
	| ZLowerVaridIns       annotation lexicalZLower                    (list varidInner)
		-- ^ ‘insz’ starts no excluded identifier.

-- | A non-symbolic variable (lowercase-style) identifier name, excluding
-- reserved names, after an ‘mod’ was parsed from the beginning.
data VaridModBase list lexicalEndOfParse varidInnerSansAscSmallUnderscore varidInner lexicalALower lexicalBLower lexicalCLower lexicalDLower lexicalELower lexicalFLower lexicalGLower lexicalHLower lexicalILower lexicalJLower lexicalKLower lexicalLLower lexicalMLower lexicalNLower lexicalOLower lexicalPLower lexicalQLower lexicalRLower lexicalSLower lexicalTLower lexicalULower varidModu lexicalVLower lexicalWLower lexicalXLower lexicalYLower lexicalZLower annotation fixpoint =
	  EOPVaridMod          annotation lexicalEndOfParse
		-- ^ We're provided with a certification that no more characters will
		-- follow in whatever we're parsing, and ‘mod’ is not excluded.
	| InnerSansAscVaridMod annotation varidInnerSansAscSmallUnderscore (list varidInner)
		-- ^ Characters we know aren't excluded are valid characters.
	| ALowerVaridMod       annotation lexicalALower                    (list varidInner)
		-- ^ ‘moda’ starts no excluded identifier.
	| BLowerVaridMod       annotation lexicalBLower                    (list varidInner)
		-- ^ ‘modb’ starts no excluded identifier.
	| CLowerVaridMod       annotation lexicalCLower                    (list varidInner)
		-- ^ ‘modc’ starts no excluded identifier.
	| DLowerVaridMod       annotation lexicalDLower                    (list varidInner)
		-- ^ ‘modd’ starts no excluded identifier.
	| ELowerVaridMod       annotation lexicalELower                    (list varidInner)
		-- ^ ‘mode’ starts no excluded identifier.
	| FLowerVaridMod       annotation lexicalFLower                    (list varidInner)
		-- ^ ‘modf’ starts no excluded identifier.
	| GLowerVaridMod       annotation lexicalGLower                    (list varidInner)
		-- ^ ‘modg’ starts no excluded identifier.
	| HLowerVaridMod       annotation lexicalHLower                    (list varidInner)
		-- ^ ‘modh’ starts no excluded identifier.
	| ILowerVaridMod       annotation lexicalILower                    (list varidInner)
		-- ^ ‘modi’ starts no excluded identifier.
	| JLowerVaridMod       annotation lexicalJLower                    (list varidInner)
		-- ^ ‘modj’ starts no excluded identifier.
	| KLowerVaridMod       annotation lexicalKLower                    (list varidInner)
		-- ^ ‘modk’ starts no excluded identifier.
	| LLowerVaridMod       annotation lexicalLLower                    (list varidInner)
		-- ^ ‘modl’ starts no excluded identifier.
	| MLowerVaridMod       annotation lexicalMLower                    (list varidInner)
		-- ^ ‘modm’ starts no excluded identifier.
	| NLowerVaridMod       annotation lexicalNLower                    (list varidInner)
		-- ^ ‘modn’ starts no excluded identifier.
	| OLowerVaridMod       annotation lexicalOLower                    (list varidInner)
		-- ^ ‘modo’ starts no excluded identifier.
	| PLowerVaridMod       annotation lexicalPLower                    (list varidInner)
		-- ^ ‘modp’ starts no excluded identifier.
	| QLowerVaridMod       annotation lexicalQLower                    (list varidInner)
		-- ^ ‘modq’ starts no excluded identifier.
	| RLowerVaridMod       annotation lexicalRLower                    (list varidInner)
		-- ^ ‘modr’ starts no excluded identifier.
	| SLowerVaridMod       annotation lexicalSLower                    (list varidInner)
		-- ^ ‘mods’ starts no excluded identifier.
	| TLowerVaridMod       annotation lexicalTLower                    (list varidInner)
		-- ^ ‘modt’ starts no excluded identifier.
	| ULowerVaridMod       annotation lexicalULower                    varidModu
		-- ^ ‘modu’ starts 1 excluded identifier.
	| VLowerVaridMod       annotation lexicalVLower                    (list varidInner)
		-- ^ ‘modv’ starts no excluded identifier.
	| WLowerVaridMod       annotation lexicalWLower                    (list varidInner)
		-- ^ ‘modw’ starts no excluded identifier.
	| XLowerVaridMod       annotation lexicalXLower                    (list varidInner)
		-- ^ ‘modx’ starts no excluded identifier.
	| YLowerVaridMod       annotation lexicalYLower                    (list varidInner)
		-- ^ ‘mody’ starts no excluded identifier.
	| ZLowerVaridMod       annotation lexicalZLower                    (list varidInner)
		-- ^ ‘modz’ starts no excluded identifier.

-- | A non-symbolic variable (lowercase-style) identifier name, excluding
-- reserved names, after an ‘new’ was parsed from the beginning.
data VaridNewBase list lexicalEndOfParse varidInnerSansAscSmallUnderscore varidInner lexicalALower lexicalBLower lexicalCLower lexicalDLower lexicalELower lexicalFLower lexicalGLower lexicalHLower lexicalILower lexicalJLower lexicalKLower lexicalLLower lexicalMLower lexicalNLower lexicalOLower lexicalPLower lexicalQLower lexicalRLower lexicalSLower lexicalTLower varidNewt lexicalULower lexicalVLower lexicalWLower lexicalXLower lexicalYLower lexicalZLower annotation fixpoint =
	  EOPVaridNew          annotation lexicalEndOfParse
		-- ^ We're provided with a certification that no more characters will
		-- follow in whatever we're parsing, and ‘new’ is not excluded.
	| InnerSansAscVaridNew annotation varidInnerSansAscSmallUnderscore (list varidInner)
		-- ^ Characters we know aren't excluded are valid characters.
	| ALowerVaridNew       annotation lexicalALower                    (list varidInner)
		-- ^ ‘newa’ starts no excluded identifier.
	| BLowerVaridNew       annotation lexicalBLower                    (list varidInner)
		-- ^ ‘newb’ starts no excluded identifier.
	| CLowerVaridNew       annotation lexicalCLower                    (list varidInner)
		-- ^ ‘newc’ starts no excluded identifier.
	| DLowerVaridNew       annotation lexicalDLower                    (list varidInner)
		-- ^ ‘newd’ starts no excluded identifier.
	| ELowerVaridNew       annotation lexicalELower                    (list varidInner)
		-- ^ ‘newe’ starts no excluded identifier.
	| FLowerVaridNew       annotation lexicalFLower                    (list varidInner)
		-- ^ ‘newf’ starts no excluded identifier.
	| GLowerVaridNew       annotation lexicalGLower                    (list varidInner)
		-- ^ ‘newg’ starts no excluded identifier.
	| HLowerVaridNew       annotation lexicalHLower                    (list varidInner)
		-- ^ ‘newh’ starts no excluded identifier.
	| ILowerVaridNew       annotation lexicalILower                    (list varidInner)
		-- ^ ‘newi’ starts no excluded identifier.
	| JLowerVaridNew       annotation lexicalJLower                    (list varidInner)
		-- ^ ‘newj’ starts no excluded identifier.
	| KLowerVaridNew       annotation lexicalKLower                    (list varidInner)
		-- ^ ‘newk’ starts no excluded identifier.
	| LLowerVaridNew       annotation lexicalLLower                    (list varidInner)
		-- ^ ‘newl’ starts no excluded identifier.
	| MLowerVaridNew       annotation lexicalMLower                    (list varidInner)
		-- ^ ‘newm’ starts no excluded identifier.
	| NLowerVaridNew       annotation lexicalNLower                    (list varidInner)
		-- ^ ‘newn’ starts no excluded identifier.
	| OLowerVaridNew       annotation lexicalOLower                    (list varidInner)
		-- ^ ‘newo’ starts no excluded identifier.
	| PLowerVaridNew       annotation lexicalPLower                    (list varidInner)
		-- ^ ‘newp’ starts no excluded identifier.
	| QLowerVaridNew       annotation lexicalQLower                    (list varidInner)
		-- ^ ‘newq’ starts no excluded identifier.
	| RLowerVaridNew       annotation lexicalRLower                    (list varidInner)
		-- ^ ‘newr’ starts no excluded identifier.
	| SLowerVaridNew       annotation lexicalSLower                    (list varidInner)
		-- ^ ‘news’ starts no excluded identifier.
	| TLowerVaridNew       annotation lexicalTLower                    varidNewt
		-- ^ ‘newt’ starts 1 excluded identifier.
	| ULowerVaridNew       annotation lexicalULower                    (list varidInner)
		-- ^ ‘newu’ starts no excluded identifier.
	| VLowerVaridNew       annotation lexicalVLower                    (list varidInner)
		-- ^ ‘newv’ starts no excluded identifier.
	| WLowerVaridNew       annotation lexicalWLower                    (list varidInner)
		-- ^ ‘neww’ starts no excluded identifier.
	| XLowerVaridNew       annotation lexicalXLower                    (list varidInner)
		-- ^ ‘newx’ starts no excluded identifier.
	| YLowerVaridNew       annotation lexicalYLower                    (list varidInner)
		-- ^ ‘newy’ starts no excluded identifier.
	| ZLowerVaridNew       annotation lexicalZLower                    (list varidInner)
		-- ^ ‘newz’ starts no excluded identifier.

-- | A non-symbolic variable (lowercase-style) identifier name, excluding
-- reserved names, after an ‘the’ was parsed from the beginning.
data VaridTheBase list lexicalEndOfParse varidInnerSansAscSmallUnderscore varidInner lexicalALower lexicalBLower lexicalCLower lexicalDLower lexicalELower lexicalFLower lexicalGLower lexicalHLower lexicalILower lexicalJLower lexicalKLower lexicalLLower lexicalMLower lexicalNLower lexicalOLower lexicalPLower lexicalQLower lexicalRLower lexicalSLower lexicalTLower lexicalULower lexicalVLower lexicalWLower lexicalXLower lexicalYLower lexicalZLower annotation fixpoint =
	  EOPVaridThe          annotation lexicalEndOfParse
		-- ^ We're provided with a certification that no more characters will
		-- follow in whatever we're parsing, and ‘the’ is not excluded.
	| InnerSansAscVaridThe annotation varidInnerSansAscSmallUnderscore (list varidInner)
		-- ^ Characters we know aren't excluded are valid characters.
	| ALowerVaridThe       annotation lexicalALower                    (list varidInner)
		-- ^ ‘thea’ starts no excluded identifier.
	| BLowerVaridThe       annotation lexicalBLower                    (list varidInner)
		-- ^ ‘theb’ starts no excluded identifier.
	| CLowerVaridThe       annotation lexicalCLower                    (list varidInner)
		-- ^ ‘thec’ starts no excluded identifier.
	| DLowerVaridThe       annotation lexicalDLower                    (list varidInner)
		-- ^ ‘thed’ starts no excluded identifier.
	| ELowerVaridThe       annotation lexicalELower                    (list varidInner)
		-- ^ ‘thee’ starts no excluded identifier.
	| FLowerVaridThe       annotation lexicalFLower                    (list varidInner)
		-- ^ ‘thef’ starts no excluded identifier.
	| GLowerVaridThe       annotation lexicalGLower                    (list varidInner)
		-- ^ ‘theg’ starts no excluded identifier.
	| HLowerVaridThe       annotation lexicalHLower                    (list varidInner)
		-- ^ ‘theh’ starts no excluded identifier.
	| ILowerVaridThe       annotation lexicalILower                    (list varidInner)
		-- ^ ‘thei’ starts no excluded identifier.
	| JLowerVaridThe       annotation lexicalJLower                    (list varidInner)
		-- ^ ‘thej’ starts no excluded identifier.
	| KLowerVaridThe       annotation lexicalKLower                    (list varidInner)
		-- ^ ‘thek’ starts no excluded identifier.
	| LLowerVaridThe       annotation lexicalLLower                    (list varidInner)
		-- ^ ‘thel’ starts no excluded identifier.
	| MLowerVaridThe       annotation lexicalMLower                    (list varidInner)
		-- ^ ‘them’ starts no excluded identifier.
	| NLowerVaridThe       annotation lexicalNLower                    varidInner        (list varidInner)
		-- ^ ‘then’ alone is excluded.  Additional characters is valid.
	| OLowerVaridThe       annotation lexicalOLower                    (list varidInner)
		-- ^ ‘theo’ starts no excluded identifier.
	| PLowerVaridThe       annotation lexicalPLower                    (list varidInner)
		-- ^ ‘thep’ starts no excluded identifier.
	| QLowerVaridThe       annotation lexicalQLower                    (list varidInner)
		-- ^ ‘theq’ starts no excluded identifier.
	| RLowerVaridThe       annotation lexicalRLower                    (list varidInner)
		-- ^ ‘ther’ starts no excluded identifier.
	| SLowerVaridThe       annotation lexicalSLower                    (list varidInner)
		-- ^ ‘thes’ starts no excluded identifier.
	| TLowerVaridThe       annotation lexicalTLower                    (list varidInner)
		-- ^ ‘thet’ starts no excluded identifier.
	| ULowerVaridThe       annotation lexicalULower                    (list varidInner)
		-- ^ ‘theu’ starts no excluded identifier.
	| VLowerVaridThe       annotation lexicalVLower                    (list varidInner)
		-- ^ ‘thev’ starts no excluded identifier.
	| WLowerVaridThe       annotation lexicalWLower                    (list varidInner)
		-- ^ ‘thew’ starts no excluded identifier.
	| XLowerVaridThe       annotation lexicalXLower                    (list varidInner)
		-- ^ ‘thex’ starts no excluded identifier.
	| YLowerVaridThe       annotation lexicalYLower                    (list varidInner)
		-- ^ ‘they’ starts no excluded identifier.
	| ZLowerVaridThe       annotation lexicalZLower                    (list varidInner)
		-- ^ ‘thez’ starts no excluded identifier.

-- | A non-symbolic variable (lowercase-style) identifier name, excluding
-- reserved names, after an ‘typ’ was parsed from typ beginning.
data VaridTypBase list lexicalEndOfParse varidInnerSansAscSmallUnderscore varidInner lexicalALower lexicalBLower lexicalCLower lexicalDLower lexicalELower lexicalFLower lexicalGLower lexicalHLower lexicalILower lexicalJLower lexicalKLower lexicalLLower lexicalMLower lexicalNLower lexicalOLower lexicalPLower lexicalQLower lexicalRLower lexicalSLower lexicalTLower lexicalULower lexicalVLower lexicalWLower lexicalXLower lexicalYLower lexicalZLower annotation fixpoint =
	  EOPVaridTyp          annotation lexicalEndOfParse
		-- ^ We're provided with a certification that no more characters will
		-- follow in whatever we're parsing, and ‘typ’ is not excluded.
	| InnerSansAscVaridTyp annotation varidInnerSansAscSmallUnderscore (list varidInner)
		-- ^ Characters we know aren't excluded are valid characters.
	| ALowerVaridTyp       annotation lexicalALower                    (list varidInner)
		-- ^ ‘typa’ starts no excluded identifier.
	| BLowerVaridTyp       annotation lexicalBLower                    (list varidInner)
		-- ^ ‘typb’ starts no excluded identifier.
	| CLowerVaridTyp       annotation lexicalCLower                    (list varidInner)
		-- ^ ‘typc’ starts no excluded identifier.
	| DLowerVaridTyp       annotation lexicalDLower                    (list varidInner)
		-- ^ ‘typd’ starts no excluded identifier.
	| ELowerVaridTyp       annotation lexicalELower                    varidInner        (list varidInner)
		-- ^ ‘type’ alone is excluded.  Additional characters is valid.
	| FLowerVaridTyp       annotation lexicalFLower                    (list varidInner)
		-- ^ ‘typf’ starts no excluded identifier.
	| GLowerVaridTyp       annotation lexicalGLower                    (list varidInner)
		-- ^ ‘typg’ starts no excluded identifier.
	| HLowerVaridTyp       annotation lexicalHLower                    (list varidInner)
		-- ^ ‘typh’ starts no excluded identifier.
	| ILowerVaridTyp       annotation lexicalILower                    (list varidInner)
		-- ^ ‘typi’ starts no excluded identifier.
	| JLowerVaridTyp       annotation lexicalJLower                    (list varidInner)
		-- ^ ‘typj’ starts no excluded identifier.
	| KLowerVaridTyp       annotation lexicalKLower                    (list varidInner)
		-- ^ ‘typk’ starts no excluded identifier.
	| LLowerVaridTyp       annotation lexicalLLower                    (list varidInner)
		-- ^ ‘typl’ starts no excluded identifier.
	| MLowerVaridTyp       annotation lexicalMLower                    (list varidInner)
		-- ^ ‘typm’ starts no excluded identifier.
	| NLowerVaridTyp       annotation lexicalNLower                    (list varidInner)
		-- ^ ‘typn’ starts no excluded identifier.
	| OLowerVaridTyp       annotation lexicalOLower                    (list varidInner)
		-- ^ ‘typo’ starts no excluded identifier.
	| PLowerVaridTyp       annotation lexicalPLower                    (list varidInner)
		-- ^ ‘typp’ starts no excluded identifier.
	| QLowerVaridTyp       annotation lexicalQLower                    (list varidInner)
		-- ^ ‘typq’ starts no excluded identifier.
	| RLowerVaridTyp       annotation lexicalRLower                    (list varidInner)
		-- ^ ‘typr’ starts no excluded identifier.
	| SLowerVaridTyp       annotation lexicalSLower                    (list varidInner)
		-- ^ ‘typs’ starts no excluded identifier.
	| TLowerVaridTyp       annotation lexicalTLower                    (list varidInner)
		-- ^ ‘typt’ starts no excluded identifier.
	| ULowerVaridTyp       annotation lexicalULower                    (list varidInner)
		-- ^ ‘typu’ starts no excluded identifier.
	| VLowerVaridTyp       annotation lexicalVLower                    (list varidInner)
		-- ^ ‘typv’ starts no excluded identifier.
	| WLowerVaridTyp       annotation lexicalWLower                    (list varidInner)
		-- ^ ‘typw’ starts no excluded identifier.
	| XLowerVaridTyp       annotation lexicalXLower                    (list varidInner)
		-- ^ ‘typx’ starts no excluded identifier.
	| YLowerVaridTyp       annotation lexicalYLower                    (list varidInner)
		-- ^ ‘typy’ starts no excluded identifier.
	| ZLowerVaridTyp       annotation lexicalZLower                    (list varidInner)
		-- ^ ‘typz’ starts no excluded identifier.

-- | A non-symbolic variable (lowercase-style) identifier name, excluding
-- reserved names, after an ‘whe’ was parsed from the beginning.
data VaridWheBase list lexicalEndOfParse varidInnerSansAscSmallUnderscore varidInner lexicalALower lexicalBLower lexicalCLower lexicalDLower lexicalELower lexicalFLower lexicalGLower lexicalHLower lexicalILower lexicalJLower lexicalKLower lexicalLLower lexicalMLower lexicalNLower lexicalOLower lexicalPLower lexicalQLower lexicalRLower varidWher lexicalSLower lexicalTLower lexicalULower lexicalVLower lexicalWLower lexicalXLower lexicalYLower lexicalZLower annotation fixpoint =
	  EOPVaridWhe          annotation lexicalEndOfParse
		-- ^ We're provided with a certification that no more characters will
		-- follow in whatever we're parsing, and ‘whe’ is not excluded.
	| InnerSansAscVaridWhe annotation varidInnerSansAscSmallUnderscore (list varidInner)
		-- ^ Characters we know aren't excluded are valid characters.
	| ALowerVaridWhe       annotation lexicalALower                    (list varidInner)
		-- ^ ‘whea’ starts no excluded identifier.
	| BLowerVaridWhe       annotation lexicalBLower                    (list varidInner)
		-- ^ ‘wheb’ starts no excluded identifier.
	| CLowerVaridWhe       annotation lexicalCLower                    (list varidInner)
		-- ^ ‘whec’ starts no excluded identifier.
	| DLowerVaridWhe       annotation lexicalDLower                    (list varidInner)
		-- ^ ‘whed’ starts no excluded identifier.
	| ELowerVaridWhe       annotation lexicalELower                    (list varidInner)
		-- ^ ‘whee’ starts no excluded identifier.
	| FLowerVaridWhe       annotation lexicalFLower                    (list varidInner)
		-- ^ ‘whef’ starts no excluded identifier.
	| GLowerVaridWhe       annotation lexicalGLower                    (list varidInner)
		-- ^ ‘wheg’ starts no excluded identifier.
	| HLowerVaridWhe       annotation lexicalHLower                    (list varidInner)
		-- ^ ‘wheh’ starts no excluded identifier.
	| ILowerVaridWhe       annotation lexicalILower                    (list varidInner)
		-- ^ ‘whei’ starts no excluded identifier.
	| JLowerVaridWhe       annotation lexicalJLower                    (list varidInner)
		-- ^ ‘whej’ starts no excluded identifier.
	| KLowerVaridWhe       annotation lexicalKLower                    (list varidInner)
		-- ^ ‘whek’ starts no excluded identifier.
	| LLowerVaridWhe       annotation lexicalLLower                    (list varidInner)
		-- ^ ‘whel’ starts no excluded identifier.
	| MLowerVaridWhe       annotation lexicalMLower                    (list varidInner)
		-- ^ ‘whem’ starts no excluded identifier.
	| NLowerVaridWhe       annotation lexicalNLower                    (list varidInner)
		-- ^ ‘when’ starts no excluded identifier.
	| OLowerVaridWhe       annotation lexicalOLower                    (list varidInner)
		-- ^ ‘wheo’ starts no excluded identifier.
	| PLowerVaridWhe       annotation lexicalPLower                    (list varidInner)
		-- ^ ‘whep’ starts no excluded identifier.
	| QLowerVaridWhe       annotation lexicalQLower                    (list varidInner)
		-- ^ ‘wheq’ starts no excluded identifier.
	| RLowerVaridWhe       annotation lexicalRLower                    varidWher
		-- ^ ‘wher’ starts 1 excluded identifier.
	| SLowerVaridWhe       annotation lexicalSLower                    (list varidInner)
		-- ^ ‘whes’ starts no excluded identifier.
	| TLowerVaridWhe       annotation lexicalTLower                    (list varidInner)
		-- ^ ‘whet’ starts no excluded identifier.
	| ULowerVaridWhe       annotation lexicalULower                    (list varidInner)
		-- ^ ‘wheu’ starts no excluded identifier.
	| VLowerVaridWhe       annotation lexicalVLower                    (list varidInner)
		-- ^ ‘whev’ starts no excluded identifier.
	| WLowerVaridWhe       annotation lexicalWLower                    (list varidInner)
		-- ^ ‘whew’ starts no excluded identifier.
	| XLowerVaridWhe       annotation lexicalXLower                    (list varidInner)
		-- ^ ‘whex’ starts no excluded identifier.
	| YLowerVaridWhe       annotation lexicalYLower                    (list varidInner)
		-- ^ ‘whey’ starts no excluded identifier.
	| ZLowerVaridWhe       annotation lexicalZLower                    (list varidInner)
		-- ^ ‘whez’ starts no excluded identifier.

-- | A non-symbolic variable (lowercase-style) identifier name, excluding
-- reserved names, after an ‘clas’ was parsed from the beginning.
data VaridClasBase list lexicalEndOfParse varidInnerSansAscSmallUnderscore varidInner lexicalALower lexicalBLower lexicalCLower lexicalDLower lexicalELower lexicalFLower lexicalGLower lexicalHLower lexicalILower lexicalJLower lexicalKLower lexicalLLower lexicalMLower lexicalNLower lexicalOLower lexicalPLower lexicalQLower lexicalRLower lexicalSLower lexicalTLower lexicalULower lexicalVLower lexicalWLower lexicalXLower lexicalYLower lexicalZLower annotation fixpoint =
	  EOPVaridClas          annotation lexicalEndOfParse
		-- ^ We're provided with a certification that no more characters will
		-- follow in whatever we're parsing, and ‘clas’ is not excluded.
	| InnerSansAscVaridClas annotation varidInnerSansAscSmallUnderscore (list varidInner)
		-- ^ Characters we know aren't excluded are valid characters.
	| ALowerVaridClas       annotation lexicalALower                    (list varidInner)
		-- ^ ‘clasa’ starts no excluded identifier.
	| BLowerVaridClas       annotation lexicalBLower                    (list varidInner)
		-- ^ ‘clasb’ starts no excluded identifier.
	| CLowerVaridClas       annotation lexicalCLower                    (list varidInner)
		-- ^ ‘clasc’ starts no excluded identifier.
	| DLowerVaridClas       annotation lexicalDLower                    (list varidInner)
		-- ^ ‘clasd’ starts no excluded identifier.
	| ELowerVaridClas       annotation lexicalELower                    (list varidInner)
		-- ^ ‘clase’ starts no excluded identifier.
	| FLowerVaridClas       annotation lexicalFLower                    (list varidInner)
		-- ^ ‘clasf’ starts no excluded identifier.
	| GLowerVaridClas       annotation lexicalGLower                    (list varidInner)
		-- ^ ‘clasg’ starts no excluded identifier.
	| HLowerVaridClas       annotation lexicalHLower                    (list varidInner)
		-- ^ ‘clash’ starts no excluded identifier.
	| ILowerVaridClas       annotation lexicalILower                    (list varidInner)
		-- ^ ‘clasi’ starts no excluded identifier.
	| JLowerVaridClas       annotation lexicalJLower                    (list varidInner)
		-- ^ ‘clasj’ starts no excluded identifier.
	| KLowerVaridClas       annotation lexicalKLower                    (list varidInner)
		-- ^ ‘clask’ starts no excluded identifier.
	| LLowerVaridClas       annotation lexicalLLower                    (list varidInner)
		-- ^ ‘clasl’ starts no excluded identifier.
	| MLowerVaridClas       annotation lexicalMLower                    (list varidInner)
		-- ^ ‘clasm’ starts no excluded identifier.
	| NLowerVaridClas       annotation lexicalNLower                    (list varidInner)
		-- ^ ‘clasn’ starts no excluded identifier.
	| OLowerVaridClas       annotation lexicalOLower                    (list varidInner)
		-- ^ ‘claso’ starts no excluded identifier.
	| PLowerVaridClas       annotation lexicalPLower                    (list varidInner)
		-- ^ ‘clasp’ starts no excluded identifier.
	| QLowerVaridClas       annotation lexicalQLower                    (list varidInner)
		-- ^ ‘clasq’ starts no excluded identifier.
	| RLowerVaridClas       annotation lexicalRLower                    (list varidInner)
		-- ^ ‘clasr’ starts no excluded identifier.
	| SLowerVaridClas       annotation lexicalSLower                    varidInner        (list varidInner)
		-- ^ ‘class’ alone is excluded.  Additional characters is valid.
	| TLowerVaridClas       annotation lexicalTLower                    (list varidInner)
		-- ^ ‘clast’ starts no excluded identifier.
	| ULowerVaridClas       annotation lexicalULower                    (list varidInner)
		-- ^ ‘clasu’ starts no excluded identifier.
	| VLowerVaridClas       annotation lexicalVLower                    (list varidInner)
		-- ^ ‘clasv’ starts no excluded identifier.
	| WLowerVaridClas       annotation lexicalWLower                    (list varidInner)
		-- ^ ‘clasw’ starts no excluded identifier.
	| XLowerVaridClas       annotation lexicalXLower                    (list varidInner)
		-- ^ ‘clasx’ starts no excluded identifier.
	| YLowerVaridClas       annotation lexicalYLower                    (list varidInner)
		-- ^ ‘clasy’ starts no excluded identifier.
	| ZLowerVaridClas       annotation lexicalZLower                    (list varidInner)
		-- ^ ‘clasz’ starts no excluded identifier.

-- | A non-symbolic variable (lowercase-style) identifier name, excluding
-- reserved names, after an ‘defa’ was parsed from the beginning.
data VaridDefaBase list lexicalEndOfParse varidInnerSansAscSmallUnderscore varidInner lexicalALower lexicalBLower lexicalCLower lexicalDLower lexicalELower lexicalFLower lexicalGLower lexicalHLower lexicalILower lexicalJLower lexicalKLower lexicalLLower lexicalMLower lexicalNLower lexicalOLower lexicalPLower lexicalQLower lexicalRLower lexicalSLower lexicalTLower lexicalULower varidDefau lexicalVLower lexicalWLower lexicalXLower lexicalYLower lexicalZLower annotation fixpoint =
	  EOPVaridDefa          annotation lexicalEndOfParse
		-- ^ We're provided with a certification that no more characters will
		-- follow in whatever we're parsing, and ‘defa’ is not excluded.
	| InnerSansAscVaridDefa annotation varidInnerSansAscSmallUnderscore (list varidInner)
		-- ^ Characters we know aren't excluded are valid characters.
	| ALowerVaridDefa       annotation lexicalALower                    (list varidInner)
		-- ^ ‘defaa’ starts no excluded identifier.
	| BLowerVaridDefa       annotation lexicalBLower                    (list varidInner)
		-- ^ ‘defab’ starts no excluded identifier.
	| CLowerVaridDefa       annotation lexicalCLower                    (list varidInner)
		-- ^ ‘defac’ starts no excluded identifier.
	| DLowerVaridDefa       annotation lexicalDLower                    (list varidInner)
		-- ^ ‘defad’ starts no excluded identifier.
	| ELowerVaridDefa       annotation lexicalELower                    (list varidInner)
		-- ^ ‘defae’ starts no excluded identifier.
	| FLowerVaridDefa       annotation lexicalFLower                    (list varidInner)
		-- ^ ‘defaf’ starts no excluded identifier.
	| GLowerVaridDefa       annotation lexicalGLower                    (list varidInner)
		-- ^ ‘defag’ starts no excluded identifier.
	| HLowerVaridDefa       annotation lexicalHLower                    (list varidInner)
		-- ^ ‘defah’ starts no excluded identifier.
	| ILowerVaridDefa       annotation lexicalILower                    (list varidInner)
		-- ^ ‘defai’ starts no excluded identifier.
	| JLowerVaridDefa       annotation lexicalJLower                    (list varidInner)
		-- ^ ‘defaj’ starts no excluded identifier.
	| KLowerVaridDefa       annotation lexicalKLower                    (list varidInner)
		-- ^ ‘defak’ starts no excluded identifier.
	| LLowerVaridDefa       annotation lexicalLLower                    (list varidInner)
		-- ^ ‘defal’ starts no excluded identifier.
	| MLowerVaridDefa       annotation lexicalMLower                    (list varidInner)
		-- ^ ‘defam’ starts no excluded identifier.
	| NLowerVaridDefa       annotation lexicalNLower                    (list varidInner)
		-- ^ ‘defan’ starts no excluded identifier.
	| OLowerVaridDefa       annotation lexicalOLower                    (list varidInner)
		-- ^ ‘defao’ starts no excluded identifier.
	| PLowerVaridDefa       annotation lexicalPLower                    (list varidInner)
		-- ^ ‘defap’ starts no excluded identifier.
	| QLowerVaridDefa       annotation lexicalQLower                    (list varidInner)
		-- ^ ‘defaq’ starts no excluded identifier.
	| RLowerVaridDefa       annotation lexicalRLower                    (list varidInner)
		-- ^ ‘defar’ starts no excluded identifier.
	| SLowerVaridDefa       annotation lexicalSLower                    (list varidInner)
		-- ^ ‘defas’ starts no excluded identifier.
	| TLowerVaridDefa       annotation lexicalTLower                    (list varidInner)
		-- ^ ‘defat’ starts no excluded identifier.
	| ULowerVaridDefa       annotation lexicalULower                    varidDefau
		-- ^ ‘defau’ starts 1 excluded identifier.
	| VLowerVaridDefa       annotation lexicalVLower                    (list varidInner)
		-- ^ ‘defav’ starts no excluded identifier.
	| WLowerVaridDefa       annotation lexicalWLower                    (list varidInner)
		-- ^ ‘defaw’ starts no excluded identifier.
	| XLowerVaridDefa       annotation lexicalXLower                    (list varidInner)
		-- ^ ‘defax’ starts no excluded identifier.
	| YLowerVaridDefa       annotation lexicalYLower                    (list varidInner)
		-- ^ ‘defay’ starts no excluded identifier.
	| ZLowerVaridDefa       annotation lexicalZLower                    (list varidInner)
		-- ^ ‘defaz’ starts no excluded identifier.

-- | A non-symbolic variable (lowercase-style) identifier name, excluding
-- reserved names, after an ‘deri’ was parsed from the beginning.
data VaridDeriBase list lexicalEndOfParse varidInnerSansAscSmallUnderscore varidInner lexicalALower lexicalBLower lexicalCLower lexicalDLower lexicalELower lexicalFLower lexicalGLower lexicalHLower lexicalILower lexicalJLower lexicalKLower lexicalLLower lexicalMLower lexicalNLower lexicalOLower lexicalPLower lexicalQLower lexicalRLower lexicalSLower lexicalTLower lexicalULower lexicalVLower varidDeriv lexicalWLower lexicalXLower lexicalYLower lexicalZLower annotation fixpoint =
	  EOPVaridDeri          annotation lexicalEndOfParse
		-- ^ We're provided with a certification that no more characters will
		-- follow in whatever we're parsing, and ‘deri’ is not excluded.
	| InnerSansAscVaridDeri annotation varidInnerSansAscSmallUnderscore (list varidInner)
		-- ^ Characters we know aren't excluded are valid characters.
	| ALowerVaridDeri       annotation lexicalALower                    (list varidInner)
		-- ^ ‘deria’ starts no excluded identifier.
	| BLowerVaridDeri       annotation lexicalBLower                    (list varidInner)
		-- ^ ‘derib’ starts no excluded identifier.
	| CLowerVaridDeri       annotation lexicalCLower                    (list varidInner)
		-- ^ ‘deric’ starts no excluded identifier.
	| DLowerVaridDeri       annotation lexicalDLower                    (list varidInner)
		-- ^ ‘derid’ starts no excluded identifier.
	| ELowerVaridDeri       annotation lexicalELower                    (list varidInner)
		-- ^ ‘derie’ starts no excluded identifier.
	| FLowerVaridDeri       annotation lexicalFLower                    (list varidInner)
		-- ^ ‘derif’ starts no excluded identifier.
	| GLowerVaridDeri       annotation lexicalGLower                    (list varidInner)
		-- ^ ‘derig’ starts no excluded identifier.
	| HLowerVaridDeri       annotation lexicalHLower                    (list varidInner)
		-- ^ ‘derih’ starts no excluded identifier.
	| ILowerVaridDeri       annotation lexicalILower                    (list varidInner)
		-- ^ ‘derii’ starts no excluded identifier.
	| JLowerVaridDeri       annotation lexicalJLower                    (list varidInner)
		-- ^ ‘derij’ starts no excluded identifier.
	| KLowerVaridDeri       annotation lexicalKLower                    (list varidInner)
		-- ^ ‘derik’ starts no excluded identifier.
	| LLowerVaridDeri       annotation lexicalLLower                    (list varidInner)
		-- ^ ‘deril’ starts no excluded identifier.
	| MLowerVaridDeri       annotation lexicalMLower                    (list varidInner)
		-- ^ ‘derim’ starts no excluded identifier.
	| NLowerVaridDeri       annotation lexicalNLower                    (list varidInner)
		-- ^ ‘derin’ starts no excluded identifier.
	| OLowerVaridDeri       annotation lexicalOLower                    (list varidInner)
		-- ^ ‘derio’ starts no excluded identifier.
	| PLowerVaridDeri       annotation lexicalPLower                    (list varidInner)
		-- ^ ‘derip’ starts no excluded identifier.
	| QLowerVaridDeri       annotation lexicalQLower                    (list varidInner)
		-- ^ ‘deriq’ starts no excluded identifier.
	| RLowerVaridDeri       annotation lexicalRLower                    (list varidInner)
		-- ^ ‘derir’ starts no excluded identifier.
	| SLowerVaridDeri       annotation lexicalSLower                    (list varidInner)
		-- ^ ‘deris’ starts no excluded identifier.
	| TLowerVaridDeri       annotation lexicalTLower                    (list varidInner)
		-- ^ ‘derit’ starts no excluded identifier.
	| ULowerVaridDeri       annotation lexicalULower                    (list varidInner)
		-- ^ ‘deriu’ starts 1 excluded identifier.
	| VLowerVaridDeri       annotation lexicalVLower                    varidDeriv
		-- ^ ‘deriv’ starts no excluded identifier.
	| WLowerVaridDeri       annotation lexicalWLower                    (list varidInner)
		-- ^ ‘deriw’ starts no excluded identifier.
	| XLowerVaridDeri       annotation lexicalXLower                    (list varidInner)
		-- ^ ‘derix’ starts no excluded identifier.
	| YLowerVaridDeri       annotation lexicalYLower                    (list varidInner)
		-- ^ ‘deriy’ starts no excluded identifier.
	| ZLowerVaridDeri       annotation lexicalZLower                    (list varidInner)
		-- ^ ‘deriz’ starts no excluded identifier.

-- | A non-symbolic variable (lowercase-style) identifier name, excluding
-- reserved names, after an ‘fore’ was parsed from the beginning.
data VaridForeBase list lexicalEndOfParse varidInnerSansAscSmallUnderscore varidInner lexicalALower lexicalBLower lexicalCLower lexicalDLower lexicalELower lexicalFLower lexicalGLower lexicalHLower lexicalILower varidForei lexicalJLower lexicalKLower lexicalLLower lexicalMLower lexicalNLower lexicalOLower lexicalPLower lexicalQLower lexicalRLower lexicalSLower lexicalTLower lexicalULower lexicalVLower lexicalWLower lexicalXLower lexicalYLower lexicalZLower annotation fixpoint =
	  EOPVaridFore          annotation lexicalEndOfParse
		-- ^ We're provided with a certification that no more characters will
		-- follow in whatever we're parsing, and ‘fore’ is not excluded.
	| InnerSansAscVaridFore annotation varidInnerSansAscSmallUnderscore (list varidInner)
		-- ^ Characters we know aren't excluded are valid characters.
	| ALowerVaridFore       annotation lexicalALower                    (list varidInner)
		-- ^ ‘forea’ starts no excluded identifier.
	| BLowerVaridFore       annotation lexicalBLower                    (list varidInner)
		-- ^ ‘foreb’ starts no excluded identifier.
	| CLowerVaridFore       annotation lexicalCLower                    (list varidInner)
		-- ^ ‘forec’ starts no excluded identifier.
	| DLowerVaridFore       annotation lexicalDLower                    (list varidInner)
		-- ^ ‘fored’ starts no excluded identifier.
	| ELowerVaridFore       annotation lexicalELower                    (list varidInner)
		-- ^ ‘foree’ starts no excluded identifier.
	| FLowerVaridFore       annotation lexicalFLower                    (list varidInner)
		-- ^ ‘foref’ starts no excluded identifier.
	| GLowerVaridFore       annotation lexicalGLower                    (list varidInner)
		-- ^ ‘foreg’ starts no excluded identifier.
	| HLowerVaridFore       annotation lexicalHLower                    (list varidInner)
		-- ^ ‘foreh’ starts no excluded identifier.
	| ILowerVaridFore       annotation lexicalILower                    varidForei
		-- ^ ‘forei’ starts 1 excluded identifiers.
	| JLowerVaridFore       annotation lexicalJLower                    (list varidInner)
		-- ^ ‘forej’ starts no excluded identifier.
	| KLowerVaridFore       annotation lexicalKLower                    (list varidInner)
		-- ^ ‘forek’ starts no excluded identifier.
	| LLowerVaridFore       annotation lexicalLLower                    (list varidInner)
		-- ^ ‘forel’ starts no excluded identifier.
	| MLowerVaridFore       annotation lexicalMLower                    (list varidInner)
		-- ^ ‘forem’ starts no excluded identifier.
	| NLowerVaridFore       annotation lexicalNLower                    (list varidInner)
		-- ^ ‘foren’ starts no excluded identifier.
	| OLowerVaridFore       annotation lexicalOLower                    (list varidInner)
		-- ^ ‘foreo’ starts no excluded identifier.
	| PLowerVaridFore       annotation lexicalPLower                    (list varidInner)
		-- ^ ‘forep’ starts no excluded identifier.
	| QLowerVaridFore       annotation lexicalQLower                    (list varidInner)
		-- ^ ‘foreq’ starts no excluded identifier.
	| RLowerVaridFore       annotation lexicalRLower                    (list varidInner)
		-- ^ ‘forer’ starts no excluded identifier.
	| SLowerVaridFore       annotation lexicalSLower                    (list varidInner)
		-- ^ ‘fores’ starts no excluded identifier.
	| TLowerVaridFore       annotation lexicalTLower                    (list varidInner)
		-- ^ ‘foret’ starts no excluded identifier.
	| ULowerVaridFore       annotation lexicalULower                    (list varidInner)
		-- ^ ‘foreu’ starts no excluded identifier.
	| VLowerVaridFore       annotation lexicalVLower                    (list varidInner)
		-- ^ ‘forev’ starts no excluded identifier.
	| WLowerVaridFore       annotation lexicalWLower                    (list varidInner)
		-- ^ ‘forew’ starts no excluded identifier.
	| XLowerVaridFore       annotation lexicalXLower                    (list varidInner)
		-- ^ ‘forex’ starts no excluded identifier.
	| YLowerVaridFore       annotation lexicalYLower                    (list varidInner)
		-- ^ ‘forey’ starts no excluded identifier.
	| ZLowerVaridFore       annotation lexicalZLower                    (list varidInner)
		-- ^ ‘forez’ starts no excluded identifier.

-- | A non-symbolic variable (lowercase-style) identifier name, excluding
-- reserved names, after an ‘impo’ was parsed from the beginning.
data VaridImpoBase list lexicalEndOfParse varidInnerSansAscSmallUnderscore varidInner lexicalALower lexicalBLower lexicalCLower lexicalDLower lexicalELower lexicalFLower lexicalGLower lexicalHLower lexicalILower lexicalJLower lexicalKLower lexicalLLower lexicalMLower lexicalNLower lexicalOLower lexicalPLower lexicalQLower lexicalRLower varidImpor lexicalSLower lexicalTLower lexicalULower lexicalVLower lexicalWLower lexicalXLower lexicalYLower lexicalZLower annotation fixpoint =
	  EOPVaridImpo          annotation lexicalEndOfParse
		-- ^ We're provided with a certification that no more characters will
		-- follow in whatever we're parsing, and ‘impo’ is not excluded.
	| InnerSansAscVaridImpo annotation varidInnerSansAscSmallUnderscore (list varidInner)
		-- ^ Characters we know aren't excluded are valid characters.
	| ALowerVaridImpo       annotation lexicalALower                    (list varidInner)
		-- ^ ‘impoa’ starts no excluded identifier.
	| BLowerVaridImpo       annotation lexicalBLower                    (list varidInner)
		-- ^ ‘impob’ starts no excluded identifier.
	| CLowerVaridImpo       annotation lexicalCLower                    (list varidInner)
		-- ^ ‘impoc’ starts no excluded identifier.
	| DLowerVaridImpo       annotation lexicalDLower                    (list varidInner)
		-- ^ ‘impod’ starts no excluded identifier.
	| ELowerVaridImpo       annotation lexicalELower                    (list varidInner)
		-- ^ ‘impoe’ starts no excluded identifier.
	| FLowerVaridImpo       annotation lexicalFLower                    (list varidInner)
		-- ^ ‘impof’ starts no excluded identifier.
	| GLowerVaridImpo       annotation lexicalGLower                    (list varidInner)
		-- ^ ‘impog’ starts no excluded identifier.
	| HLowerVaridImpo       annotation lexicalHLower                    (list varidInner)
		-- ^ ‘impoh’ starts no excluded identifier.
	| ILowerVaridImpo       annotation lexicalILower                    (list varidInner)
		-- ^ ‘impoi’ starts no excluded identifier.
	| JLowerVaridImpo       annotation lexicalJLower                    (list varidInner)
		-- ^ ‘impoj’ starts no excluded identifier.
	| KLowerVaridImpo       annotation lexicalKLower                    (list varidInner)
		-- ^ ‘impok’ starts no excluded identifier.
	| LLowerVaridImpo       annotation lexicalLLower                    (list varidInner)
		-- ^ ‘impol’ starts no excluded identifier.
	| MLowerVaridImpo       annotation lexicalMLower                    (list varidInner)
		-- ^ ‘impom’ starts no excluded identifier.
	| NLowerVaridImpo       annotation lexicalNLower                    (list varidInner)
		-- ^ ‘impon’ starts no excluded identifier.
	| OLowerVaridImpo       annotation lexicalOLower                    (list varidInner)
		-- ^ ‘impoo’ starts no excluded identifier.
	| PLowerVaridImpo       annotation lexicalPLower                    (list varidInner)
		-- ^ ‘impop’ starts no excluded identifier.
	| QLowerVaridImpo       annotation lexicalQLower                    (list varidInner)
		-- ^ ‘impoq’ starts no excluded identifier.
	| RLowerVaridImpo       annotation lexicalRLower                    varidImpor
		-- ^ ‘impor’ starts 1 excluded identifier.
	| SLowerVaridImpo       annotation lexicalSLower                    (list varidInner)
		-- ^ ‘impos’ starts no excluded identifier.
	| TLowerVaridImpo       annotation lexicalTLower                    (list varidInner)
		-- ^ ‘impot’ starts no excluded identifier.
	| ULowerVaridImpo       annotation lexicalULower                    (list varidInner)
		-- ^ ‘impou’ starts no excluded identifier.
	| VLowerVaridImpo       annotation lexicalVLower                    (list varidInner)
		-- ^ ‘impov’ starts no excluded identifier.
	| WLowerVaridImpo       annotation lexicalWLower                    (list varidInner)
		-- ^ ‘impow’ starts no excluded identifier.
	| XLowerVaridImpo       annotation lexicalXLower                    (list varidInner)
		-- ^ ‘impox’ starts no excluded identifier.
	| YLowerVaridImpo       annotation lexicalYLower                    (list varidInner)
		-- ^ ‘impoy’ starts no excluded identifier.
	| ZLowerVaridImpo       annotation lexicalZLower                    (list varidInner)
		-- ^ ‘impoz’ starts no excluded identifier.

-- | A non-symbolic variable (lowercase-style) identifier name, excluding
-- reserved names, after an ‘infi’ was parsed from the beginning.
data VaridInfiBase list lexicalEndOfParse varidInnerSansAscSmallUnderscore varidInner lexicalALower lexicalBLower lexicalCLower lexicalDLower lexicalELower lexicalFLower lexicalGLower lexicalHLower lexicalILower lexicalJLower lexicalKLower lexicalLLower lexicalMLower lexicalNLower lexicalOLower lexicalPLower lexicalQLower lexicalRLower lexicalSLower lexicalTLower lexicalULower lexicalVLower lexicalWLower lexicalXLower varidInfix lexicalYLower lexicalZLower annotation fixpoint =
	  EOPVaridInfi          annotation lexicalEndOfParse
		-- ^ We're provided with a certification that no more characters will
		-- follow in whatever we're parsing, and ‘infi’ is not excluded.
	| InnerSansAscVaridInfi annotation varidInnerSansAscSmallUnderscore (list varidInner)
		-- ^ Characters we know aren't excluded are valid characters.
	| ALowerVaridInfi       annotation lexicalALower                    (list varidInner)
		-- ^ ‘infia’ starts no excluded identifier.
	| BLowerVaridInfi       annotation lexicalBLower                    (list varidInner)
		-- ^ ‘infib’ starts no excluded identifier.
	| CLowerVaridInfi       annotation lexicalCLower                    (list varidInner)
		-- ^ ‘infic’ starts no excluded identifier.
	| DLowerVaridInfi       annotation lexicalDLower                    (list varidInner)
		-- ^ ‘infid’ starts no excluded identifier.
	| ELowerVaridInfi       annotation lexicalELower                    (list varidInner)
		-- ^ ‘infie’ starts no excluded identifier.
	| FLowerVaridInfi       annotation lexicalFLower                    (list varidInner)
		-- ^ ‘infif’ starts no excluded identifier.
	| GLowerVaridInfi       annotation lexicalGLower                    (list varidInner)
		-- ^ ‘infig’ starts no excluded identifier.
	| HLowerVaridInfi       annotation lexicalHLower                    (list varidInner)
		-- ^ ‘infih’ starts no excluded identifier.
	| ILowerVaridInfi       annotation lexicalILower                    (list varidInner)
		-- ^ ‘infii’ starts no excluded identifier.
	| JLowerVaridInfi       annotation lexicalJLower                    (list varidInner)
		-- ^ ‘infij’ starts no excluded identifier.
	| KLowerVaridInfi       annotation lexicalKLower                    (list varidInner)
		-- ^ ‘infik’ starts no excluded identifier.
	| LLowerVaridInfi       annotation lexicalLLower                    (list varidInner)
		-- ^ ‘infil’ starts no excluded identifier.
	| MLowerVaridInfi       annotation lexicalMLower                    (list varidInner)
		-- ^ ‘infim’ starts no excluded identifier.
	| NLowerVaridInfi       annotation lexicalNLower                    (list varidInner)
		-- ^ ‘infin’ starts no excluded identifier.
	| OLowerVaridInfi       annotation lexicalOLower                    (list varidInner)
		-- ^ ‘infio’ starts no excluded identifier.
	| PLowerVaridInfi       annotation lexicalPLower                    (list varidInner)
		-- ^ ‘infip’ starts no excluded identifier.
	| QLowerVaridInfi       annotation lexicalQLower                    (list varidInner)
		-- ^ ‘infiq’ starts no excluded identifier.
	| RLowerVaridInfi       annotation lexicalRLower                    (list varidInner)
		-- ^ ‘infir’ starts no excluded identifier.
	| SLowerVaridInfi       annotation lexicalSLower                    (list varidInner)
		-- ^ ‘infis’ starts no excluded identifier.
	| TLowerVaridInfi       annotation lexicalTLower                    (list varidInner)
		-- ^ ‘infit’ starts no excluded identifier.
	| ULowerVaridInfi       annotation lexicalULower                    (list varidInner)
		-- ^ ‘infiu’ starts no excluded identifier.
	| VLowerVaridInfi       annotation lexicalVLower                    (list varidInner)
		-- ^ ‘infiv’ starts no excluded identifier.
	| WLowerVaridInfi       annotation lexicalWLower                    (list varidInner)
		-- ^ ‘infiw’ starts no excluded identifier.
	| XLowerVaridInfi       annotation lexicalXLower                    varidInfix
		-- ^ ‘infix’ starts 2 excluded identifiers.
	| YLowerVaridInfi       annotation lexicalYLower                    (list varidInner)
		-- ^ ‘infiy’ starts no excluded identifier.
	| ZLowerVaridInfi       annotation lexicalZLower                    (list varidInner)
		-- ^ ‘infiz’ starts no excluded identifier.

-- | A non-symbolic variable (lowercase-style) identifier name, excluding
-- reserved names, after an ‘inst’ was parsed from the beginning.
data VaridInstBase list lexicalEndOfParse varidInnerSansAscSmallUnderscore varidInner lexicalALower varidInsta lexicalBLower lexicalCLower lexicalDLower lexicalELower lexicalFLower lexicalGLower lexicalHLower lexicalILower lexicalJLower lexicalKLower lexicalLLower lexicalMLower lexicalNLower lexicalOLower lexicalPLower lexicalQLower lexicalRLower lexicalSLower lexicalTLower lexicalULower lexicalVLower lexicalWLower lexicalXLower lexicalYLower lexicalZLower annotation fixpoint =
	  EOPVaridInst          annotation lexicalEndOfParse
		-- ^ We're provided with a certification that no more characters will
		-- follow in whatever we're parsing, and ‘inst’ is not excluded.
	| InnerSansAscVaridInst annotation varidInnerSansAscSmallUnderscore (list varidInner)
		-- ^ Characters we know aren't excluded are valid characters.
	| ALowerVaridInst       annotation lexicalALower                    varidInsta
		-- ^ ‘insta’ starts 1 excluded identifier.
	| BLowerVaridInst       annotation lexicalBLower                    (list varidInner)
		-- ^ ‘instb’ starts no excluded identifier.
	| CLowerVaridInst       annotation lexicalCLower                    (list varidInner)
		-- ^ ‘instc’ starts no excluded identifier.
	| DLowerVaridInst       annotation lexicalDLower                    (list varidInner)
		-- ^ ‘instd’ starts no excluded identifier.
	| ELowerVaridInst       annotation lexicalELower                    (list varidInner)
		-- ^ ‘inste’ starts no excluded identifier.
	| FLowerVaridInst       annotation lexicalFLower                    (list varidInner)
		-- ^ ‘instf’ starts no excluded identifier.
	| GLowerVaridInst       annotation lexicalGLower                    (list varidInner)
		-- ^ ‘instg’ starts no excluded identifier.
	| HLowerVaridInst       annotation lexicalHLower                    (list varidInner)
		-- ^ ‘insth’ starts no excluded identifier.
	| ILowerVaridInst       annotation lexicalILower                    (list varidInner)
		-- ^ ‘insti’ starts no excluded identifier.
	| JLowerVaridInst       annotation lexicalJLower                    (list varidInner)
		-- ^ ‘instj’ starts no excluded identifier.
	| KLowerVaridInst       annotation lexicalKLower                    (list varidInner)
		-- ^ ‘instk’ starts no excluded identifier.
	| LLowerVaridInst       annotation lexicalLLower                    (list varidInner)
		-- ^ ‘instl’ starts no excluded identifier.
	| MLowerVaridInst       annotation lexicalMLower                    (list varidInner)
		-- ^ ‘instm’ starts no excluded identifier.
	| NLowerVaridInst       annotation lexicalNLower                    (list varidInner)
		-- ^ ‘instn’ starts no excluded identifier.
	| OLowerVaridInst       annotation lexicalOLower                    (list varidInner)
		-- ^ ‘insto’ starts no excluded identifier.
	| PLowerVaridInst       annotation lexicalPLower                    (list varidInner)
		-- ^ ‘instp’ starts no excluded identifier.
	| QLowerVaridInst       annotation lexicalQLower                    (list varidInner)
		-- ^ ‘instq’ starts no excluded identifier.
	| RLowerVaridInst       annotation lexicalRLower                    (list varidInner)
		-- ^ ‘instr’ starts no excluded identifier.
	| SLowerVaridInst       annotation lexicalSLower                    (list varidInner)
		-- ^ ‘insts’ starts no excluded identifier.
	| TLowerVaridInst       annotation lexicalTLower                    (list varidInner)
		-- ^ ‘instt’ starts no excluded identifier.
	| ULowerVaridInst       annotation lexicalULower                    (list varidInner)
		-- ^ ‘instu’ starts no excluded identifier.
	| VLowerVaridInst       annotation lexicalVLower                    (list varidInner)
		-- ^ ‘instv’ starts no excluded identifier.
	| WLowerVaridInst       annotation lexicalWLower                    (list varidInner)
		-- ^ ‘instw’ starts no excluded identifier.
	| XLowerVaridInst       annotation lexicalXLower                    (list varidInner)
		-- ^ ‘instx’ starts no excluded identifier.
	| YLowerVaridInst       annotation lexicalYLower                    (list varidInner)
		-- ^ ‘insty’ starts no excluded identifier.
	| ZLowerVaridInst       annotation lexicalZLower                    (list varidInner)
		-- ^ ‘instz’ starts no excluded identifier.

-- | A non-symbolic variable (lowercase-style) identifier name, excluding
-- reserved names, after an ‘modu’ was parsed from the beginning.
data VaridModuBase list lexicalEndOfParse varidInnerSansAscSmallUnderscore varidInner lexicalALower lexicalBLower lexicalCLower lexicalDLower lexicalELower lexicalFLower lexicalGLower lexicalHLower lexicalILower lexicalJLower lexicalKLower lexicalLLower varidModul lexicalMLower lexicalNLower lexicalOLower lexicalPLower lexicalQLower lexicalRLower lexicalSLower lexicalTLower lexicalULower lexicalVLower lexicalWLower lexicalXLower lexicalYLower lexicalZLower annotation fixpoint =
	  EOPVaridModu          annotation lexicalEndOfParse
		-- ^ We're provided with a certification that no more characters will
		-- follow in whatever we're parsing, and ‘modu’ is not excluded.
	| InnerSansAscVaridModu annotation varidInnerSansAscSmallUnderscore (list varidInner)
		-- ^ Characters we know aren't excluded are valid characters.
	| ALowerVaridModu       annotation lexicalALower                    (list varidInner)
		-- ^ ‘modua’ starts no excluded identifier.
	| BLowerVaridModu       annotation lexicalBLower                    (list varidInner)
		-- ^ ‘modub’ starts no excluded identifier.
	| CLowerVaridModu       annotation lexicalCLower                    (list varidInner)
		-- ^ ‘moduc’ starts no excluded identifier.
	| DLowerVaridModu       annotation lexicalDLower                    (list varidInner)
		-- ^ ‘modud’ starts no excluded identifier.
	| ELowerVaridModu       annotation lexicalELower                    (list varidInner)
		-- ^ ‘modue’ starts no excluded identifier.
	| FLowerVaridModu       annotation lexicalFLower                    (list varidInner)
		-- ^ ‘moduf’ starts no excluded identifier.
	| GLowerVaridModu       annotation lexicalGLower                    (list varidInner)
		-- ^ ‘modug’ starts no excluded identifier.
	| HLowerVaridModu       annotation lexicalHLower                    (list varidInner)
		-- ^ ‘moduh’ starts no excluded identifier.
	| ILowerVaridModu       annotation lexicalILower                    (list varidInner)
		-- ^ ‘modui’ starts no excluded identifier.
	| JLowerVaridModu       annotation lexicalJLower                    (list varidInner)
		-- ^ ‘moduj’ starts no excluded identifier.
	| KLowerVaridModu       annotation lexicalKLower                    (list varidInner)
		-- ^ ‘moduk’ starts no excluded identifier.
	| LLowerVaridModu       annotation lexicalLLower                    varidModul
		-- ^ ‘modul’ starts 1 excluded identifier.
	| MLowerVaridModu       annotation lexicalMLower                    (list varidInner)
		-- ^ ‘modum’ starts no excluded identifier.
	| NLowerVaridModu       annotation lexicalNLower                    (list varidInner)
		-- ^ ‘modun’ starts no excluded identifier.
	| OLowerVaridModu       annotation lexicalOLower                    (list varidInner)
		-- ^ ‘moduo’ starts no excluded identifier.
	| PLowerVaridModu       annotation lexicalPLower                    (list varidInner)
		-- ^ ‘modup’ starts no excluded identifier.
	| QLowerVaridModu       annotation lexicalQLower                    (list varidInner)
		-- ^ ‘moduq’ starts no excluded identifier.
	| RLowerVaridModu       annotation lexicalRLower                    (list varidInner)
		-- ^ ‘modur’ starts no excluded identifier.
	| SLowerVaridModu       annotation lexicalSLower                    (list varidInner)
		-- ^ ‘modus’ starts no excluded identifier.
	| TLowerVaridModu       annotation lexicalTLower                    (list varidInner)
		-- ^ ‘modut’ starts no excluded identifier.
	| ULowerVaridModu       annotation lexicalULower                    (list varidInner)
		-- ^ ‘moduu’ starts no excluded identifier.
	| VLowerVaridModu       annotation lexicalVLower                    (list varidInner)
		-- ^ ‘moduv’ starts no excluded identifier.
	| WLowerVaridModu       annotation lexicalWLower                    (list varidInner)
		-- ^ ‘moduw’ starts no excluded identifier.
	| XLowerVaridModu       annotation lexicalXLower                    (list varidInner)
		-- ^ ‘modux’ starts no excluded identifier.
	| YLowerVaridModu       annotation lexicalYLower                    (list varidInner)
		-- ^ ‘moduy’ starts no excluded identifier.
	| ZLowerVaridModu       annotation lexicalZLower                    (list varidInner)
		-- ^ ‘moduz’ starts no excluded identifier.

-- | A non-symbolic variable (lowercase-style) identifier name, excluding
-- reserved names, after an ‘newt’ was parsed from the beginning.
data VaridNewtBase list lexicalEndOfParse varidInnerSansAscSmallUnderscore varidInner lexicalALower lexicalBLower lexicalCLower lexicalDLower lexicalELower lexicalFLower lexicalGLower lexicalHLower lexicalILower lexicalJLower lexicalKLower lexicalLLower lexicalMLower lexicalNLower lexicalOLower lexicalPLower lexicalQLower lexicalRLower lexicalSLower lexicalTLower lexicalULower lexicalVLower lexicalWLower lexicalXLower lexicalYLower varidNewty lexicalZLower annotation fixpoint =
	  EOPVaridNewt          annotation lexicalEndOfParse
		-- ^ We're provided with a certification that no more characters will
		-- follow in whatever we're parsing, and ‘newt’ is not excluded.
	| InnerSansAscVaridNewt annotation varidInnerSansAscSmallUnderscore (list varidInner)
		-- ^ Characters we know aren't excluded are valid characters.
	| ALowerVaridNewt       annotation lexicalALower                    (list varidInner)
		-- ^ ‘newta’ starts no excluded identifier.
	| BLowerVaridNewt       annotation lexicalBLower                    (list varidInner)
		-- ^ ‘newtb’ starts no excluded identifier.
	| CLowerVaridNewt       annotation lexicalCLower                    (list varidInner)
		-- ^ ‘newtc’ starts no excluded identifier.
	| DLowerVaridNewt       annotation lexicalDLower                    (list varidInner)
		-- ^ ‘newtd’ starts no excluded identifier.
	| ELowerVaridNewt       annotation lexicalELower                    (list varidInner)
		-- ^ ‘newte’ starts no excluded identifier.
	| FLowerVaridNewt       annotation lexicalFLower                    (list varidInner)
		-- ^ ‘newtf’ starts no excluded identifier.
	| GLowerVaridNewt       annotation lexicalGLower                    (list varidInner)
		-- ^ ‘newtg’ starts no excluded identifier.
	| HLowerVaridNewt       annotation lexicalHLower                    (list varidInner)
		-- ^ ‘newth’ starts no excluded identifier.
	| ILowerVaridNewt       annotation lexicalILower                    (list varidInner)
		-- ^ ‘newti’ starts no excluded identifier.
	| JLowerVaridNewt       annotation lexicalJLower                    (list varidInner)
		-- ^ ‘newtj’ starts no excluded identifier.
	| KLowerVaridNewt       annotation lexicalKLower                    (list varidInner)
		-- ^ ‘newtk’ starts no excluded identifier.
	| LLowerVaridNewt       annotation lexicalLLower                    (list varidInner)
		-- ^ ‘newtl’ starts no excluded identifier.
	| MLowerVaridNewt       annotation lexicalMLower                    (list varidInner)
		-- ^ ‘newtm’ starts no excluded identifier.
	| NLowerVaridNewt       annotation lexicalNLower                    (list varidInner)
		-- ^ ‘newtn’ starts no excluded identifier.
	| OLowerVaridNewt       annotation lexicalOLower                    (list varidInner)
		-- ^ ‘newto’ starts no excluded identifier.
	| PLowerVaridNewt       annotation lexicalPLower                    (list varidInner)
		-- ^ ‘newtp’ starts no excluded identifier.
	| QLowerVaridNewt       annotation lexicalQLower                    (list varidInner)
		-- ^ ‘newtq’ starts no excluded identifier.
	| RLowerVaridNewt       annotation lexicalRLower                    (list varidInner)
		-- ^ ‘newtr’ starts no excluded identifier.
	| SLowerVaridNewt       annotation lexicalSLower                    (list varidInner)
		-- ^ ‘newts’ starts no excluded identifier.
	| TLowerVaridNewt       annotation lexicalTLower                    (list varidInner)
		-- ^ ‘newtt’ starts no excluded identifier.
	| ULowerVaridNewt       annotation lexicalULower                    (list varidInner)
		-- ^ ‘newtu’ starts no excluded identifier.
	| VLowerVaridNewt       annotation lexicalVLower                    (list varidInner)
		-- ^ ‘newtv’ starts no excluded identifier.
	| WLowerVaridNewt       annotation lexicalWLower                    (list varidInner)
		-- ^ ‘newtw’ starts no excluded identifier.
	| XLowerVaridNewt       annotation lexicalXLower                    (list varidInner)
		-- ^ ‘newtx’ starts no excluded identifier.
	| YLowerVaridNewt       annotation lexicalYLower                    varidNewty
		-- ^ ‘newty’ starts 1 excluded identifier.
	| ZLowerVaridNewt       annotation lexicalZLower                    (list varidInner)
		-- ^ ‘newtz’ starts no excluded identifier.

-- | A non-symbolic variable (lowercase-style) identifier name, excluding
-- reserved names, after an ‘wher’ was parsed from the beginning.
data VaridWherBase list lexicalEndOfParse varidInnerSansAscSmallUnderscore varidInner lexicalALower lexicalBLower lexicalCLower lexicalDLower lexicalELower lexicalFLower lexicalGLower lexicalHLower lexicalILower lexicalJLower lexicalKLower lexicalLLower lexicalMLower lexicalNLower lexicalOLower lexicalPLower lexicalQLower lexicalRLower lexicalSLower lexicalTLower lexicalULower lexicalVLower lexicalWLower lexicalXLower lexicalYLower lexicalZLower annotation fixpoint =
	  EOPVaridWher          annotation lexicalEndOfParse
		-- ^ We're provided with a certification that no more characters will
		-- follow in whatever we're parsing, and ‘wher’ is not excluded.
	| InnerSansAscVaridWher annotation varidInnerSansAscSmallUnderscore (list varidInner)
		-- ^ Characters we know aren't excluded are valid characters.
	| ALowerVaridWher       annotation lexicalALower                    (list varidInner)
		-- ^ ‘whera’ starts no excluded identifier.
	| BLowerVaridWher       annotation lexicalBLower                    (list varidInner)
		-- ^ ‘wherb’ starts no excluded identifier.
	| CLowerVaridWher       annotation lexicalCLower                    (list varidInner)
		-- ^ ‘wherc’ starts no excluded identifier.
	| DLowerVaridWher       annotation lexicalDLower                    (list varidInner)
		-- ^ ‘wherd’ starts no excluded identifier.
	| ELowerVaridWher       annotation lexicalELower                    varidInner        (list varidInner)
		-- ^ ‘where’ alone is excluded.  Additional characters is valid.
	| FLowerVaridWher       annotation lexicalFLower                    (list varidInner)
		-- ^ ‘wherf’ starts no excluded identifier.
	| GLowerVaridWher       annotation lexicalGLower                    (list varidInner)
		-- ^ ‘wherg’ starts no excluded identifier.
	| HLowerVaridWher       annotation lexicalHLower                    (list varidInner)
		-- ^ ‘wherh’ starts no excluded identifier.
	| ILowerVaridWher       annotation lexicalILower                    (list varidInner)
		-- ^ ‘wheri’ starts no excluded identifier.
	| JLowerVaridWher       annotation lexicalJLower                    (list varidInner)
		-- ^ ‘wherj’ starts no excluded identifier.
	| KLowerVaridWher       annotation lexicalKLower                    (list varidInner)
		-- ^ ‘wherk’ starts no excluded identifier.
	| LLowerVaridWher       annotation lexicalLLower                    (list varidInner)
		-- ^ ‘wherl’ starts no excluded identifier.
	| MLowerVaridWher       annotation lexicalMLower                    (list varidInner)
		-- ^ ‘wherm’ starts no excluded identifier.
	| NLowerVaridWher       annotation lexicalNLower                    (list varidInner)
		-- ^ ‘whern’ starts no excluded identifier.
	| OLowerVaridWher       annotation lexicalOLower                    (list varidInner)
		-- ^ ‘whero’ starts no excluded identifier.
	| PLowerVaridWher       annotation lexicalPLower                    (list varidInner)
		-- ^ ‘wherp’ starts no excluded identifier.
	| QLowerVaridWher       annotation lexicalQLower                    (list varidInner)
		-- ^ ‘wherq’ starts no excluded identifier.
	| RLowerVaridWher       annotation lexicalRLower                    (list varidInner)
		-- ^ ‘wherr’ starts no excluded identifier.
	| SLowerVaridWher       annotation lexicalSLower                    (list varidInner)
		-- ^ ‘whers’ starts no excluded identifier.
	| TLowerVaridWher       annotation lexicalTLower                    (list varidInner)
		-- ^ ‘whert’ starts no excluded identifier.
	| ULowerVaridWher       annotation lexicalULower                    (list varidInner)
		-- ^ ‘wheru’ starts no excluded identifier.
	| VLowerVaridWher       annotation lexicalVLower                    (list varidInner)
		-- ^ ‘wherv’ starts no excluded identifier.
	| WLowerVaridWher       annotation lexicalWLower                    (list varidInner)
		-- ^ ‘wherw’ starts no excluded identifier.
	| XLowerVaridWher       annotation lexicalXLower                    (list varidInner)
		-- ^ ‘wherx’ starts no excluded identifier.
	| YLowerVaridWher       annotation lexicalYLower                    (list varidInner)
		-- ^ ‘whery’ starts 1 excluded identifier.
	| ZLowerVaridWher       annotation lexicalZLower                    (list varidInner)
		-- ^ ‘wherz’ starts no excluded identifier.

-- | A non-symbolic variable (lowercase-style) identifier name, excluding
-- reserved names, after an ‘defau’ was parsed from the beginning.
data VaridDefauBase list lexicalEndOfParse varidInnerSansAscSmallUnderscore varidInner lexicalALower lexicalBLower lexicalCLower lexicalDLower lexicalELower lexicalFLower lexicalGLower lexicalHLower lexicalILower lexicalJLower lexicalKLower lexicalLLower varidDefaul lexicalMLower lexicalNLower lexicalOLower lexicalPLower lexicalQLower lexicalRLower lexicalSLower lexicalTLower lexicalULower lexicalVLower lexicalWLower lexicalXLower lexicalYLower lexicalZLower annotation fixpoint =
	  EOPVaridDefau          annotation lexicalEndOfParse
		-- ^ We're provided with a certification that no more characters will
		-- follow in whatever we're parsing, and ‘defau’ is not excluded.
	| InnerSansAscVaridDefau annotation varidInnerSansAscSmallUnderscore (list varidInner)
		-- ^ Characters we know aren't excluded are valid characters.
	| ALowerVaridDefau       annotation lexicalALower                    (list varidInner)
		-- ^ ‘defaua’ starts no excluded identifier.
	| BLowerVaridDefau       annotation lexicalBLower                    (list varidInner)
		-- ^ ‘defaub’ starts no excluded identifier.
	| CLowerVaridDefau       annotation lexicalCLower                    (list varidInner)
		-- ^ ‘defauc’ starts no excluded identifier.
	| DLowerVaridDefau       annotation lexicalDLower                    (list varidInner)
		-- ^ ‘defaud’ starts no excluded identifier.
	| ELowerVaridDefau       annotation lexicalELower                    (list varidInner)
		-- ^ ‘defaue’ starts no excluded identifier.
	| FLowerVaridDefau       annotation lexicalFLower                    (list varidInner)
		-- ^ ‘defauf’ starts no excluded identifier.
	| GLowerVaridDefau       annotation lexicalGLower                    (list varidInner)
		-- ^ ‘defaug’ starts no excluded identifier.
	| HLowerVaridDefau       annotation lexicalHLower                    (list varidInner)
		-- ^ ‘defauh’ starts no excluded identifier.
	| ILowerVaridDefau       annotation lexicalILower                    (list varidInner)
		-- ^ ‘defaui’ starts no excluded identifier.
	| JLowerVaridDefau       annotation lexicalJLower                    (list varidInner)
		-- ^ ‘defauj’ starts no excluded identifier.
	| KLowerVaridDefau       annotation lexicalKLower                    (list varidInner)
		-- ^ ‘defauk’ starts no excluded identifier.
	| LLowerVaridDefau       annotation lexicalLLower                    varidDefaul
		-- ^ ‘defaul’ starts 1 excluded identifier.
	| MLowerVaridDefau       annotation lexicalMLower                    (list varidInner)
		-- ^ ‘defaum’ starts no excluded identifier.
	| NLowerVaridDefau       annotation lexicalNLower                    (list varidInner)
		-- ^ ‘defaun’ starts no excluded identifier.
	| OLowerVaridDefau       annotation lexicalOLower                    (list varidInner)
		-- ^ ‘defauo’ starts no excluded identifier.
	| PLowerVaridDefau       annotation lexicalPLower                    (list varidInner)
		-- ^ ‘defaup’ starts no excluded identifier.
	| QLowerVaridDefau       annotation lexicalQLower                    (list varidInner)
		-- ^ ‘defauq’ starts no excluded identifier.
	| RLowerVaridDefau       annotation lexicalRLower                    (list varidInner)
		-- ^ ‘defaur’ starts no excluded identifier.
	| SLowerVaridDefau       annotation lexicalSLower                    (list varidInner)
		-- ^ ‘defaus’ starts no excluded identifier.
	| TLowerVaridDefau       annotation lexicalTLower                    (list varidInner)
		-- ^ ‘defaut’ starts no excluded identifier.
	| ULowerVaridDefau       annotation lexicalULower                    (list varidInner)
		-- ^ ‘defauu’ starts no excluded identifier.
	| VLowerVaridDefau       annotation lexicalVLower                    (list varidInner)
		-- ^ ‘defauv’ starts no excluded identifier.
	| WLowerVaridDefau       annotation lexicalWLower                    (list varidInner)
		-- ^ ‘defauw’ starts no excluded identifier.
	| XLowerVaridDefau       annotation lexicalXLower                    (list varidInner)
		-- ^ ‘defaux’ starts no excluded identifier.
	| YLowerVaridDefau       annotation lexicalYLower                    (list varidInner)
		-- ^ ‘defauy’ starts 1 excluded identifier.
	| ZLowerVaridDefau       annotation lexicalZLower                    (list varidInner)
		-- ^ ‘defauz’ starts no excluded identifier.

-- | A non-symbolic variable (lowercase-style) identifier name, excluding
-- reserved names, after an ‘deriv’ was parsed from the beginning.
data VaridDerivBase list lexicalEndOfParse varidInnerSansAscSmallUnderscore varidInner lexicalALower lexicalBLower lexicalCLower lexicalDLower lexicalELower lexicalFLower lexicalGLower lexicalHLower lexicalILower varidDerivi lexicalJLower lexicalKLower lexicalLLower lexicalMLower lexicalNLower lexicalOLower lexicalPLower lexicalQLower lexicalRLower lexicalSLower lexicalTLower lexicalULower lexicalVLower lexicalWLower lexicalXLower lexicalYLower lexicalZLower annotation fixpoint =
	  EOPVaridDeriv          annotation lexicalEndOfParse
		-- ^ We're provided with a certification that no more characters will
		-- follow in whatever we're parsing, and ‘deriv’ is not excluded.
	| InnerSansAscVaridDeriv annotation varidInnerSansAscSmallUnderscore (list varidInner)
		-- ^ Characters we know aren't excluded are valid characters.
	| ALowerVaridDeriv       annotation lexicalALower                    (list varidInner)
		-- ^ ‘deriva’ starts no excluded identifier.
	| BLowerVaridDeriv       annotation lexicalBLower                    (list varidInner)
		-- ^ ‘derivb’ starts no excluded identifier.
	| CLowerVaridDeriv       annotation lexicalCLower                    (list varidInner)
		-- ^ ‘derivc’ starts no excluded identifier.
	| DLowerVaridDeriv       annotation lexicalDLower                    (list varidInner)
		-- ^ ‘derivd’ starts no excluded identifier.
	| ELowerVaridDeriv       annotation lexicalELower                    (list varidInner)
		-- ^ ‘derive’ starts no excluded identifier.
	| FLowerVaridDeriv       annotation lexicalFLower                    (list varidInner)
		-- ^ ‘derivf’ starts no excluded identifier.
	| GLowerVaridDeriv       annotation lexicalGLower                    (list varidInner)
		-- ^ ‘derivg’ starts no excluded identifier.
	| HLowerVaridDeriv       annotation lexicalHLower                    (list varidInner)
		-- ^ ‘derivh’ starts no excluded identifier.
	| ILowerVaridDeriv       annotation lexicalILower                    varidDerivi
		-- ^ ‘derivi’ starts 1 excluded identifier.
	| JLowerVaridDeriv       annotation lexicalJLower                    (list varidInner)
		-- ^ ‘derivj’ starts no excluded identifier.
	| KLowerVaridDeriv       annotation lexicalKLower                    (list varidInner)
		-- ^ ‘derivk’ starts no excluded identifier.
	| LLowerVaridDeriv       annotation lexicalLLower                    (list varidInner)
		-- ^ ‘derivl’ starts no excluded identifier.
	| MLowerVaridDeriv       annotation lexicalMLower                    (list varidInner)
		-- ^ ‘derivm’ starts no excluded identifier.
	| NLowerVaridDeriv       annotation lexicalNLower                    (list varidInner)
		-- ^ ‘derivn’ starts no excluded identifier.
	| OLowerVaridDeriv       annotation lexicalOLower                    (list varidInner)
		-- ^ ‘derivo’ starts no excluded identifier.
	| PLowerVaridDeriv       annotation lexicalPLower                    (list varidInner)
		-- ^ ‘derivp’ starts no excluded identifier.
	| QLowerVaridDeriv       annotation lexicalQLower                    (list varidInner)
		-- ^ ‘derivq’ starts no excluded identifier.
	| RLowerVaridDeriv       annotation lexicalRLower                    (list varidInner)
		-- ^ ‘derivr’ starts no excluded identifier.
	| SLowerVaridDeriv       annotation lexicalSLower                    (list varidInner)
		-- ^ ‘derivs’ starts no excluded identifier.
	| TLowerVaridDeriv       annotation lexicalTLower                    (list varidInner)
		-- ^ ‘derivt’ starts no excluded identifier.
	| ULowerVaridDeriv       annotation lexicalULower                    (list varidInner)
		-- ^ ‘derivu’ starts no excluded identifier.
	| VLowerVaridDeriv       annotation lexicalVLower                    (list varidInner)
		-- ^ ‘derivv’ starts no excluded identifier.
	| WLowerVaridDeriv       annotation lexicalWLower                    (list varidInner)
		-- ^ ‘derivw’ starts no excluded identifier.
	| XLowerVaridDeriv       annotation lexicalXLower                    (list varidInner)
		-- ^ ‘derivx’ starts no excluded identifier.
	| YLowerVaridDeriv       annotation lexicalYLower                    (list varidInner)
		-- ^ ‘derivy’ starts 1 excluded identifier.
	| ZLowerVaridDeriv       annotation lexicalZLower                    (list varidInner)
		-- ^ ‘derivz’ starts no excluded identifier.

-- | A non-symbolic variable (lowercase-style) identifier name, excluding
-- reserved names, after an ‘forei’ was parsed from the beginning.
data VaridForeiBase list lexicalEndOfParse varidInnerSansAscSmallUnderscore varidInner lexicalALower lexicalBLower lexicalCLower lexicalDLower lexicalELower lexicalFLower lexicalGLower varidForeig lexicalHLower lexicalILower lexicalJLower lexicalKLower lexicalLLower lexicalMLower lexicalNLower lexicalOLower lexicalPLower lexicalQLower lexicalRLower lexicalSLower lexicalTLower lexicalULower lexicalVLower lexicalWLower lexicalXLower lexicalYLower lexicalZLower annotation fixpoint =
	  EOPVaridForei          annotation lexicalEndOfParse
		-- ^ We're provided with a certification that no more characters will
		-- follow in whatever we're parsing, and ‘forei’ is not excluded.
	| InnerSansAscVaridForei annotation varidInnerSansAscSmallUnderscore (list varidInner)
		-- ^ Characters we know aren't excluded are valid characters.
	| ALowerVaridForei       annotation lexicalALower                    (list varidInner)
		-- ^ ‘foreia’ starts no excluded identifier.
	| BLowerVaridForei       annotation lexicalBLower                    (list varidInner)
		-- ^ ‘foreib’ starts no excluded identifier.
	| CLowerVaridForei       annotation lexicalCLower                    (list varidInner)
		-- ^ ‘foreic’ starts no excluded identifier.
	| DLowerVaridForei       annotation lexicalDLower                    (list varidInner)
		-- ^ ‘foreid’ starts no excluded identifier.
	| ELowerVaridForei       annotation lexicalELower                    (list varidInner)
		-- ^ ‘foreie’ starts no excluded identifier.
	| FLowerVaridForei       annotation lexicalFLower                    (list varidInner)
		-- ^ ‘foreif’ starts no excluded identifier.
	| GLowerVaridForei       annotation lexicalGLower                    varidForeig
		-- ^ ‘foreig’ starts 1 excluded identifier.
	| HLowerVaridForei       annotation lexicalHLower                    (list varidInner)
		-- ^ ‘foreih’ starts no excluded identifier.
	| ILowerVaridForei       annotation lexicalILower                    (list varidInner)
		-- ^ ‘foreii’ starts no excluded identifier.
	| JLowerVaridForei       annotation lexicalJLower                    (list varidInner)
		-- ^ ‘foreij’ starts no excluded identifier.
	| KLowerVaridForei       annotation lexicalKLower                    (list varidInner)
		-- ^ ‘foreik’ starts no excluded identifier.
	| LLowerVaridForei       annotation lexicalLLower                    (list varidInner)
		-- ^ ‘foreil’ starts no excluded identifier.
	| MLowerVaridForei       annotation lexicalMLower                    (list varidInner)
		-- ^ ‘foreim’ starts no excluded identifier.
	| NLowerVaridForei       annotation lexicalNLower                    (list varidInner)
		-- ^ ‘forein’ starts no excluded identifier.
	| OLowerVaridForei       annotation lexicalOLower                    (list varidInner)
		-- ^ ‘foreio’ starts no excluded identifier.
	| PLowerVaridForei       annotation lexicalPLower                    (list varidInner)
		-- ^ ‘foreip’ starts no excluded identifier.
	| QLowerVaridForei       annotation lexicalQLower                    (list varidInner)
		-- ^ ‘foreiq’ starts no excluded identifier.
	| RLowerVaridForei       annotation lexicalRLower                    (list varidInner)
		-- ^ ‘foreir’ starts no excluded identifier.
	| SLowerVaridForei       annotation lexicalSLower                    (list varidInner)
		-- ^ ‘foreis’ starts no excluded identifier.
	| TLowerVaridForei       annotation lexicalTLower                    (list varidInner)
		-- ^ ‘foreit’ starts no excluded identifier.
	| ULowerVaridForei       annotation lexicalULower                    (list varidInner)
		-- ^ ‘foreiu’ starts no excluded identifier.
	| VLowerVaridForei       annotation lexicalVLower                    (list varidInner)
		-- ^ ‘foreiv’ starts no excluded identifier.
	| WLowerVaridForei       annotation lexicalWLower                    (list varidInner)
		-- ^ ‘foreiw’ starts no excluded identifier.
	| XLowerVaridForei       annotation lexicalXLower                    (list varidInner)
		-- ^ ‘foreix’ starts no excluded identifier.
	| YLowerVaridForei       annotation lexicalYLower                    (list varidInner)
		-- ^ ‘foreiy’ starts 1 excluded identifier.
	| ZLowerVaridForei       annotation lexicalZLower                    (list varidInner)
		-- ^ ‘foreiz’ starts no excluded identifier.

-- | A non-symbolic variable (lowercase-style) identifier name, excluding
-- reserved names, after an ‘impor’ was parsed from the beginning.
data VaridImporBase list lexicalEndOfParse varidInnerSansAscSmallUnderscore varidInner lexicalALower lexicalBLower lexicalCLower lexicalDLower lexicalELower lexicalFLower lexicalGLower lexicalHLower lexicalILower lexicalJLower lexicalKLower lexicalLLower lexicalMLower lexicalNLower lexicalOLower lexicalPLower lexicalQLower lexicalRLower lexicalSLower lexicalTLower lexicalULower lexicalVLower lexicalWLower lexicalXLower lexicalYLower lexicalZLower annotation fixpoint =
	  EOPVaridImpor          annotation lexicalEndOfParse
		-- ^ We're provided with a certification that no more characters will
		-- follow in whatever we're parsing, and ‘impor’ is not excluded.
	| InnerSansAscVaridImpor annotation varidInnerSansAscSmallUnderscore (list varidInner)
		-- ^ Characters we know aren't excluded are valid characters.
	| ALowerVaridImpor       annotation lexicalALower                    (list varidInner)
		-- ^ ‘impora’ starts no excluded identifier.
	| BLowerVaridImpor       annotation lexicalBLower                    (list varidInner)
		-- ^ ‘imporb’ starts no excluded identifier.
	| CLowerVaridImpor       annotation lexicalCLower                    (list varidInner)
		-- ^ ‘imporc’ starts no excluded identifier.
	| DLowerVaridImpor       annotation lexicalDLower                    (list varidInner)
		-- ^ ‘impord’ starts no excluded identifier.
	| ELowerVaridImpor       annotation lexicalELower                    (list varidInner)
		-- ^ ‘impore’ starts no excluded identifier.
	| FLowerVaridImpor       annotation lexicalFLower                    (list varidInner)
		-- ^ ‘imporf’ starts no excluded identifier.
	| GLowerVaridImpor       annotation lexicalGLower                    (list varidInner)
		-- ^ ‘imporg’ starts no excluded identifier.
	| HLowerVaridImpor       annotation lexicalHLower                    (list varidInner)
		-- ^ ‘imporh’ starts no excluded identifier.
	| ILowerVaridImpor       annotation lexicalILower                    (list varidInner)
		-- ^ ‘impori’ starts no excluded identifier.
	| JLowerVaridImpor       annotation lexicalJLower                    (list varidInner)
		-- ^ ‘imporj’ starts no excluded identifier.
	| KLowerVaridImpor       annotation lexicalKLower                    (list varidInner)
		-- ^ ‘impork’ starts no excluded identifier.
	| LLowerVaridImpor       annotation lexicalLLower                    (list varidInner)
		-- ^ ‘imporl’ starts no excluded identifier.
	| MLowerVaridImpor       annotation lexicalMLower                    (list varidInner)
		-- ^ ‘imporm’ starts no excluded identifier.
	| NLowerVaridImpor       annotation lexicalNLower                    (list varidInner)
		-- ^ ‘imporn’ starts no excluded identifier.
	| OLowerVaridImpor       annotation lexicalOLower                    (list varidInner)
		-- ^ ‘imporo’ starts no excluded identifier.
	| PLowerVaridImpor       annotation lexicalPLower                    (list varidInner)
		-- ^ ‘imporp’ starts no excluded identifier.
	| QLowerVaridImpor       annotation lexicalQLower                    (list varidInner)
		-- ^ ‘imporq’ starts no excluded identifier.
	| RLowerVaridImpor       annotation lexicalRLower                    (list varidInner)
		-- ^ ‘imporr’ starts no excluded identifier.
	| SLowerVaridImpor       annotation lexicalSLower                    (list varidInner)
		-- ^ ‘impors’ starts no excluded identifier.
	| TLowerVaridImpor       annotation lexicalTLower                    varidInner        (list varidInner)
		-- ^ ‘import’ alone is excluded.  Additional characters is valid.
	| ULowerVaridImpor       annotation lexicalULower                    (list varidInner)
		-- ^ ‘imporu’ starts no excluded identifier.
	| VLowerVaridImpor       annotation lexicalVLower                    (list varidInner)
		-- ^ ‘imporv’ starts no excluded identifier.
	| WLowerVaridImpor       annotation lexicalWLower                    (list varidInner)
		-- ^ ‘imporw’ starts no excluded identifier.
	| XLowerVaridImpor       annotation lexicalXLower                    (list varidInner)
		-- ^ ‘imporx’ starts no excluded identifier.
	| YLowerVaridImpor       annotation lexicalYLower                    (list varidInner)
		-- ^ ‘impory’ starts 1 excluded identifier.
	| ZLowerVaridImpor       annotation lexicalZLower                    (list varidInner)
		-- ^ ‘imporz’ starts no excluded identifier.

-- | A non-symbolic variable (lowercase-style) identifier name, excluding
-- reserved names, after an ‘infix’ was parsed from the beginning.
data VaridInfixBase list lexicalEndOfParse varidInnerSansAscSmallUnderscore varidInner lexicalALower lexicalBLower lexicalCLower lexicalDLower lexicalELower lexicalFLower lexicalGLower lexicalHLower lexicalILower lexicalJLower lexicalKLower lexicalLLower lexicalMLower lexicalNLower lexicalOLower lexicalPLower lexicalQLower lexicalRLower lexicalSLower lexicalTLower lexicalULower lexicalVLower lexicalWLower lexicalXLower lexicalYLower lexicalZLower annotation fixpoint =
	-- (We could include this if ‘infix’ identifiers are included, but they are not:)
	{-
	  EOPVaridInfix          annotation lexicalEndOfParse
		-- ^ We're provided with a certification that no more characters will
		-- follow in whatever we're parsing.
	-}
	  InnerSansAscVaridInfix annotation varidInnerSansAscSmallUnderscore (list varidInner)
		-- ^ Characters we know aren't excluded are valid characters.
	| ALowerVaridInfix       annotation lexicalALower                    (list varidInner)
		-- ^ ‘infixa’ starts no excluded identifier.
	| BLowerVaridInfix       annotation lexicalBLower                    (list varidInner)
		-- ^ ‘infixb’ starts no excluded identifier.
	| CLowerVaridInfix       annotation lexicalCLower                    (list varidInner)
		-- ^ ‘infixc’ starts no excluded identifier.
	| DLowerVaridInfix       annotation lexicalDLower                    (list varidInner)
		-- ^ ‘infixd’ starts no excluded identifier.
	| ELowerVaridInfix       annotation lexicalELower                    (list varidInner)
		-- ^ ‘infixe’ starts no excluded identifier.
	| FLowerVaridInfix       annotation lexicalFLower                    (list varidInner)
		-- ^ ‘infixf’ starts no excluded identifier.
	| GLowerVaridInfix       annotation lexicalGLower                    (list varidInner)
		-- ^ ‘infixg’ starts no excluded identifier.
	| HLowerVaridInfix       annotation lexicalHLower                    (list varidInner)
		-- ^ ‘infixh’ starts no excluded identifier.
	| ILowerVaridInfix       annotation lexicalILower                    (list varidInner)
		-- ^ ‘infixi’ starts no excluded identifier.
	| JLowerVaridInfix       annotation lexicalJLower                    (list varidInner)
		-- ^ ‘infixj’ starts no excluded identifier.
	| KLowerVaridInfix       annotation lexicalKLower                    (list varidInner)
		-- ^ ‘infixk’ starts no excluded identifier.
	| LLowerVaridInfix       annotation lexicalLLower                    varidInner        (list varidInner)
		-- ^ ‘infixl’ alone is excluded.  Additional characters is valid.
	| MLowerVaridInfix       annotation lexicalMLower                    (list varidInner)
		-- ^ ‘infixm’ starts no excluded identifier.
	| NLowerVaridInfix       annotation lexicalNLower                    (list varidInner)
		-- ^ ‘infixn’ starts no excluded identifier.
	| OLowerVaridInfix       annotation lexicalOLower                    (list varidInner)
		-- ^ ‘infixo’ starts no excluded identifier.
	| PLowerVaridInfix       annotation lexicalPLower                    (list varidInner)
		-- ^ ‘infixp’ starts no excluded identifier.
	| QLowerVaridInfix       annotation lexicalQLower                    (list varidInner)
		-- ^ ‘infixq’ starts no excluded identifier.
	| RLowerVaridInfix       annotation lexicalRLower                    varidInner        (list varidInner)
		-- ^ ‘infixr’ alone is excluded.  Additional characters is valid.
	| SLowerVaridInfix       annotation lexicalSLower                    (list varidInner)
		-- ^ ‘infixs’ starts no excluded identifier.
	| TLowerVaridInfix       annotation lexicalTLower                    (list varidInner)
		-- ^ ‘infixt’ starts no excluded identifier.
	| ULowerVaridInfix       annotation lexicalULower                    (list varidInner)
		-- ^ ‘infixu’ starts no excluded identifier.
	| VLowerVaridInfix       annotation lexicalVLower                    (list varidInner)
		-- ^ ‘infixv’ starts no excluded identifier.
	| WLowerVaridInfix       annotation lexicalWLower                    (list varidInner)
		-- ^ ‘infixw’ starts no excluded identifier.
	| XLowerVaridInfix       annotation lexicalXLower                    (list varidInner)
		-- ^ ‘infixx’ starts no excluded identifier.
	| YLowerVaridInfix       annotation lexicalYLower                    (list varidInner)
		-- ^ ‘infixy’ starts 1 excluded identifier.
	| ZLowerVaridInfix       annotation lexicalZLower                    (list varidInner)
		-- ^ ‘infixz’ starts no excluded identifier.

-- | A non-symbolic variable (lowercase-style) identifier name, excluding
-- reserved names, after an ‘insta’ was parsed from the beginning.
data VaridInstaBase list lexicalEndOfParse varidInnerSansAscSmallUnderscore varidInner lexicalALower lexicalBLower lexicalCLower lexicalDLower lexicalELower lexicalFLower lexicalGLower lexicalHLower lexicalILower lexicalJLower lexicalKLower lexicalLLower lexicalMLower lexicalNLower varidInstan lexicalOLower lexicalPLower lexicalQLower lexicalRLower lexicalSLower lexicalTLower lexicalULower lexicalVLower lexicalWLower lexicalXLower lexicalYLower lexicalZLower annotation fixpoint =
	  EOPVaridInsta          annotation lexicalEndOfParse
		-- ^ We're provided with a certification that no more characters will
		-- follow in whatever we're parsing, and ‘insta’ is not excluded.
	| InnerSansAscVaridInsta annotation varidInnerSansAscSmallUnderscore (list varidInner)
		-- ^ Characters we know aren't excluded are valid characters.
	| ALowerVaridInsta       annotation lexicalALower                    (list varidInner)
		-- ^ ‘instaa’ starts no excluded identifier.
	| BLowerVaridInsta       annotation lexicalBLower                    (list varidInner)
		-- ^ ‘instab’ starts no excluded identifier.
	| CLowerVaridInsta       annotation lexicalCLower                    (list varidInner)
		-- ^ ‘instac’ starts no excluded identifier.
	| DLowerVaridInsta       annotation lexicalDLower                    (list varidInner)
		-- ^ ‘instad’ starts no excluded identifier.
	| ELowerVaridInsta       annotation lexicalELower                    (list varidInner)
		-- ^ ‘instae’ starts no excluded identifier.
	| FLowerVaridInsta       annotation lexicalFLower                    (list varidInner)
		-- ^ ‘instaf’ starts no excluded identifier.
	| GLowerVaridInsta       annotation lexicalGLower                    (list varidInner)
		-- ^ ‘instag’ starts no excluded identifier.
	| HLowerVaridInsta       annotation lexicalHLower                    (list varidInner)
		-- ^ ‘instah’ starts no excluded identifier.
	| ILowerVaridInsta       annotation lexicalILower                    (list varidInner)
		-- ^ ‘instai’ starts no excluded identifier.
	| JLowerVaridInsta       annotation lexicalJLower                    (list varidInner)
		-- ^ ‘instaj’ starts no excluded identifier.
	| KLowerVaridInsta       annotation lexicalKLower                    (list varidInner)
		-- ^ ‘instak’ starts no excluded identifier.
	| LLowerVaridInsta       annotation lexicalLLower                    (list varidInner)
		-- ^ ‘instal’ starts no excluded identifier.
	| MLowerVaridInsta       annotation lexicalMLower                    (list varidInner)
		-- ^ ‘instam’ starts no excluded identifier.
	| NLowerVaridInsta       annotation lexicalNLower                    varidInstan
		-- ^ ‘instan’ starts 1 excluded identifier.
	| OLowerVaridInsta       annotation lexicalOLower                    (list varidInner)
		-- ^ ‘instao’ starts no excluded identifier.
	| PLowerVaridInsta       annotation lexicalPLower                    (list varidInner)
		-- ^ ‘instap’ starts no excluded identifier.
	| QLowerVaridInsta       annotation lexicalQLower                    (list varidInner)
		-- ^ ‘instaq’ starts no excluded identifier.
	| RLowerVaridInsta       annotation lexicalRLower                    (list varidInner)
		-- ^ ‘instar’ starts no excluded identifier.
	| SLowerVaridInsta       annotation lexicalSLower                    (list varidInner)
		-- ^ ‘instas’ starts no excluded identifier.
	| TLowerVaridInsta       annotation lexicalTLower                    (list varidInner)
		-- ^ ‘instat’ starts no excluded identifier.
	| ULowerVaridInsta       annotation lexicalULower                    (list varidInner)
		-- ^ ‘instau’ starts no excluded identifier.
	| VLowerVaridInsta       annotation lexicalVLower                    (list varidInner)
		-- ^ ‘instav’ starts no excluded identifier.
	| WLowerVaridInsta       annotation lexicalWLower                    (list varidInner)
		-- ^ ‘instaw’ starts no excluded identifier.
	| XLowerVaridInsta       annotation lexicalXLower                    (list varidInner)
		-- ^ ‘instax’ starts no excluded identifier.
	| YLowerVaridInsta       annotation lexicalYLower                    (list varidInner)
		-- ^ ‘instay’ starts no excluded identifier.
	| ZLowerVaridInsta       annotation lexicalZLower                    (list varidInner)
		-- ^ ‘instaz’ starts no excluded identifier.

-- | A non-symbolic variable (lowercase-style) identifier name, excluding
-- reserved names, after an ‘modul’ was parsed from the beginning.
data VaridModulBase list lexicalEndOfParse varidInnerSansAscSmallUnderscore varidInner lexicalALower lexicalBLower lexicalCLower lexicalDLower lexicalELower lexicalFLower lexicalGLower lexicalHLower lexicalILower lexicalJLower lexicalKLower lexicalLLower lexicalMLower lexicalNLower lexicalOLower lexicalPLower lexicalQLower lexicalRLower lexicalSLower lexicalTLower lexicalULower lexicalVLower lexicalWLower lexicalXLower lexicalYLower lexicalZLower annotation fixpoint =
	  EOPVaridModul          annotation lexicalEndOfParse
		-- ^ We're provided with a certification that no more characters will
		-- follow in whatever we're parsing, and ‘modul’ is not excluded.
	| InnerSansAscVaridModul annotation varidInnerSansAscSmallUnderscore (list varidInner)
		-- ^ Characters we know aren't excluded are valid characters.
	| ALowerVaridModul       annotation lexicalALower                    (list varidInner)
		-- ^ ‘modula’ starts no excluded identifier.
	| BLowerVaridModul       annotation lexicalBLower                    (list varidInner)
		-- ^ ‘modulb’ starts no excluded identifier.
	| CLowerVaridModul       annotation lexicalCLower                    (list varidInner)
		-- ^ ‘modulc’ starts no excluded identifier.
	| DLowerVaridModul       annotation lexicalDLower                    (list varidInner)
		-- ^ ‘moduld’ starts no excluded identifier.
	| ELowerVaridModul       annotation lexicalELower                    varidInner        (list varidInner)
		-- ^ ‘module’ alone is excluded.  Additional characters is valid.
	| FLowerVaridModul       annotation lexicalFLower                    (list varidInner)
		-- ^ ‘modulf’ starts no excluded identifier.
	| GLowerVaridModul       annotation lexicalGLower                    (list varidInner)
		-- ^ ‘modulg’ starts no excluded identifier.
	| HLowerVaridModul       annotation lexicalHLower                    (list varidInner)
		-- ^ ‘modulh’ starts no excluded identifier.
	| ILowerVaridModul       annotation lexicalILower                    (list varidInner)
		-- ^ ‘moduli’ starts no excluded identifier.
	| JLowerVaridModul       annotation lexicalJLower                    (list varidInner)
		-- ^ ‘modulj’ starts no excluded identifier.
	| KLowerVaridModul       annotation lexicalKLower                    (list varidInner)
		-- ^ ‘modulk’ starts no excluded identifier.
	| LLowerVaridModul       annotation lexicalLLower                    (list varidInner)
		-- ^ ‘modull’ starts no excluded identifier.
	| MLowerVaridModul       annotation lexicalMLower                    (list varidInner)
		-- ^ ‘modulm’ starts no excluded identifier.
	| NLowerVaridModul       annotation lexicalNLower                    (list varidInner)
		-- ^ ‘moduln’ starts no excluded identifier.
	| OLowerVaridModul       annotation lexicalOLower                    (list varidInner)
		-- ^ ‘modulo’ starts no excluded identifier.
	| PLowerVaridModul       annotation lexicalPLower                    (list varidInner)
		-- ^ ‘modulp’ starts no excluded identifier.
	| QLowerVaridModul       annotation lexicalQLower                    (list varidInner)
		-- ^ ‘modulq’ starts no excluded identifier.
	| RLowerVaridModul       annotation lexicalRLower                    (list varidInner)
		-- ^ ‘modulr’ starts no excluded identifier.
	| SLowerVaridModul       annotation lexicalSLower                    (list varidInner)
		-- ^ ‘moduls’ starts no excluded identifier.
	| TLowerVaridModul       annotation lexicalTLower                    (list varidInner)
		-- ^ ‘modult’ starts no excluded identifier.
	| ULowerVaridModul       annotation lexicalULower                    (list varidInner)
		-- ^ ‘modulu’ starts no excluded identifier.
	| VLowerVaridModul       annotation lexicalVLower                    (list varidInner)
		-- ^ ‘modulv’ starts no excluded identifier.
	| WLowerVaridModul       annotation lexicalWLower                    (list varidInner)
		-- ^ ‘modulw’ starts no excluded identifier.
	| XLowerVaridModul       annotation lexicalXLower                    (list varidInner)
		-- ^ ‘modulx’ starts no excluded identifier.
	| YLowerVaridModul       annotation lexicalYLower                    (list varidInner)
		-- ^ ‘moduly’ starts no excluded identifier.
	| ZLowerVaridModul       annotation lexicalZLower                    (list varidInner)
		-- ^ ‘modulz’ starts no excluded identifier.

-- | A non-symbolic variable (lowercase-style) identifier name, excluding
-- reserved names, after an ‘newty’ was parsed from the beginning.
data VaridNewtyBase list lexicalEndOfParse varidInnerSansAscSmallUnderscore varidInner lexicalALower lexicalBLower lexicalCLower lexicalDLower lexicalELower lexicalFLower lexicalGLower lexicalHLower lexicalILower lexicalJLower lexicalKLower lexicalLLower lexicalMLower lexicalNLower lexicalOLower lexicalPLower varidNewtyp lexicalQLower lexicalRLower lexicalSLower lexicalTLower lexicalULower lexicalVLower lexicalWLower lexicalXLower lexicalYLower lexicalZLower annotation fixpoint =
	  EOPVaridNewty          annotation lexicalEndOfParse
		-- ^ We're provided with a certification that no more characters will
		-- follow in whatever we're parsing, and ‘newty’ is not excluded.
	| InnerSansAscVaridNewty annotation varidInnerSansAscSmallUnderscore (list varidInner)
		-- ^ Characters we know aren't excluded are valid characters.
	| ALowerVaridNewty       annotation lexicalALower                    (list varidInner)
		-- ^ ‘newtya’ starts no excluded identifier.
	| BLowerVaridNewty       annotation lexicalBLower                    (list varidInner)
		-- ^ ‘newtyb’ starts no excluded identifier.
	| CLowerVaridNewty       annotation lexicalCLower                    (list varidInner)
		-- ^ ‘newtyc’ starts no excluded identifier.
	| DLowerVaridNewty       annotation lexicalDLower                    (list varidInner)
		-- ^ ‘newtyd’ starts no excluded identifier.
	| ELowerVaridNewty       annotation lexicalELower                    (list varidInner)
		-- ^ ‘newtye’ starts no excluded identifier.
	| FLowerVaridNewty       annotation lexicalFLower                    (list varidInner)
		-- ^ ‘newtyf’ starts no excluded identifier.
	| GLowerVaridNewty       annotation lexicalGLower                    (list varidInner)
		-- ^ ‘newtyg’ starts no excluded identifier.
	| HLowerVaridNewty       annotation lexicalHLower                    (list varidInner)
		-- ^ ‘newtyh’ starts no excluded identifier.
	| ILowerVaridNewty       annotation lexicalILower                    (list varidInner)
		-- ^ ‘newtyi’ starts no excluded identifier.
	| JLowerVaridNewty       annotation lexicalJLower                    (list varidInner)
		-- ^ ‘newtyj’ starts no excluded identifier.
	| KLowerVaridNewty       annotation lexicalKLower                    (list varidInner)
		-- ^ ‘newtyk’ starts no excluded identifier.
	| LLowerVaridNewty       annotation lexicalLLower                    (list varidInner)
		-- ^ ‘newtyl’ starts no excluded identifier.
	| MLowerVaridNewty       annotation lexicalMLower                    (list varidInner)
		-- ^ ‘newtym’ starts no excluded identifier.
	| NLowerVaridNewty       annotation lexicalNLower                    (list varidInner)
		-- ^ ‘newtyn’ starts no excluded identifier.
	| OLowerVaridNewty       annotation lexicalOLower                    (list varidInner)
		-- ^ ‘newtyo’ starts no excluded identifier.
	| PLowerVaridNewty       annotation lexicalPLower                    varidNewtyp
		-- ^ ‘newtyp’ starts 1 excluded identifier.
	| QLowerVaridNewty       annotation lexicalQLower                    (list varidInner)
		-- ^ ‘newtyq’ starts no excluded identifier.
	| RLowerVaridNewty       annotation lexicalRLower                    (list varidInner)
		-- ^ ‘newtyr’ starts no excluded identifier.
	| SLowerVaridNewty       annotation lexicalSLower                    (list varidInner)
		-- ^ ‘newtys’ starts no excluded identifier.
	| TLowerVaridNewty       annotation lexicalTLower                    (list varidInner)
		-- ^ ‘newtyt’ starts no excluded identifier.
	| ULowerVaridNewty       annotation lexicalULower                    (list varidInner)
		-- ^ ‘newtyu’ starts no excluded identifier.
	| VLowerVaridNewty       annotation lexicalVLower                    (list varidInner)
		-- ^ ‘newtyv’ starts no excluded identifier.
	| WLowerVaridNewty       annotation lexicalWLower                    (list varidInner)
		-- ^ ‘newtyw’ starts no excluded identifier.
	| XLowerVaridNewty       annotation lexicalXLower                    (list varidInner)
		-- ^ ‘newtyx’ starts no excluded identifier.
	| YLowerVaridNewty       annotation lexicalYLower                    (list varidInner)
		-- ^ ‘newtyy’ starts no excluded identifier.
	| ZLowerVaridNewty       annotation lexicalZLower                    (list varidInner)
		-- ^ ‘newtyz’ starts no excluded identifier.

-- | A non-symbolic variable (lowercase-style) identifier name, excluding
-- reserved names, after an ‘defaul’ was parsed from the beginning.
data VaridDefaulBase list lexicalEndOfParse varidInnerSansAscSmallUnderscore varidInner lexicalALower lexicalBLower lexicalCLower lexicalDLower lexicalELower lexicalFLower lexicalGLower lexicalHLower lexicalILower lexicalJLower lexicalKLower lexicalLLower lexicalMLower lexicalNLower lexicalOLower lexicalPLower lexicalQLower lexicalRLower lexicalSLower lexicalTLower lexicalULower lexicalVLower lexicalWLower lexicalXLower lexicalYLower lexicalZLower annotation fixpoint =
	  EOPVaridDefaul          annotation lexicalEndOfParse
		-- ^ We're provided with a certification that no more characters will
		-- follow in whatever we're parsing, and ‘defaul’ is not excluded.
	| InnerSansAscVaridDefaul annotation varidInnerSansAscSmallUnderscore (list varidInner)
		-- ^ Characters we know aren't excluded are valid characters.
	| ALowerVaridDefaul       annotation lexicalALower                    (list varidInner)
		-- ^ ‘defaula’ starts no excluded identifier.
	| BLowerVaridDefaul       annotation lexicalBLower                    (list varidInner)
		-- ^ ‘defaulb’ starts no excluded identifier.
	| CLowerVaridDefaul       annotation lexicalCLower                    (list varidInner)
		-- ^ ‘defaulc’ starts no excluded identifier.
	| DLowerVaridDefaul       annotation lexicalDLower                    (list varidInner)
		-- ^ ‘defauld’ starts no excluded identifier.
	| ELowerVaridDefaul       annotation lexicalELower                    (list varidInner)
		-- ^ ‘defaule’ starts no excluded identifier.
	| FLowerVaridDefaul       annotation lexicalFLower                    (list varidInner)
		-- ^ ‘defaulf’ starts no excluded identifier.
	| GLowerVaridDefaul       annotation lexicalGLower                    (list varidInner)
		-- ^ ‘defaulg’ starts no excluded identifier.
	| HLowerVaridDefaul       annotation lexicalHLower                    (list varidInner)
		-- ^ ‘defaulh’ starts no excluded identifier.
	| ILowerVaridDefaul       annotation lexicalILower                    (list varidInner)
		-- ^ ‘defauli’ starts no excluded identifier.
	| JLowerVaridDefaul       annotation lexicalJLower                    (list varidInner)
		-- ^ ‘defaulj’ starts no excluded identifier.
	| KLowerVaridDefaul       annotation lexicalKLower                    (list varidInner)
		-- ^ ‘defaulk’ starts no excluded identifier.
	| LLowerVaridDefaul       annotation lexicalLLower                    (list varidInner)
		-- ^ ‘defaull’ starts no excluded identifier.
	| MLowerVaridDefaul       annotation lexicalMLower                    (list varidInner)
		-- ^ ‘defaulm’ starts no excluded identifier.
	| NLowerVaridDefaul       annotation lexicalNLower                    (list varidInner)
		-- ^ ‘defauln’ starts no excluded identifier.
	| OLowerVaridDefaul       annotation lexicalOLower                    (list varidInner)
		-- ^ ‘defaulo’ starts no excluded identifier.
	| PLowerVaridDefaul       annotation lexicalPLower                    (list varidInner)
		-- ^ ‘defaulp’ starts no excluded identifier.
	| QLowerVaridDefaul       annotation lexicalQLower                    (list varidInner)
		-- ^ ‘defaulq’ starts no excluded identifier.
	| RLowerVaridDefaul       annotation lexicalRLower                    (list varidInner)
		-- ^ ‘defaulr’ starts no excluded identifier.
	| SLowerVaridDefaul       annotation lexicalSLower                    (list varidInner)
		-- ^ ‘defauls’ starts no excluded identifier.
	| TLowerVaridDefaul       annotation lexicalTLower                    varidInner        (list varidInner)
		-- ^ ‘default’ alone is excluded.  Additional characters is valid.
	| ULowerVaridDefaul       annotation lexicalULower                    (list varidInner)
		-- ^ ‘defaulu’ starts no excluded identifier.
	| VLowerVaridDefaul       annotation lexicalVLower                    (list varidInner)
		-- ^ ‘defaulv’ starts no excluded identifier.
	| WLowerVaridDefaul       annotation lexicalWLower                    (list varidInner)
		-- ^ ‘defaulw’ starts no excluded identifier.
	| XLowerVaridDefaul       annotation lexicalXLower                    (list varidInner)
		-- ^ ‘defaulx’ starts no excluded identifier.
	| YLowerVaridDefaul       annotation lexicalYLower                    (list varidInner)
		-- ^ ‘defauly’ starts no excluded identifier.
	| ZLowerVaridDefaul       annotation lexicalZLower                    (list varidInner)
		-- ^ ‘defaulz’ starts no excluded identifier.

-- | A non-symbolic variable (lowercase-style) identifier name, excluding
-- reserved names, after an ‘derivi’ was parsed from the beginning.
data VaridDeriviBase list lexicalEndOfParse varidInnerSansAscSmallUnderscore varidInner lexicalALower lexicalBLower lexicalCLower lexicalDLower lexicalELower lexicalFLower lexicalGLower lexicalHLower lexicalILower lexicalJLower lexicalKLower lexicalLLower lexicalMLower lexicalNLower varidDerivin lexicalOLower lexicalPLower lexicalQLower lexicalRLower lexicalSLower lexicalTLower lexicalULower lexicalVLower lexicalWLower lexicalXLower lexicalYLower lexicalZLower annotation fixpoint =
	  EOPVaridDerivi          annotation lexicalEndOfParse
		-- ^ We're provided with a certification that no more characters will
		-- follow in whatever we're parsing, and ‘derivi’ is not excluded.
	| InnerSansAscVaridDerivi annotation varidInnerSansAscSmallUnderscore (list varidInner)
		-- ^ Characters we know aren't excluded are valid characters.
	| ALowerVaridDerivi       annotation lexicalALower                    (list varidInner)
		-- ^ ‘derivia’ starts no excluded identifier.
	| BLowerVaridDerivi       annotation lexicalBLower                    (list varidInner)
		-- ^ ‘derivib’ starts no excluded identifier.
	| CLowerVaridDerivi       annotation lexicalCLower                    (list varidInner)
		-- ^ ‘derivic’ starts no excluded identifier.
	| DLowerVaridDerivi       annotation lexicalDLower                    (list varidInner)
		-- ^ ‘derivid’ starts no excluded identifier.
	| ELowerVaridDerivi       annotation lexicalELower                    (list varidInner)
		-- ^ ‘derivie’ starts no excluded identifier.
	| FLowerVaridDerivi       annotation lexicalFLower                    (list varidInner)
		-- ^ ‘derivif’ starts no excluded identifier.
	| GLowerVaridDerivi       annotation lexicalGLower                    (list varidInner)
		-- ^ ‘derivig’ starts no excluded identifier.
	| HLowerVaridDerivi       annotation lexicalHLower                    (list varidInner)
		-- ^ ‘derivih’ starts no excluded identifier.
	| ILowerVaridDerivi       annotation lexicalILower                    (list varidInner)
		-- ^ ‘derivii’ starts no excluded identifier.
	| JLowerVaridDerivi       annotation lexicalJLower                    (list varidInner)
		-- ^ ‘derivij’ starts no excluded identifier.
	| KLowerVaridDerivi       annotation lexicalKLower                    (list varidInner)
		-- ^ ‘derivik’ starts no excluded identifier.
	| LLowerVaridDerivi       annotation lexicalLLower                    (list varidInner)
		-- ^ ‘derivil’ starts no excluded identifier.
	| MLowerVaridDerivi       annotation lexicalMLower                    (list varidInner)
		-- ^ ‘derivim’ starts no excluded identifier.
	| NLowerVaridDerivi       annotation lexicalNLower                    varidDerivin
		-- ^ ‘derivin’ starts 1 excluded identifier.
	| OLowerVaridDerivi       annotation lexicalOLower                    (list varidInner)
		-- ^ ‘derivio’ starts no excluded identifier.
	| PLowerVaridDerivi       annotation lexicalPLower                    (list varidInner)
		-- ^ ‘derivip’ starts no excluded identifier.
	| QLowerVaridDerivi       annotation lexicalQLower                    (list varidInner)
		-- ^ ‘deriviq’ starts no excluded identifier.
	| RLowerVaridDerivi       annotation lexicalRLower                    (list varidInner)
		-- ^ ‘derivir’ starts no excluded identifier.
	| SLowerVaridDerivi       annotation lexicalSLower                    (list varidInner)
		-- ^ ‘derivis’ starts no excluded identifier.
	| TLowerVaridDerivi       annotation lexicalTLower                    (list varidInner)
		-- ^ ‘derivit’ starts no excluded identifier.
	| ULowerVaridDerivi       annotation lexicalULower                    (list varidInner)
		-- ^ ‘deriviu’ starts no excluded identifier.
	| VLowerVaridDerivi       annotation lexicalVLower                    (list varidInner)
		-- ^ ‘deriviv’ starts no excluded identifier.
	| WLowerVaridDerivi       annotation lexicalWLower                    (list varidInner)
		-- ^ ‘deriviw’ starts no excluded identifier.
	| XLowerVaridDerivi       annotation lexicalXLower                    (list varidInner)
		-- ^ ‘derivix’ starts no excluded identifier.
	| YLowerVaridDerivi       annotation lexicalYLower                    (list varidInner)
		-- ^ ‘deriviy’ starts no excluded identifier.
	| ZLowerVaridDerivi       annotation lexicalZLower                    (list varidInner)
		-- ^ ‘deriviz’ starts no excluded identifier.

-- | A non-symbolic variable (lowercase-style) identifier name, excluding
-- reserved names, after an ‘foreig’ was parsed from the beginning.
data VaridForeigBase list lexicalEndOfParse varidInnerSansAscSmallUnderscore varidInner lexicalALower lexicalBLower lexicalCLower lexicalDLower lexicalELower lexicalFLower lexicalGLower lexicalHLower lexicalILower lexicalJLower lexicalKLower lexicalLLower lexicalMLower lexicalNLower lexicalOLower lexicalPLower lexicalQLower lexicalRLower lexicalSLower lexicalTLower lexicalULower lexicalVLower lexicalWLower lexicalXLower lexicalYLower lexicalZLower annotation fixpoint =
	  EOPVaridForeig          annotation lexicalEndOfParse
		-- ^ We're provided with a certification that no more characters will
		-- follow in whatever we're parsing, and ‘foreig’ is not excluded.
	| InnerSansAscVaridForeig annotation varidInnerSansAscSmallUnderscore (list varidInner)
		-- ^ Characters we know aren't excluded are valid characters.
	| ALowerVaridForeig       annotation lexicalALower                    (list varidInner)
		-- ^ ‘foreiga’ starts no excluded identifier.
	| BLowerVaridForeig       annotation lexicalBLower                    (list varidInner)
		-- ^ ‘foreigb’ starts no excluded identifier.
	| CLowerVaridForeig       annotation lexicalCLower                    (list varidInner)
		-- ^ ‘foreigc’ starts no excluded identifier.
	| DLowerVaridForeig       annotation lexicalDLower                    (list varidInner)
		-- ^ ‘foreigd’ starts no excluded identifier.
	| ELowerVaridForeig       annotation lexicalELower                    (list varidInner)
		-- ^ ‘foreige’ starts no excluded identifier.
	| FLowerVaridForeig       annotation lexicalFLower                    (list varidInner)
		-- ^ ‘foreigf’ starts no excluded identifier.
	| GLowerVaridForeig       annotation lexicalGLower                    (list varidInner)
		-- ^ ‘foreigg’ starts no excluded identifier.
	| HLowerVaridForeig       annotation lexicalHLower                    (list varidInner)
		-- ^ ‘foreigh’ starts no excluded identifier.
	| ILowerVaridForeig       annotation lexicalILower                    (list varidInner)
		-- ^ ‘foreigi’ starts no excluded identifier.
	| JLowerVaridForeig       annotation lexicalJLower                    (list varidInner)
		-- ^ ‘foreigj’ starts no excluded identifier.
	| KLowerVaridForeig       annotation lexicalKLower                    (list varidInner)
		-- ^ ‘foreigk’ starts no excluded identifier.
	| LLowerVaridForeig       annotation lexicalLLower                    (list varidInner)
		-- ^ ‘foreigl’ starts no excluded identifier.
	| MLowerVaridForeig       annotation lexicalMLower                    (list varidInner)
		-- ^ ‘foreigm’ starts no excluded identifier.
	| NLowerVaridForeig       annotation lexicalNLower                    varidInner        (list varidInner)
		-- ^ ‘foreign’ alone is excluded.  Additional characters is valid.
	| OLowerVaridForeig       annotation lexicalOLower                    (list varidInner)
		-- ^ ‘foreigo’ starts no excluded identifier.
	| PLowerVaridForeig       annotation lexicalPLower                    (list varidInner)
		-- ^ ‘foreigp’ starts no excluded identifier.
	| QLowerVaridForeig       annotation lexicalQLower                    (list varidInner)
		-- ^ ‘foreigq’ starts no excluded identifier.
	| RLowerVaridForeig       annotation lexicalRLower                    (list varidInner)
		-- ^ ‘foreigr’ starts no excluded identifier.
	| SLowerVaridForeig       annotation lexicalSLower                    (list varidInner)
		-- ^ ‘foreigs’ starts no excluded identifier.
	| TLowerVaridForeig       annotation lexicalTLower                    (list varidInner)
		-- ^ ‘foreigt’ starts no excluded identifier.
	| ULowerVaridForeig       annotation lexicalULower                    (list varidInner)
		-- ^ ‘foreigu’ starts no excluded identifier.
	| VLowerVaridForeig       annotation lexicalVLower                    (list varidInner)
		-- ^ ‘foreigv’ starts no excluded identifier.
	| WLowerVaridForeig       annotation lexicalWLower                    (list varidInner)
		-- ^ ‘foreigw’ starts no excluded identifier.
	| XLowerVaridForeig       annotation lexicalXLower                    (list varidInner)
		-- ^ ‘foreigx’ starts no excluded identifier.
	| YLowerVaridForeig       annotation lexicalYLower                    (list varidInner)
		-- ^ ‘foreigy’ starts no excluded identifier.
	| ZLowerVaridForeig       annotation lexicalZLower                    (list varidInner)
		-- ^ ‘foreigz’ starts no excluded identifier.

-- | A non-symbolic variable (lowercase-style) identifier name, excluding
-- reserved names, after an ‘instan’ was parsed from the beginning.
data VaridInstanBase list lexicalEndOfParse varidInnerSansAscSmallUnderscore varidInner lexicalALower lexicalBLower lexicalCLower varidInstanc lexicalDLower lexicalELower lexicalFLower lexicalGLower lexicalHLower lexicalILower lexicalJLower lexicalKLower lexicalLLower lexicalMLower lexicalNLower lexicalOLower lexicalPLower lexicalQLower lexicalRLower lexicalSLower lexicalTLower lexicalULower lexicalVLower lexicalWLower lexicalXLower lexicalYLower lexicalZLower annotation fixpoint =
	  EOPVaridInstan          annotation lexicalEndOfParse
		-- ^ We're provided with a certification that no more characters will
		-- follow in whatever we're parsing, and ‘instan’ is not excluded.
	| InnerSansAscVaridInstan annotation varidInnerSansAscSmallUnderscore (list varidInner)
		-- ^ Characters we know aren't excluded are valid characters.
	| ALowerVaridInstan       annotation lexicalALower                    (list varidInner)
		-- ^ ‘instana’ starts no excluded identifier.
	| BLowerVaridInstan       annotation lexicalBLower                    (list varidInner)
		-- ^ ‘instanb’ starts no excluded identifier.
	| CLowerVaridInstan       annotation lexicalCLower                    varidInstanc
		-- ^ ‘instanc’ starts 1 excluded identifier.
	| DLowerVaridInstan       annotation lexicalDLower                    (list varidInner)
		-- ^ ‘instand’ starts no excluded identifier.
	| ELowerVaridInstan       annotation lexicalELower                    (list varidInner)
		-- ^ ‘instane’ starts no excluded identifier.
	| FLowerVaridInstan       annotation lexicalFLower                    (list varidInner)
		-- ^ ‘instanf’ starts no excluded identifier.
	| GLowerVaridInstan       annotation lexicalGLower                    (list varidInner)
		-- ^ ‘instang’ starts no excluded identifier.
	| HLowerVaridInstan       annotation lexicalHLower                    (list varidInner)
		-- ^ ‘instanh’ starts no excluded identifier.
	| ILowerVaridInstan       annotation lexicalILower                    (list varidInner)
		-- ^ ‘instani’ starts no excluded identifier.
	| JLowerVaridInstan       annotation lexicalJLower                    (list varidInner)
		-- ^ ‘instanj’ starts no excluded identifier.
	| KLowerVaridInstan       annotation lexicalKLower                    (list varidInner)
		-- ^ ‘instank’ starts no excluded identifier.
	| LLowerVaridInstan       annotation lexicalLLower                    (list varidInner)
		-- ^ ‘instanl’ starts no excluded identifier.
	| MLowerVaridInstan       annotation lexicalMLower                    (list varidInner)
		-- ^ ‘instanm’ starts no excluded identifier.
	| NLowerVaridInstan       annotation lexicalNLower                    (list varidInner)
		-- ^ ‘instann’ starts no excluded identifier.
	| OLowerVaridInstan       annotation lexicalOLower                    (list varidInner)
		-- ^ ‘instano’ starts no excluded identifier.
	| PLowerVaridInstan       annotation lexicalPLower                    (list varidInner)
		-- ^ ‘instanp’ starts no excluded identifier.
	| QLowerVaridInstan       annotation lexicalQLower                    (list varidInner)
		-- ^ ‘instanq’ starts no excluded identifier.
	| RLowerVaridInstan       annotation lexicalRLower                    (list varidInner)
		-- ^ ‘instanr’ starts no excluded identifier.
	| SLowerVaridInstan       annotation lexicalSLower                    (list varidInner)
		-- ^ ‘instans’ starts no excluded identifier.
	| TLowerVaridInstan       annotation lexicalTLower                    (list varidInner)
		-- ^ ‘instant’ starts no excluded identifier.
	| ULowerVaridInstan       annotation lexicalULower                    (list varidInner)
		-- ^ ‘instanu’ starts no excluded identifier.
	| VLowerVaridInstan       annotation lexicalVLower                    (list varidInner)
		-- ^ ‘instanv’ starts no excluded identifier.
	| WLowerVaridInstan       annotation lexicalWLower                    (list varidInner)
		-- ^ ‘instanw’ starts no excluded identifier.
	| XLowerVaridInstan       annotation lexicalXLower                    (list varidInner)
		-- ^ ‘instanx’ starts no excluded identifier.
	| YLowerVaridInstan       annotation lexicalYLower                    (list varidInner)
		-- ^ ‘instany’ starts no excluded identifier.
	| ZLowerVaridInstan       annotation lexicalZLower                    (list varidInner)
		-- ^ ‘instanz’ starts no excluded identifier.

-- | A non-symbolic variable (lowercase-style) identifier name, excluding
-- reserved names, after an ‘newtyp’ was parsed from the beginning.
data VaridNewtypBase list lexicalEndOfParse varidInnerSansAscSmallUnderscore varidInner lexicalALower lexicalBLower lexicalCLower lexicalDLower lexicalELower lexicalFLower lexicalGLower lexicalHLower lexicalILower lexicalJLower lexicalKLower lexicalLLower lexicalMLower lexicalNLower lexicalOLower lexicalPLower lexicalQLower lexicalRLower lexicalSLower lexicalTLower lexicalULower lexicalVLower lexicalWLower lexicalXLower lexicalYLower lexicalZLower annotation fixpoint =
	  EOPVaridNewtyp          annotation lexicalEndOfParse
		-- ^ We're provided with a certification that no more characters will
		-- follow in whatever we're parsing, and ‘newtyp’ is not excluded.
	| InnerSansAscVaridNewtyp annotation varidInnerSansAscSmallUnderscore (list varidInner)
		-- ^ Characters we know aren't excluded are valid characters.
	| ALowerVaridNewtyp       annotation lexicalALower                    (list varidInner)
		-- ^ ‘newtypa’ starts no excluded identifier.
	| BLowerVaridNewtyp       annotation lexicalBLower                    (list varidInner)
		-- ^ ‘newtypb’ starts no excluded identifier.
	| CLowerVaridNewtyp       annotation lexicalCLower                    (list varidInner)
		-- ^ ‘newtypc’ starts no excluded identifier.
	| DLowerVaridNewtyp       annotation lexicalDLower                    (list varidInner)
		-- ^ ‘newtypd’ starts no excluded identifier.
	| ELowerVaridNewtyp       annotation lexicalELower                    varidInner        (list varidInner)
		-- ^ ‘newtype’ alone is excluded.  Additional characters is valid.
	| FLowerVaridNewtyp       annotation lexicalFLower                    (list varidInner)
		-- ^ ‘newtypf’ starts no excluded identifier.
	| GLowerVaridNewtyp       annotation lexicalGLower                    (list varidInner)
		-- ^ ‘newtypg’ starts no excluded identifier.
	| HLowerVaridNewtyp       annotation lexicalHLower                    (list varidInner)
		-- ^ ‘newtyph’ starts no excluded identifier.
	| ILowerVaridNewtyp       annotation lexicalILower                    (list varidInner)
		-- ^ ‘newtypi’ starts no excluded identifier.
	| JLowerVaridNewtyp       annotation lexicalJLower                    (list varidInner)
		-- ^ ‘newtypj’ starts no excluded identifier.
	| KLowerVaridNewtyp       annotation lexicalKLower                    (list varidInner)
		-- ^ ‘newtypk’ starts no excluded identifier.
	| LLowerVaridNewtyp       annotation lexicalLLower                    (list varidInner)
		-- ^ ‘newtypl’ starts no excluded identifier.
	| MLowerVaridNewtyp       annotation lexicalMLower                    (list varidInner)
		-- ^ ‘newtypm’ starts no excluded identifier.
	| NLowerVaridNewtyp       annotation lexicalNLower                    (list varidInner)
		-- ^ ‘newtypn’ starts no excluded identifier.
	| OLowerVaridNewtyp       annotation lexicalOLower                    (list varidInner)
		-- ^ ‘newtypo’ starts no excluded identifier.
	| PLowerVaridNewtyp       annotation lexicalPLower                    (list varidInner)
		-- ^ ‘newtypp’ starts no excluded identifier.
	| QLowerVaridNewtyp       annotation lexicalQLower                    (list varidInner)
		-- ^ ‘newtypq’ starts no excluded identifier.
	| RLowerVaridNewtyp       annotation lexicalRLower                    (list varidInner)
		-- ^ ‘newtypr’ starts no excluded identifier.
	| SLowerVaridNewtyp       annotation lexicalSLower                    (list varidInner)
		-- ^ ‘newtyps’ starts no excluded identifier.
	| TLowerVaridNewtyp       annotation lexicalTLower                    (list varidInner)
		-- ^ ‘newtypt’ starts no excluded identifier.
	| ULowerVaridNewtyp       annotation lexicalULower                    (list varidInner)
		-- ^ ‘newtypu’ starts no excluded identifier.
	| VLowerVaridNewtyp       annotation lexicalVLower                    (list varidInner)
		-- ^ ‘newtypv’ starts no excluded identifier.
	| WLowerVaridNewtyp       annotation lexicalWLower                    (list varidInner)
		-- ^ ‘newtypw’ starts no excluded identifier.
	| XLowerVaridNewtyp       annotation lexicalXLower                    (list varidInner)
		-- ^ ‘newtypx’ starts no excluded identifier.
	| YLowerVaridNewtyp       annotation lexicalYLower                    (list varidInner)
		-- ^ ‘newtypy’ starts no excluded identifier.
	| ZLowerVaridNewtyp       annotation lexicalZLower                    (list varidInner)
		-- ^ ‘newtypz’ starts no excluded identifier.

-- | A non-symbolic variable (lowercase-style) identifier name, excluding
-- reserved names, after an ‘derivin’ was parsed from the beginning.
data VaridDerivinBase list lexicalEndOfParse varidInnerSansAscSmallUnderscore varidInner lexicalALower lexicalBLower lexicalCLower lexicalDLower lexicalELower lexicalFLower lexicalGLower lexicalHLower lexicalILower lexicalJLower lexicalKLower lexicalLLower lexicalMLower lexicalNLower lexicalOLower lexicalPLower lexicalQLower lexicalRLower lexicalSLower lexicalTLower lexicalULower lexicalVLower lexicalWLower lexicalXLower lexicalYLower lexicalZLower annotation fixpoint =
	  EOPVaridDerivin          annotation lexicalEndOfParse
		-- ^ We're provided with a certification that no more characters will
		-- follow in whatever we're parsing, and ‘derivin’ is not excluded.
	| InnerSansAscVaridDerivin annotation varidInnerSansAscSmallUnderscore (list varidInner)
		-- ^ Characters we know aren't excluded are valid characters.
	| ALowerVaridDerivin       annotation lexicalALower                    (list varidInner)
		-- ^ ‘derivina’ starts no excluded identifier.
	| BLowerVaridDerivin       annotation lexicalBLower                    (list varidInner)
		-- ^ ‘derivinb’ starts no excluded identifier.
	| CLowerVaridDerivin       annotation lexicalCLower                    (list varidInner)
		-- ^ ‘derivinc’ starts no excluded identifier.
	| DLowerVaridDerivin       annotation lexicalDLower                    (list varidInner)
		-- ^ ‘derivind’ starts no excluded identifier.
	| ELowerVaridDerivin       annotation lexicalELower                    (list varidInner)
		-- ^ ‘derivine’ starts no excluded identifier.
	| FLowerVaridDerivin       annotation lexicalFLower                    (list varidInner)
		-- ^ ‘derivinf’ starts no excluded identifier.
	| GLowerVaridDerivin       annotation lexicalGLower                    varidInner        (list varidInner)
		-- ^ ‘deriving’ alone is excluded.  Additional characters is valid.
	| HLowerVaridDerivin       annotation lexicalHLower                    (list varidInner)
		-- ^ ‘derivinh’ starts no excluded identifier.
	| ILowerVaridDerivin       annotation lexicalILower                    (list varidInner)
		-- ^ ‘derivini’ starts no excluded identifier.
	| JLowerVaridDerivin       annotation lexicalJLower                    (list varidInner)
		-- ^ ‘derivinj’ starts no excluded identifier.
	| KLowerVaridDerivin       annotation lexicalKLower                    (list varidInner)
		-- ^ ‘derivink’ starts no excluded identifier.
	| LLowerVaridDerivin       annotation lexicalLLower                    (list varidInner)
		-- ^ ‘derivinl’ starts no excluded identifier.
	| MLowerVaridDerivin       annotation lexicalMLower                    (list varidInner)
		-- ^ ‘derivinm’ starts no excluded identifier.
	| NLowerVaridDerivin       annotation lexicalNLower                    (list varidInner)
		-- ^ ‘derivinn’ starts no excluded identifier.
	| OLowerVaridDerivin       annotation lexicalOLower                    (list varidInner)
		-- ^ ‘derivino’ starts no excluded identifier.
	| PLowerVaridDerivin       annotation lexicalPLower                    (list varidInner)
		-- ^ ‘derivinp’ starts no excluded identifier.
	| QLowerVaridDerivin       annotation lexicalQLower                    (list varidInner)
		-- ^ ‘derivinq’ starts no excluded identifier.
	| RLowerVaridDerivin       annotation lexicalRLower                    (list varidInner)
		-- ^ ‘derivinr’ starts no excluded identifier.
	| SLowerVaridDerivin       annotation lexicalSLower                    (list varidInner)
		-- ^ ‘derivins’ starts no excluded identifier.
	| TLowerVaridDerivin       annotation lexicalTLower                    (list varidInner)
		-- ^ ‘derivint’ starts no excluded identifier.
	| ULowerVaridDerivin       annotation lexicalULower                    (list varidInner)
		-- ^ ‘derivinu’ starts no excluded identifier.
	| VLowerVaridDerivin       annotation lexicalVLower                    (list varidInner)
		-- ^ ‘derivinv’ starts no excluded identifier.
	| WLowerVaridDerivin       annotation lexicalWLower                    (list varidInner)
		-- ^ ‘derivinw’ starts no excluded identifier.
	| XLowerVaridDerivin       annotation lexicalXLower                    (list varidInner)
		-- ^ ‘derivinx’ starts no excluded identifier.
	| YLowerVaridDerivin       annotation lexicalYLower                    (list varidInner)
		-- ^ ‘deriviny’ starts no excluded identifier.
	| ZLowerVaridDerivin       annotation lexicalZLower                    (list varidInner)
		-- ^ ‘derivinz’ starts no excluded identifier.

-- | A non-symbolic variable (lowercase-style) identifier name, excluding
-- reserved names, after an ‘instanc’ was parsed from the beginning.
data VaridInstancBase list lexicalEndOfParse varidInnerSansAscSmallUnderscore varidInner lexicalALower lexicalBLower lexicalCLower lexicalDLower lexicalELower lexicalFLower lexicalGLower lexicalHLower lexicalILower lexicalJLower lexicalKLower lexicalLLower lexicalMLower lexicalNLower lexicalOLower lexicalPLower lexicalQLower lexicalRLower lexicalSLower lexicalTLower lexicalULower lexicalVLower lexicalWLower lexicalXLower lexicalYLower lexicalZLower annotation fixpoint =
	  EOPVaridInstanc          annotation lexicalEndOfParse
		-- ^ We're provided with a certification that no more characters will
		-- follow in whatever we're parsing, and ‘instanc’ is not excluded.
	| InnerSansAscVaridInstanc annotation varidInnerSansAscSmallUnderscore (list varidInner)
		-- ^ Characters we know aren't excluded are valid characters.
	| ALowerVaridInstanc       annotation lexicalALower                    (list varidInner)
		-- ^ ‘instanca’ starts no excluded identifier.
	| BLowerVaridInstanc       annotation lexicalBLower                    (list varidInner)
		-- ^ ‘instancb’ starts no excluded identifier.
	| CLowerVaridInstanc       annotation lexicalCLower                    (list varidInner)
		-- ^ ‘instancc’ starts no excluded identifier.
	| DLowerVaridInstanc       annotation lexicalDLower                    (list varidInner)
		-- ^ ‘instancd’ starts no excluded identifier.
	| ELowerVaridInstanc       annotation lexicalELower                    varidInner        (list varidInner)
		-- ^ ‘instance’ alone is excluded.  Additional characters is valid.
	| FLowerVaridInstanc       annotation lexicalFLower                    (list varidInner)
		-- ^ ‘instancf’ starts no excluded identifier.
	| GLowerVaridInstanc       annotation lexicalGLower                    (list varidInner)
		-- ^ ‘instancg’ starts no excluded identifier.
	| HLowerVaridInstanc       annotation lexicalHLower                    (list varidInner)
		-- ^ ‘instanch’ starts no excluded identifier.
	| ILowerVaridInstanc       annotation lexicalILower                    (list varidInner)
		-- ^ ‘instanci’ starts no excluded identifier.
	| JLowerVaridInstanc       annotation lexicalJLower                    (list varidInner)
		-- ^ ‘instancj’ starts no excluded identifier.
	| KLowerVaridInstanc       annotation lexicalKLower                    (list varidInner)
		-- ^ ‘instanck’ starts no excluded identifier.
	| LLowerVaridInstanc       annotation lexicalLLower                    (list varidInner)
		-- ^ ‘instancl’ starts no excluded identifier.
	| MLowerVaridInstanc       annotation lexicalMLower                    (list varidInner)
		-- ^ ‘instancm’ starts no excluded identifier.
	| NLowerVaridInstanc       annotation lexicalNLower                    (list varidInner)
		-- ^ ‘instancn’ starts no excluded identifier.
	| OLowerVaridInstanc       annotation lexicalOLower                    (list varidInner)
		-- ^ ‘instanco’ starts no excluded identifier.
	| PLowerVaridInstanc       annotation lexicalPLower                    (list varidInner)
		-- ^ ‘instancp’ starts no excluded identifier.
	| QLowerVaridInstanc       annotation lexicalQLower                    (list varidInner)
		-- ^ ‘instancq’ starts no excluded identifier.
	| RLowerVaridInstanc       annotation lexicalRLower                    (list varidInner)
		-- ^ ‘instancr’ starts no excluded identifier.
	| SLowerVaridInstanc       annotation lexicalSLower                    (list varidInner)
		-- ^ ‘instancs’ starts no excluded identifier.
	| TLowerVaridInstanc       annotation lexicalTLower                    (list varidInner)
		-- ^ ‘instanct’ starts no excluded identifier.
	| ULowerVaridInstanc       annotation lexicalULower                    (list varidInner)
		-- ^ ‘instancu’ starts no excluded identifier.
	| VLowerVaridInstanc       annotation lexicalVLower                    (list varidInner)
		-- ^ ‘instancv’ starts no excluded identifier.
	| WLowerVaridInstanc       annotation lexicalWLower                    (list varidInner)
		-- ^ ‘instancw’ starts no excluded identifier.
	| XLowerVaridInstanc       annotation lexicalXLower                    (list varidInner)
		-- ^ ‘instancx’ starts no excluded identifier.
	| YLowerVaridInstanc       annotation lexicalYLower                    (list varidInner)
		-- ^ ‘instancy’ starts no excluded identifier.
	| ZLowerVaridInstanc       annotation lexicalZLower                    (list varidInner)
		-- ^ ‘instancz’ starts no excluded identifier.

-- | A restricted 'symbol', without ascSymbol, to help build 'VarSymBase'.
data SymbolSansAscBase uniSymbolSansSpecialishAsc annotation fixpoint =
	UnicodeNonspecialNonscorequoteSymbolSansAsc annotation uniSymbolSansSpecialishAsc

-- | A Unicode symbol except for those found in 'special', the underscore, the
-- double and single quote characters, and ASCII symbols.
--
--  That it, it has no ASCII symbols.
data UniSymbolSansSpecialishAscBase lexicalUnicodeSymbolSansSpecialUnderscoreDoubleQuoteSingleQuoteAscii annotation fixpoint =
	UnicodeSymbolSansSpecialishAsc annotation lexicalUnicodeSymbolSansSpecialUnderscoreDoubleQuoteSingleQuoteAscii

-- | A restricted 'symbol', without the hyphen, to help build 'VarSymBase'.
data SymbolSansHyphenBase ascSymbolSansHyphen uniSymbolSansSpecialishHyphen annotation fixpoint =
	  AsciiNonspecialSymbolSansHyphen                annotation ascSymbolSansHyphen
	| UnicodeNonspecialNonscorequoteSymbolSansHyphen annotation uniSymbolSansSpecialishHyphen

-- | An ASCII symbol except special characters, underscore, quote characters, and hyphens.
--
-- This is to help build 'VarSymBase'.
data AscSymbolSansHyphenBase lexicalExclamation lexicalHash lexicalDollar lexicalPercent lexicalAmpersand lexicalAsterisk lexicalPlus lexicalDot lexicalSlash lexicalLeftAngleBracket lexicalEquals lexicalRightAngleBracket lexicalQuestionMark lexicalAt lexicalBackslash lexicalCaret lexicalPipe lexicalTilde lexicalColon annotation fixpoint =
	  ExclamationAsciiSymbolSansHyphen       annotation lexicalExclamation
	| HashAsciiSymbolSansHyphen              annotation lexicalHash
	| DollarAsciiSymbolSansHyphen            annotation lexicalDollar
	| PercentAsciiSymbolSansHyphen           annotation lexicalPercent
	| AmpersandAsciiSymbolSansHyphen         annotation lexicalAmpersand
	| AsteriskAsciiSymbolSansHyphen          annotation lexicalAsterisk
	| PlusAsciiSymbolSansHyphen              annotation lexicalPlus
	| DotAsciiSymbolSansHyphen               annotation lexicalDot
	| SlashAsciiSymbolSansHyphen             annotation lexicalSlash
	| LeftAngleBracketAsciiSymbolSansHyphen  annotation lexicalLeftAngleBracket
	| EqualsAsciiSymbolSansHyphen            annotation lexicalEquals
	| RightAngleBracketAsciiSymbolSansHyphen annotation lexicalRightAngleBracket
	| QuestionMarkAsciiSymbolSansHyphen      annotation lexicalQuestionMark
	| AtAsciiSymbolSansHyphen                annotation lexicalAt
	| BackslashAsciiSymbolSansHyphen         annotation lexicalBackslash
	| CaretAsciiSymbolSansHyphen             annotation lexicalCaret
	| PipeAsciiSymbolSansHyphen              annotation lexicalPipe
	| TildeAsciiSymbolSansHyphen             annotation lexicalTilde
	| ColonAsciiSymbolSansHyphen             annotation lexicalColon

-- | A Unicode symbol except for those found in 'special', the underscore, the
-- the double and single quote characters, and the hyphen.
--
-- This is a restricted version of 'UniSymbolSansSpecialishBase' to help build 'VarSymBase'.
data UniSymbolSansSpecialishHyphenBase lexicalUnicodeSymbolSansSpecialUnderscoreDoubleQuoteSingleQuoteHyphen annotation fixpoint =
	UnicodeSymbolSansSpecialishHyphen annotation lexicalUnicodeSymbolSansSpecialUnderscoreDoubleQuoteSingleQuoteHyphen

-- | A symbolic variable (lowercase-style) identifier name, excluding reserved
-- names and dash-only sequences.
--
-- Here we just provide a simpler enumeration of possibilities.
data VarSymBase list symbolSansAsc symbol lexicalExclamation lexicalHash lexicalDollar lexicalPercent lexicalAmpersand lexicalAsterisk lexicalPlus lexicalDot lexicalSlash lexicalLeftAngleBracket lexicalRightAngleBracket lexicalQuestionMark lexicalCaret lexicalHyphen lexicalEquals lexicalAt lexicalBackslash lexicalPipe lexicalTilde lexicalColon symbolSansHyphen annotation fixpoint =
	  UniVarSym                               annotation symbolSansAsc            (list symbol)
	-- ^ A single-symbol non-ASCII name is valid (we know it isn't ‘:’), as is
	-- anything that starts with it.

	| ExclamationVarSym                       annotation lexicalExclamation       (list symbol)
	-- ^ ‘!’* is valid.
	| HashVarSym                              annotation lexicalHash              (list symbol)
	-- ^ ‘#’* is valid.
	| DollarVarSym                            annotation lexicalDollar            (list symbol)
	-- ^ ‘$’* is valid.
	| PercentVarSym                           annotation lexicalPercent           (list symbol)
	-- ^ ‘%’* is valid.
	| AmpersandVarSym                         annotation lexicalAmpersand         (list symbol)
	-- ^ ‘&’* is valid.
	| AsteriskVarSym                          annotation lexicalAsterisk          (list symbol)
	-- ^ ‘*’* is valid.
	| PlusVarSym                              annotation lexicalPlus              (list symbol)
	-- ^ ‘+’* is valid.
	| DotVarSym                               annotation lexicalDot
	-- ^ ‘.’ is valid.
	| SlashVarSym                             annotation lexicalSlash             (list symbol)
	-- ^ ‘/’* is valid.
	| LeftAngleBracketVarSym                  annotation lexicalLeftAngleBracket
	-- ^ ‘<’ is valid.
	--EqualVarSym
	--   ‘=’ is not valid.
	| RightAngleBracketVarSym                 annotation lexicalRightAngleBracket (list symbol)
	-- ^ ‘>’* is valid.
	| QuestionMarkVarSym                      annotation lexicalQuestionMark      (list symbol)
	-- ^ ‘?’* is valid.
	--AtVarSym
	--   ‘@’ is not valid.
	--BackslashVarSym
	--   ‘\’ is not valid.
	| CaretVarSym                             annotation lexicalCaret             (list symbol)
	-- ^ ‘^’* is valid.
	--PipeVarSym
	--   ‘|’ is not valid.
	| HyphenVarSym                            annotation lexicalHyphen
	-- ^ ‘-’ is valid.
	--TildeVarSym
	--   ‘~’ is not valid.
	--ColonVarSym
	--   ‘:’ is not valid.

	| DotExclamationVarSym                    annotation lexicalDot               lexicalExclamation        (list symbol)
	-- ^ ‘.!’* is valid.
	| DotHashVarSym                           annotation lexicalDot               lexicalHash               (list symbol)
	-- ^ ‘.#’* is valid.
	| DotDollarVarSym                         annotation lexicalDot               lexicalDollar             (list symbol)
	-- ^ ‘.$’* is valid.
	| DotPercentVarSym                        annotation lexicalDot               lexicalPercent            (list symbol)
	-- ^ ‘.%’* is valid.
	| DotAmpersandVarSym                      annotation lexicalDot               lexicalAmpersand          (list symbol)
	-- ^ ‘.&’* is valid.
	| DotAsteriskVarSym                       annotation lexicalDot               lexicalAsterisk           (list symbol)
	-- ^ ‘.*’* is valid.
	| DotPlusVarSym                           annotation lexicalDot               lexicalPlus               (list symbol)
	-- ^ ‘.+’* is valid.
	--DotDotVarSym
	--   ‘..’ is not valid.
	| DotSlashVarSym                          annotation lexicalDot               lexicalSlash              (list symbol)
	-- ^ ‘./’* is valid.
	| DotLeftAngleBracketVarSym               annotation lexicalDot               lexicalLeftAngleBracket   (list symbol)
	-- ^ ‘.<’* is valid.
	| DotEqualsVarSym                         annotation lexicalDot               lexicalEquals             (list symbol)
	-- ^ ‘.=’* is valid.
	| DotRightAngleBracketVarSym              annotation lexicalDot               lexicalRightAngleBracket  (list symbol)
	-- ^ ‘.>’* is valid.
	| DotQuestionMarkVarSym                   annotation lexicalDot               lexicalQuestionMark       (list symbol)
	-- ^ ‘.?’* is valid.
	| DotAtVarSym                             annotation lexicalDot               lexicalAt                 (list symbol)
	-- ^ ‘.@’* is valid.
	| DotBackslashVarSym                      annotation lexicalDot               lexicalBackslash          (list symbol)
	-- ^ ‘.\’* is valid.
	| DotCaretVarSym                          annotation lexicalDot               lexicalCaret              (list symbol)
	-- ^ ‘.^’* is valid.
	| DotPipeVarSym                           annotation lexicalDot               lexicalPipe               (list symbol)
	-- ^ ‘.|’* is valid.
	| DotHyphenVarSym                         annotation lexicalDot               lexicalHyphen             (list symbol)
	-- ^ ‘.-’* is valid.
	| DotTildeVarSym                          annotation lexicalDot               lexicalTilde              (list symbol)
	-- ^ ‘.~’* is valid.
	| DotColonVarSym                          annotation lexicalDot               lexicalColon              (list symbol)
	-- ^ ‘.:’* is valid.

	| LeftAngleBracketExclamationVarSym       annotation lexicalLeftAngleBracket  lexicalExclamation        (list symbol)
	-- ^ ‘<!’* is valid.
	| LeftAngleBracketHashVarSym              annotation lexicalLeftAngleBracket  lexicalHash               (list symbol)
	-- ^ ‘<#’* is valid.
	| LeftAngleBracketDollarVarSym            annotation lexicalLeftAngleBracket  lexicalDollar             (list symbol)
	-- ^ ‘<$’* is valid.
	| LeftAngleBracketPercentVarSym           annotation lexicalLeftAngleBracket  lexicalPercent            (list symbol)
	-- ^ ‘<%’* is valid.
	| LeftAngleBracketAmpersandVarSym         annotation lexicalLeftAngleBracket  lexicalAmpersand          (list symbol)
	-- ^ ‘<&’* is valid.
	| LeftAngleBracketAsteriskVarSym          annotation lexicalLeftAngleBracket  lexicalAsterisk           (list symbol)
	-- ^ ‘<*’* is valid.
	| LeftAngleBracketPlusVarSym              annotation lexicalLeftAngleBracket  lexicalPlus               (list symbol)
	-- ^ ‘<+’* is valid.
	| LeftAngleBracketDotVarSym               annotation lexicalLeftAngleBracket  lexicalDot                (list symbol)
	-- ^ ‘<.’* is valid.
	| LeftAngleBracketSlashVarSym             annotation lexicalLeftAngleBracket  lexicalSlash              (list symbol)
	-- ^ ‘</’* is valid.
	| LeftAngleBracketLeftAngleBracketVarSym  annotation lexicalLeftAngleBracket  lexicalLeftAngleBracket   (list symbol)
	-- ^ ‘<<’* is valid.
	| LeftAngleBracketEqualsVarSym            annotation lexicalLeftAngleBracket  lexicalEquals             (list symbol)
	-- ^ ‘<=’* is valid.
	| LeftAngleBracketRightAngleBracketVarSym annotation lexicalLeftAngleBracket  lexicalRightAngleBracket  (list symbol)
	-- ^ ‘<>’* is valid.
	| LeftAngleBracketQuestionMarkVarSym      annotation lexicalLeftAngleBracket  lexicalQuestionMark       (list symbol)
	-- ^ ‘<?’* is valid.
	| LeftAngleBracketAtVarSym                annotation lexicalLeftAngleBracket  lexicalAt                 (list symbol)
	-- ^ ‘<@’* is valid.
	| LeftAngleBracketBackslashVarSym         annotation lexicalLeftAngleBracket  lexicalBackslash          (list symbol)
	-- ^ ‘<\’* is valid.
	| LeftAngleBracketCaretVarSym             annotation lexicalLeftAngleBracket  lexicalCaret              (list symbol)
	-- ^ ‘<^’* is valid.
	| LeftAngleBracketPipeVarSym              annotation lexicalLeftAngleBracket  lexicalPipe               (list symbol)
	-- ^ ‘<|’* is valid.
	--LeftAngleBracketHyphenVarSym
	--   ‘<-’ is not valid.
	| LeftAngleBracketTildeVarSym             annotation lexicalLeftAngleBracket  lexicalTilde              (list symbol)
	-- ^ ‘<~’* is valid.
	| LeftAngleBracketColonVarSym             annotation lexicalLeftAngleBracket  lexicalColon              (list symbol)
	-- ^ ‘<:’* is valid.

	| EqualsExclamationVarSym                 annotation lexicalEquals            lexicalExclamation        (list symbol)
	-- ^ ‘=!’* is valid.
	| EqualsHashVarSym                        annotation lexicalEquals            lexicalHash               (list symbol)
	-- ^ ‘=#’* is valid.
	| EqualsDollarVarSym                      annotation lexicalEquals            lexicalDollar             (list symbol)
	-- ^ ‘=$’* is valid.
	| EqualsPercentVarSym                     annotation lexicalEquals            lexicalPercent            (list symbol)
	-- ^ ‘=%’* is valid.
	| EqualsAmpersandVarSym                   annotation lexicalEquals            lexicalAmpersand          (list symbol)
	-- ^ ‘=&’* is valid.
	| EqualsAsteriskVarSym                    annotation lexicalEquals            lexicalAsterisk           (list symbol)
	-- ^ ‘=*’* is valid.
	| EqualsPlusVarSym                        annotation lexicalEquals            lexicalPlus               (list symbol)
	-- ^ ‘=+’* is valid.
	| EqualsDotVarSym                         annotation lexicalEquals            lexicalDot                (list symbol)
	-- ^ ‘=.’* is valid.
	| EqualsSlashVarSym                       annotation lexicalEquals            lexicalSlash              (list symbol)
	-- ^ ‘=/’* is valid.
	| EqualsLeftAngleBracketVarSym            annotation lexicalEquals            lexicalLeftAngleBracket   (list symbol)
	-- ^ ‘=<’* is valid.
	| EqualsEqualsVarSym                      annotation lexicalEquals            lexicalEquals             (list symbol)
	-- ^ ‘==’* is valid.
	--EqualsRightAngleBracketVarSym
	--   ‘=>’ is not valid.
	| EqualsQuestionMarkVarSym                annotation lexicalEquals            lexicalQuestionMark       (list symbol)
	-- ^ ‘=?’* is valid.
	| EqualsAtVarSym                          annotation lexicalEquals            lexicalAt                 (list symbol)
	-- ^ ‘=@’* is valid.
	| EqualsBackslashVarSym                   annotation lexicalEquals            lexicalBackslash          (list symbol)
	-- ^ ‘=\’* is valid.
	| EqualsCaretVarSym                       annotation lexicalEquals            lexicalCaret              (list symbol)
	-- ^ ‘=^’* is valid.
	| EqualsPipeVarSym                        annotation lexicalEquals            lexicalPipe               (list symbol)
	-- ^ ‘=|’* is valid.
	| EqualsHyphenVarSym                      annotation lexicalEquals            lexicalHyphen             (list symbol)
	-- ^ ‘=-’* is valid.
	| EqualsTildeMarkVarSym                   annotation lexicalEquals            lexicalTilde              (list symbol)
	-- ^ ‘=~’* is valid.
	| EqualsColonMarkVarSym                   annotation lexicalEquals            lexicalColon              (list symbol)
	-- ^ ‘=:’* is valid.

	| AtVarSym                                annotation lexicalAt                symbol                    (list symbol)
	-- ^ ‘@’?* is valid.

	| BackslashVarSym                         annotation lexicalBackslash         symbol                    (list symbol)
	-- ^ ‘\’?* is valid.

	| PipeVarSym                              annotation lexicalPipe              symbol                    (list symbol)
	-- ^ ‘|’?* is valid.

	| HyphenExclamationVarSym                 annotation lexicalHyphen            lexicalExclamation        (list symbol)
	-- ^ ‘-!’* is valid.
	| HyphenHashVarSym                        annotation lexicalHyphen            lexicalHash               (list symbol)
	-- ^ ‘-#’* is valid.
	| HyphenDollarVarSym                      annotation lexicalHyphen            lexicalDollar             (list symbol)
	-- ^ ‘-$’* is valid.
	| HyphenPercentVarSym                     annotation lexicalHyphen            lexicalPercent            (list symbol)
	-- ^ ‘-%’* is valid.
	| HyphenAmpersandVarSym                   annotation lexicalHyphen            lexicalAmpersand          (list symbol)
	-- ^ ‘-&’* is valid.
	| HyphenAsteriskVarSym                    annotation lexicalHyphen            lexicalAsterisk           (list symbol)
	-- ^ ‘-*’* is valid.
	| HyphenPlusVarSym                        annotation lexicalHyphen            lexicalPlus               (list symbol)
	-- ^ ‘-+’* is valid.
	| HyphenDotVarSym                         annotation lexicalHyphen            lexicalDot                (list symbol)
	-- ^ ‘-.’* is valid.
	| HyphenSlashVarSym                       annotation lexicalHyphen            lexicalSlash              (list symbol)
	-- ^ ‘-/’* is valid.
	| HyphenLeftAngleBracketVarSym            annotation lexicalHyphen            lexicalLeftAngleBracket   (list symbol)
	-- ^ ‘-<’* is valid.
	| HyphenEqualsVarSym                      annotation lexicalHyphen            lexicalEquals             (list symbol)
	-- ^ ‘-=’* is valid.
	--HyphenRightAngleBracketVarSym
	--   ‘->’ is not valid.
	| HyphenQuestionMarkVarSym                annotation lexicalHyphen            lexicalQuestionMark       (list symbol)
	-- ^ ‘-?’* is valid.
	| HyphenAtVarSym                          annotation lexicalHyphen            lexicalAt                 (list symbol)
	-- ^ ‘-@’* is valid.
	| HyphenBackslashVarSym                   annotation lexicalHyphen            lexicalBackslash          (list symbol)
	-- ^ ‘-\’* is valid.
	| HyphenCaretVarSym                       annotation lexicalHyphen            lexicalCaret              (list symbol)
	-- ^ ‘-^’* is valid.
	| HyphenPipeVarSym                        annotation lexicalHyphen            lexicalPipe               (list symbol)
	-- ^ ‘-|’* is valid.
	--HyphenHyphenVarSym
	--   ‘--’ is not valid.
	| HyphenTildeVarSym                       annotation lexicalHyphen            lexicalTilde              (list symbol)
	-- ^ ‘-~’* is valid.
	| HyphenColonVarSym                       annotation lexicalHyphen            lexicalColon              (list symbol)
	-- ^ ‘-:’* is valid.

	| TildeVarSym                             annotation lexicalTilde             symbol                    (list symbol)
	-- ^ ‘~’?* is valid.

	| ColonExclamationVarSym                  annotation lexicalColon             lexicalExclamation        (list symbol)
	-- ^ ‘:!’* is valid.
	| ColonHashVarSym                         annotation lexicalColon             lexicalHash               (list symbol)
	-- ^ ‘:#’* is valid.
	| ColonDollarVarSym                       annotation lexicalColon             lexicalDollar             (list symbol)
	-- ^ ‘:$’* is valid.
	| ColonPercentVarSym                      annotation lexicalColon             lexicalPercent            (list symbol)
	-- ^ ‘:%’* is valid.
	| ColonAmpersandVarSym                    annotation lexicalColon             lexicalAmpersand          (list symbol)
	-- ^ ‘:&’* is valid.
	| ColonAsteriskVarSym                     annotation lexicalColon             lexicalAsterisk           (list symbol)
	-- ^ ‘:*’* is valid.
	| ColonPlusVarSym                         annotation lexicalColon             lexicalPlus               (list symbol)
	-- ^ ‘:+’* is valid.
	| ColonDotVarSym                          annotation lexicalColon             lexicalDot                (list symbol)
	-- ^ ‘:.’* is valid.
	| ColonSlashVarSym                        annotation lexicalColon             lexicalSlash              (list symbol)
	-- ^ ‘:/’* is valid.
	| ColonLeftAngleBracketVarSym             annotation lexicalColon             lexicalLeftAngleBracket   (list symbol)
	-- ^ ‘:<’* is valid.
	| ColonEqualsVarSym                       annotation lexicalColon             lexicalEquals             (list symbol)
	-- ^ ‘:=’* is valid.
	| ColonRightAngleBracketVarSym            annotation lexicalColon             lexicalRightAngleBracket  (list symbol)
	-- ^ ‘:>’* is valid.
	| ColonQuestionMarkVarSym                 annotation lexicalColon             lexicalQuestionMark       (list symbol)
	-- ^ ‘:?’* is valid.
	| ColonAtVarSym                           annotation lexicalColon             lexicalAt                 (list symbol)
	-- ^ ‘:@’* is valid.
	| ColonBackslashVarSym                    annotation lexicalColon             lexicalBackslash          (list symbol)
	-- ^ ‘:\’* is valid.
	| ColonCaretVarSym                        annotation lexicalColon             lexicalCaret              (list symbol)
	-- ^ ‘:^’* is valid.
	| ColonPipeVarSym                         annotation lexicalColon             lexicalPipe               (list symbol)
	-- ^ ‘:|’* is valid.
	| ColonHyphenVarSym                       annotation lexicalColon             lexicalHyphen             (list symbol)
	-- ^ ‘:-’* is valid.
	| ColonTildeVarSym                        annotation lexicalColon             lexicalTilde              (list symbol)
	-- ^ ‘:~’* is valid.
	--ColonColonVarSym
	--   ‘::’ is not valid.

	| DotDotVarSym                            annotation lexicalDot               lexicalDot                symbol               (list symbol)
	-- ^ ‘..’?* is valid.

	| LeftAngleBracketHyphenVarSym            annotation lexicalLeftAngleBracket  lexicalHyphen             symbol               (list symbol)
	-- ^ ‘<-’?* is valid.

	| EqualsRightAngleBracketVarSym           annotation lexicalEquals            lexicalRightAngleBracket  symbol               (list symbol)
	-- ^ ‘=>’?* is valid.

	| HyphenRightAngleBracketVarSym           annotation lexicalHyphen            lexicalRightAngleBracket  symbol               (list symbol)
	-- ^ ‘->’?* is valid.

	| HyphenHyphenVarSym                      annotation lexicalHyphen            lexicalHyphen             (list lexicalHyphen) symbolSansHyphen (list symbol)
	-- ^ ‘/---*[^-].*/’ is valid; there just needs to be at least one non-dash character.

-- | A restricted 'symbol', without the colon, to help build 'ConSymBase'.
data SymbolSansColonBase ascSymbolSansColon uniSymbolSansSpecialishColon annotation fixpoint =
	  AsciiNonspecialSymbolSansColon                annotation ascSymbolSansColon
	| UnicodeNonspecialNonscorequoteSymbolSansColon annotation uniSymbolSansSpecialishColon

-- | An ASCII symbol except special characters, underscore, quote characters, and hyphens.
--
-- This is to help build 'VarSymBase'.
data AscSymbolSansColonBase lexicalExclamation lexicalHash lexicalDollar lexicalPercent lexicalAmpersand lexicalAsterisk lexicalPlus lexicalDot lexicalSlash lexicalLeftAngleBracket lexicalEquals lexicalRightAngleBracket lexicalQuestionMark lexicalAt lexicalBackslash lexicalCaret lexicalPipe lexicalHyphen lexicalTilde annotation fixpoint =
	  ExclamationAsciiSymbolSansColon       annotation lexicalExclamation
	| HashAsciiSymbolSansColon              annotation lexicalHash
	| DollarAsciiSymbolSansColon            annotation lexicalDollar
	| PercentAsciiSymbolSansColon           annotation lexicalPercent
	| AmpersandAsciiSymbolSansColon         annotation lexicalAmpersand
	| AsteriskAsciiSymbolSansColon          annotation lexicalAsterisk
	| PlusAsciiSymbolSansColon              annotation lexicalPlus
	| DotAsciiSymbolSansColon               annotation lexicalDot
	| SlashAsciiSymbolSansColon             annotation lexicalSlash
	| LeftAngleBracketAsciiSymbolSansColon  annotation lexicalLeftAngleBracket
	| EqualsAsciiSymbolSansColon            annotation lexicalEquals
	| RightAngleBracketAsciiSymbolSansColon annotation lexicalRightAngleBracket
	| QuestionMarkAsciiSymbolSansColon      annotation lexicalQuestionMark
	| AtAsciiSymbolSansColon                annotation lexicalAt
	| BackslashAsciiSymbolSansColon         annotation lexicalBackslash
	| CaretAsciiSymbolSansColon             annotation lexicalCaret
	| PipeAsciiSymbolSansColon              annotation lexicalPipe
	| HyphenAsciiSymbolSansColon            annotation lexicalHyphen
	| TildeAsciiSymbolSansColon             annotation lexicalTilde

-- | A Unicode symbol except for those found in 'special', the underscore, the
-- the double and single quote characters, and the colon.
--
-- This is a restricted version of 'UniSymbolSansSpecialishColon' to help build 'ConSymBase'.
data UniSymbolSansSpecialishColonBase lexicalUnicodeSymbolSansSpecialUnderscoreDoubleQuoteSingleQuoteColon annotation fixpoint =
	UnicodeSymbolSansSpecialishColon annotation lexicalUnicodeSymbolSansSpecialUnderscoreDoubleQuoteSingleQuoteColon

-- | A symbolic constructor (capitalized-style) identifier name, excluding
-- reserved names.
--
-- After exclusions, this is all symbol lists starting with ‘:’ excluding ‘:’
-- and ‘::’.
data ConSymBase list lexicalColon symbol symbolSansColon annotation fixpoint =
	  Len3ConSym annotation lexicalColon symbol          symbol        (list symbol)
		-- ^ 3+ symbols.
	| Len2ConSym annotation lexicalColon symbolSansColon (list symbol)
		-- ^ 2-only symbols.
