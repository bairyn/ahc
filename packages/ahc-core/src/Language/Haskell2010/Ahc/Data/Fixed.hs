-- vim: set filetype=haskell noet

{-
 - Fixed.hs
 -
 - A simple provider of F-algebra style data recursion and type constructor fixpoints.
 -}

{-# LANGUAGE Haskell2010 #-}

-- | A provider of type construct of fixpoints through 'Fix'.
module Language.Haskell2010.Ahc.Data.Fixed (
	-- Export everything, explicitly.

	Fix(MkFix, _unFix),
) where

-- | Type constructor fixpoint.
data Fix f = MkFix { _unFix :: f (Fix f) }
