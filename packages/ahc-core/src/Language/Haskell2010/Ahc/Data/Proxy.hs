-- vim: set filetype=haskell noet

{-
 - Proxy.hs
 -
 - A simple provider of unit types carrying extra type information.
 -}

{-# LANGUAGE Haskell2010 #-}

-- | A provider of type construct of fixpoints through 'Fix'.
module Language.Haskell2010.Ahc.Data.Proxy (
	-- Export everything, explicitly.

	Proxy(MkProxy),
) where

-- | Unit type with extra type-level data.
--
-- This can be used to make kind inference infer the correct kinds.
data Proxy a = MkProxy
