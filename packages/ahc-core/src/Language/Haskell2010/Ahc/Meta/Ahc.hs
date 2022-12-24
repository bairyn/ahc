-- vim: set filetype=haskell noet

{-
 - Meta.Ahc
 -
 - Information about this package.
 -}

{-# LANGUAGE Haskell2010 #-}

module Language.Haskell2010.Ahc.Meta.Ahc (
	appName,
	appVersionStr,
	appVersionBaseComponents,
	appVersionBaseTags,
) where

import Prelude

appName :: String
appName = "ahc-minimal"

-- Note: be sure the version is consistent in the 4 places it appears:
-- 	- `CHANGELOG.md`
-- 	- `packages/ahc-core/CHANGELOG.md`
-- 	- `packages/ahc-core/ahc-core.cabal`
-- 	- `packages/ahc-core/src/Language/Haskell2010/Ahc/Meta/Ahc.hs`

appVersionStr :: String
appVersionStr = "0.1.0.0-dev"

appVersionBaseComponents :: [Integer]
appVersionBaseComponents = [0, 1, 0, 0]

appVersionBaseTags :: [String]
appVersionBaseTags = ["-dev"]
