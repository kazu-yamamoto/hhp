-- | The Happy Haskell Programming library.
--   API for commands.
module Hhp (
    -- * Cradle
    Cradle (..),
    findCradle,

    -- * Options
    Options (..),
    LineSeparator (..),
    OutputStyle (..),
    defaultOptions,

    -- * Types
    ModuleString,
    Expression,

    -- * 'IO' utilities
    bootInfo,
    browseModule,
    checkSyntax,
    lintSyntax,
    expandTemplate,
    infoExpr,
    typeExpr,
    listModules,
    listLanguages,
    listFlags,
    debugInfo,
    rootInfo,
    packageDoc,
    findSymbol,

    -- * Misc
    cProjectVersion,
    cProjectVersionInt,
) where

import Hhp.Boot
import Hhp.Browse
import Hhp.Check
import Hhp.Cradle
import Hhp.Debug
import Hhp.Find
import Hhp.Flag
import Hhp.Info
import Hhp.Lang
import Hhp.Lint
import Hhp.List
import Hhp.PkgDoc
import Hhp.Types

import GHC.Settings.Config (cProjectVersion, cProjectVersionInt)
