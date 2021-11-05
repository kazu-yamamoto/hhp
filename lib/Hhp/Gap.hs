{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, CPP #-}

module Hhp.Gap (
    WarnFlags
  , emptyWarnFlags
  , makeUserStyle
  , getModuleName
  , getTyThing
  , fixInfo
  , getModSummaries
  , LExpression
  , LBinding
  , LPattern
  , inTypes
  , outType
  , HsBindLR(..)
  , languagesAndExtensions
  , mkFunTy
  , mkFunTys
  , getModSummaryForMain
  ) where

import Data.List (find)

import DynFlags (DynFlags, supportedLanguagesAndExtensions)
import GHC(Ghc,getModuleGraph,moduleNameString,moduleName,ms_mod)

import GHC (LHsBind, LHsExpr, Type)
#if __GLASGOW_HASKELL__ >= 808
import GHC (Located, Pat)
#else
import GHC (LPat)
#endif
#if __GLASGOW_HASKELL__ >= 810
import GHC.Hs.Expr (MatchGroup)
#else
import HsExpr (MatchGroup)
#endif
import Outputable (PrintUnqualified, PprStyle, Depth(AllTheWay), mkUserStyle)

----------------------------------------------------------------
----------------------------------------------------------------

#if __GLASGOW_HASKELL__ >= 802
#else
import GHC.PackageDb (ExposedModule(..))
#endif

#if __GLASGOW_HASKELL__ >= 804
import DynFlags (WarningFlag)
import qualified EnumSet as E (EnumSet, empty)
import GHC (mgModSummaries, ModSummary, ModuleGraph)
#else
import qualified Data.IntSet as I (IntSet, empty)
import GHC (ModSummary)
#endif

#if __GLASGOW_HASKELL__ >= 810
import GHC.Hs.Expr (MatchGroupTc(..))
import GHC.Hs.Extension (GhcTc)
import GHC (mg_ext)
#elif __GLASGOW_HASKELL__ >= 806
import HsExpr (MatchGroupTc(..))
import HsExtension (GhcTc)
import GHC (mg_ext)
#elif __GLASGOW_HASKELL__ >= 804
import HsExtension (GhcTc)
import GHC (mg_res_ty, mg_arg_tys)
#else
import GHC (Id, mg_res_ty, mg_arg_tys)
#endif

#if __GLASGOW_HASKELL__ >= 810
import GHC.Hs.Binds (HsBindLR(..))
import GHC.Platform.Host
import Type (mkVisFunTy, mkVisFunTys)
#else
import HsBinds (HsBindLR(..))
import Type (mkFunTy, mkFunTys)
#endif

----------------------------------------------------------------
----------------------------------------------------------------

makeUserStyle :: DynFlags -> PrintUnqualified -> PprStyle
#if __GLASGOW_HASKELL__ >= 802
makeUserStyle dflags style = mkUserStyle dflags style AllTheWay
#else
makeUserStyle _      style = mkUserStyle        style AllTheWay
#endif

#if __GLASGOW_HASKELL__ >= 802
getModuleName :: (a, b) -> a
getModuleName = fst
#else
getModuleName :: ExposedModule unitid modulename -> modulename
getModuleName = exposedName
#endif

----------------------------------------------------------------

#if __GLASGOW_HASKELL__ >= 804
type WarnFlags = E.EnumSet WarningFlag
emptyWarnFlags :: WarnFlags
emptyWarnFlags = E.empty
#else
type WarnFlags = I.IntSet
emptyWarnFlags :: WarnFlags
emptyWarnFlags = I.empty
#endif

#if __GLASGOW_HASKELL__ >= 804
getModSummaries :: ModuleGraph -> [ModSummary]
getModSummaries = mgModSummaries

getTyThing :: (a, b, c, d, e) -> a
getTyThing (t,_,_,_,_) = t

fixInfo :: (a, b, c, d, e) -> (a, b, c, d)
fixInfo (t,f,cs,fs,_) = (t,f,cs,fs)
#else
getModSummaries :: a -> a
getModSummaries = id

getTyThing :: (a, b, c, d) -> a
getTyThing (t,_,_,_) = t

fixInfo :: (a, b, c, d) -> (a, b, c, d)
fixInfo = id
#endif

----------------------------------------------------------------

#if __GLASGOW_HASKELL__ >= 808
type LExpression = LHsExpr GhcTc
type LBinding    = LHsBind GhcTc
type LPattern    = Located (Pat GhcTc)

inTypes :: MatchGroup GhcTc LExpression -> [Type]
inTypes = mg_arg_tys . mg_ext
outType :: MatchGroup GhcTc LExpression -> Type
outType = mg_res_ty . mg_ext
#elif __GLASGOW_HASKELL__ >= 806
type LExpression = LHsExpr GhcTc
type LBinding    = LHsBind GhcTc
type LPattern    = LPat    GhcTc

inTypes :: MatchGroup GhcTc LExpression -> [Type]
inTypes = mg_arg_tys . mg_ext
outType :: MatchGroup GhcTc LExpression -> Type
outType = mg_res_ty . mg_ext
#elif __GLASGOW_HASKELL__ >= 804
type LExpression = LHsExpr GhcTc
type LBinding    = LHsBind GhcTc
type LPattern    = LPat    GhcTc

inTypes :: MatchGroup GhcTc LExpression -> [Type]
inTypes = mg_arg_tys
outType :: MatchGroup GhcTc LExpression -> Type
outType = mg_res_ty
#else
type LExpression = LHsExpr Id
type LBinding    = LHsBind Id
type LPattern    = LPat    Id

inTypes :: MatchGroup Id LExpression -> [Type]
inTypes = mg_arg_tys
outType :: MatchGroup Id LExpression -> Type
outType = mg_res_ty
#endif

----------------------------------------------------------------

languagesAndExtensions :: [String]
#if __GLASGOW_HASKELL__ >= 810
languagesAndExtensions = supportedLanguagesAndExtensions cHostPlatformMini
#else
languagesAndExtensions = supportedLanguagesAndExtensions
#endif

#if __GLASGOW_HASKELL__ >= 810
mkFunTy :: Type -> Type -> Type
mkFunTy  = mkVisFunTy

mkFunTys :: [Type] -> Type -> Type
mkFunTys = mkVisFunTys
#endif

----------------------------------------------------------------

getModSummaryForMain :: Ghc (Maybe ModSummary)
#if __GLASGOW_HASKELL__ >= 804
getModSummaryForMain = find isMain . mgModSummaries <$> getModuleGraph
#else
getModSummaryForMain = find isMain <$> getModuleGraph
#endif
  where
    isMain m = moduleNameString (moduleName (ms_mod m)) == "Main"
