module Hhp.Browse (
    browseModule
  , browse
  ) where

import GHC (Ghc, GhcException(CmdLineError), ModuleInfo, Name, TyThing, DynFlags, Type, TyCon)
import qualified GHC as G
import GHC.Core.Ppr.TyThing (pprTypeForUser)
import GHC.Core.TyCon (isAlgTyCon)
import GHC.Core.Type (dropForAlls, splitFunTy_maybe, isPredTy, mkVisFunTy)
import GHC.Data.FastString (mkFastString)
import GHC.Types.Name (getOccString)

import Control.Monad.Catch (SomeException(..), handle, catch)
import Data.Char (isAlpha)
import Data.List (sort)
import Data.Maybe (catMaybes)

import Hhp.Doc (showPage, styleUnqualified)
import Hhp.GHCApi
import Hhp.Things
import Hhp.Types

----------------------------------------------------------------

-- | Getting functions, classes, etc from a module.
--   If 'detailed' is 'True', their types are also obtained.
--   If 'operators' is 'True', operators are also returned.
browseModule :: Options
             -> Cradle
             -> ModuleString -- ^ A module name. (e.g. \"Data.List\")
             -> IO String
browseModule opt cradle pkgmdl = withGHC' $ do
    initializeFlagsWithCradle opt cradle
    browse opt pkgmdl

-- | Getting functions, classes, etc from a module.
--   If 'detailed' is 'True', their types are also obtained.
--   If 'operators' is 'True', operators are also returned.
browse :: Options
       -> ModuleString -- ^ A module name. (e.g. \"Data.List\")
       -> Ghc String
browse opt pkgmdl = do
    convert opt . sort <$> (getModule >>= listExports)
  where
    (mpkg,mdl) = splitPkgMdl pkgmdl
    mdlname = G.mkModuleName mdl
    mpkgid = mkFastString <$> mpkg
    listExports Nothing       = return []
    listExports (Just mdinfo) = processExports opt mdinfo
    -- findModule works only for package modules, moreover,
    -- you cannot load a package module. On the other hand,
    -- to browse a local module you need to load it first.
    -- If CmdLineError is signalled, we assume the user
    -- tried browsing a local module.
    getModule = browsePackageModule `catch` fallback `catch` handler
    browsePackageModule = G.findModule mdlname mpkgid >>= G.getModuleInfo
    browseLocalModule = handle handler $ do
      setTargetFiles [mdl]
      G.findModule mdlname Nothing >>= G.getModuleInfo
    fallback (CmdLineError _) = browseLocalModule
    fallback _                = return Nothing
    handler (SomeException _) = return Nothing
-- |
--
-- >>> splitPkgMdl "base:Prelude"
-- (Just "base","Prelude")
-- >>> splitPkgMdl "Prelude"
-- (Nothing,"Prelude")
splitPkgMdl :: String -> (Maybe String,String)
splitPkgMdl pkgmdl = case break (==':') pkgmdl of
    (mdl,"")    -> (Nothing,mdl)
    (pkg,_:mdl) -> (Just pkg,mdl)

processExports :: Options -> ModuleInfo -> Ghc [String]
processExports opt minfo = mapM (showExport opt minfo) $ removeOps $ G.modInfoExports minfo
  where
    removeOps
      | operators opt = id
      | otherwise = filter (isAlpha . head . getOccString)

showExport :: Options -> ModuleInfo -> Name -> Ghc String
showExport opt minfo e = do
  mtype' <- mtype
  return $ concat $ catMaybes [mqualified, Just $ formatOp $ getOccString e, mtype']
  where
    mqualified = (G.moduleNameString (G.moduleName $ G.nameModule e) ++ ".") `justIf` qualified opt
    mtype
      | detailed opt = do
        tyInfo <- G.modInfoLookupName minfo e
        -- If nothing found, load dependent module and lookup global
        tyResult <- maybe (inOtherModule e) (return . Just) tyInfo
        dflag <- G.getSessionDynFlags
        return $ do
          typeName <- tyResult >>= showThing dflag
          (" :: " ++ typeName) `justIf` detailed opt
      | otherwise = return Nothing
    formatOp nm@(n:_)
      | isAlpha n = nm
      | otherwise = "(" ++ nm ++ ")"
    formatOp "" = error "formatOp"
    inOtherModule :: Name -> Ghc (Maybe TyThing)
    inOtherModule nm = G.getModuleInfo (G.nameModule nm) >> G.lookupGlobalName nm
    justIf :: a -> Bool -> Maybe a
    justIf x True = Just x
    justIf _ False = Nothing

showThing :: DynFlags -> TyThing -> Maybe String
showThing dflag tything = showThing' dflag (fromTyThing tything)

showThing' :: DynFlags -> GapThing -> Maybe String
showThing' dflag (GtA a) = Just $ formatType dflag a
showThing' _     (GtT t) = unwords . toList <$> tyType t
  where
    toList t' = t' : getOccString t : map getOccString (G.tyConTyVars t)
showThing' _     _       = Nothing

formatType :: DynFlags -> Type -> String
formatType dflag a = showOutputable dflag (removeForAlls a)

showOutputable :: DynFlags -> Type -> String
showOutputable dflag = unwords . lines . showPage dflag styleUnqualified . pprTypeForUser

tyType :: TyCon -> Maybe String
tyType typ
    | isAlgTyCon typ
      && not (G.isNewTyCon typ)
      && not (G.isClassTyCon typ) = Just "data"
    | G.isNewTyCon typ            = Just "newtype"
    | G.isClassTyCon typ          = Just "class"
    | G.isTypeSynonymTyCon typ    = Just "type"
    | otherwise                   = Nothing

removeForAlls :: Type -> Type
removeForAlls ty = removeForAlls' ty' tty'
  where
    ty'  = dropForAlls ty
    tty' = splitFunTy_maybe ty'

removeForAlls' :: Type -> Maybe (Type, Type, Type) -> Type
removeForAlls' ty Nothing = ty
removeForAlls' ty (Just (pre, ftype, x))
    | isPredTy pre        = mkVisFunTy pre (dropForAlls ftype) x
    | otherwise           = ty
