module Hhp.Things (
    GapThing (..),
    fromTyThing,
    infoThing,
) where

import GHC (Fixity, Ghc, TyCon, TyThing, Type)
import qualified GHC as G
import GHC.Core.ConLike (ConLike (..))
import GHC.Core.DataCon (dataConNonlinearType)
import GHC.Core.FamInstEnv (pprFamInsts)
import qualified GHC.Core.InstEnv as InstEnv
import GHC.Core.PatSyn (PatSyn)
import GHC.Types.Name.Set (elemNameSet, mkNameSet)
import GHC.Types.Var (varType)
import GHC.Utils.Outputable as Outputable

import Data.List (intersperse)
import Data.Maybe (catMaybes)

import Hhp.Gap

----------------------------------------------------------------

data GapThing
    = GtA Type
    | GtT TyCon
    | GtN
    | GtPatSyn PatSyn

fromTyThing :: TyThing -> GapThing
fromTyThing (G.AnId i) = GtA $ varType i
fromTyThing (G.AConLike (RealDataCon d)) = GtA $ dataConNonlinearType d
fromTyThing (G.AConLike (PatSynCon p)) = GtPatSyn p
fromTyThing (G.ATyCon t) = GtT t
fromTyThing _ = GtN

----------------------------------------------------------------

infoThing :: String -> Ghc SDoc
infoThing str = do
    names <- G.parseName str
    mb_stuffs <- mapM (G.getInfo False) names
    let filtered = filterOutChildren getTyThing $ catMaybes $ fromNE mb_stuffs
    return $ vcat (intersperse (text "") $ map (pprInfo . fixInfo) filtered)
  where
    getTyThing (t, _, _, _, _) = t
    fixInfo (t, f, cs, fs, _) = (t, f, cs, fs)

filterOutChildren :: (a -> TyThing) -> [a] -> [a]
filterOutChildren get_thing xs =
    [x | x <- xs, not (G.getName (get_thing x) `elemNameSet` implicits)]
  where
    implicits = mkNameSet [G.getName t | x <- xs, t <- implicitTyThings (get_thing x)]

pprInfo :: (TyThing, GHC.Fixity, [InstEnv.ClsInst], [G.FamInst]) -> SDoc
pprInfo (thing, fixity, insts, famInsts) =
    pprTyThingInContextLoc thing
        $$ show_fixity fixity
        $$ InstEnv.pprInstances insts
        $$ pprFamInsts famInsts
  where
    show_fixity fx
        | fx == G.defaultFixity = Outputable.empty
        | otherwise = ppr fx <+> ppr (G.getName thing)
