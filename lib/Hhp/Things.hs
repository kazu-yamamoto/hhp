module Hhp.Things (
    GapThing(..)
  , fromTyThing
  , infoThing
  ) where

import GHC
import GHC.Core.ConLike (ConLike(..))
import GHC.Core.FamInstEnv
import qualified GHC.Core.InstEnv as InstEnv
import GHC.Core.PatSyn
import GHC.Core.Ppr.TyThing
import GHC.Driver.Types
import GHC.Types.Name.Set
import GHC.Types.Var (varType)
import GHC.Utils.Outputable as Outputable

import Data.List (intersperse)
import Data.Maybe (catMaybes)

-- from ghc/InteractiveUI.hs

----------------------------------------------------------------

data GapThing = GtA Type
              | GtT TyCon
              | GtN
              | GtPatSyn PatSyn

fromTyThing :: TyThing -> GapThing
fromTyThing (AnId i)                   = GtA $ varType i
fromTyThing (AConLike (RealDataCon d)) = GtA $ dataConWrapperType d
fromTyThing (AConLike (PatSynCon p))   = GtPatSyn p
fromTyThing (ATyCon t)                 = GtT t
fromTyThing _                          = GtN

----------------------------------------------------------------

infoThing :: String -> Ghc SDoc
infoThing str = do
    names <- parseName str
    mb_stuffs <- mapM (getInfo False) names
    let filtered = filterOutChildren getTyThing $ catMaybes mb_stuffs
    return $ vcat (intersperse (text "") $ map (pprInfo . fixInfo) filtered)
  where
    getTyThing (t,_,_,_,_) = t
    fixInfo (t,f,cs,fs,_) = (t,f,cs,fs)

filterOutChildren :: (a -> TyThing) -> [a] -> [a]
filterOutChildren get_thing xs
    = [x | x <- xs, not (getName (get_thing x) `elemNameSet` implicits)]
  where
    implicits = mkNameSet [getName t | x <- xs, t <- implicitTyThings (get_thing x)]

pprInfo :: (TyThing, GHC.Fixity, [InstEnv.ClsInst], [FamInst]) -> SDoc
pprInfo (thing, fixity, insts, famInsts)
    = pprTyThingInContextLoc thing
   $$ show_fixity fixity
   $$ InstEnv.pprInstances insts
   $$ pprFamInsts famInsts
  where
    show_fixity fx
      | fx == defaultFixity = Outputable.empty
      | otherwise           = ppr fx <+> ppr (getName thing)
