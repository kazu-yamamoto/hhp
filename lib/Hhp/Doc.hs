module Hhp.Doc (
    showPage
  , showOneLine
  , getStyle
  , styleUnqualified
  ) where

import GHC (Ghc, DynFlags, getPrintUnqual, pprCols)
import GHC.Utils.Outputable (PprStyle, SDoc, neverQualify, runSDoc, PrintUnqualified, PprStyle, Depth(..), mkUserStyle)
import GHC.Driver.Session (initSDocContext)
import GHC.Utils.Ppr (Mode(..), Style(..), renderStyle, style)

import Hhp.Gap

----------------------------------------------------------------

showPage :: DynFlags -> PprStyle -> SDoc -> String
showPage = showSDocWithMode pagemode

showOneLine :: DynFlags -> PprStyle -> SDoc -> String
showOneLine = showSDocWithMode OneLineMode

showSDocWithMode :: Mode -> DynFlags -> PprStyle -> SDoc -> String
showSDocWithMode md dflags pprstyle sdoc = renderStyle style' doc
  where
    ctx = initSDocContext dflags pprstyle
    doc = runSDoc sdoc ctx
    style' = style { mode = md, lineLength = pprCols dflags }

----------------------------------------------------------------

getStyle :: Ghc PprStyle
getStyle = makeUserStyle <$> getPrintUnqual

styleUnqualified :: PprStyle
styleUnqualified = makeUserStyle neverQualify

makeUserStyle :: PrintUnqualified -> PprStyle
makeUserStyle pu = mkUserStyle pu AllTheWay
