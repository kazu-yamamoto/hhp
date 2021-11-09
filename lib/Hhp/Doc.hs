module Hhp.Doc (
    showPage
  , showOneLine
  , getStyle
  , styleUnqualified
  ) where

import GHC (Ghc, DynFlags, getPrintUnqual, pprCols)
import GHC.Utils.Outputable (PprStyle, SDoc, neverQualify, initSDocContext, runSDoc, PrintUnqualified, PprStyle, Depth(AllTheWay), mkUserStyle)
import GHC.Utils.Ppr (Mode(..), Doc, Style(..), renderStyle, style)

makeUserStyle :: PrintUnqualified -> PprStyle
makeUserStyle sty = mkUserStyle sty AllTheWay

withPprStyleDoc :: DynFlags -> PprStyle -> SDoc -> Doc
withPprStyleDoc dflags sty d = runSDoc d (initSDocContext dflags sty)

showPage :: DynFlags -> PprStyle -> SDoc -> String
showPage dflag stl = showDocWith dflag PageMode . withPprStyleDoc dflag stl

showOneLine :: DynFlags -> PprStyle -> SDoc -> String
showOneLine dflag stl = showDocWith dflag OneLineMode . withPprStyleDoc dflag stl

getStyle :: Ghc PprStyle
getStyle = makeUserStyle <$> getPrintUnqual

styleUnqualified :: PprStyle
styleUnqualified = makeUserStyle neverQualify

showDocWith :: DynFlags -> Mode -> Doc -> String
showDocWith dflags md = renderStyle mstyle
  where
    mstyle = style { mode = md, lineLength = pprCols dflags }
