module Hhp.Doc where

import GHC (Ghc, DynFlags, getPrintUnqual, pprCols)
import Outputable (PprStyle, SDoc, withPprStyleDoc, neverQualify)
import Pretty (Mode(..), Doc, Style(..), renderStyle, style)

import Hhp.Gap (makeUserStyle)

showPage :: DynFlags -> PprStyle -> SDoc -> String
showPage dflag stl = showDocWith dflag PageMode . withPprStyleDoc dflag stl

showOneLine :: DynFlags -> PprStyle -> SDoc -> String
showOneLine dflag stl = showDocWith dflag OneLineMode . withPprStyleDoc dflag stl

getStyle :: DynFlags -> Ghc PprStyle
getStyle dflags = makeUserStyle dflags <$> getPrintUnqual

styleUnqualified :: DynFlags -> PprStyle
styleUnqualified dflags = makeUserStyle dflags neverQualify

showDocWith :: DynFlags -> Mode -> Doc -> String
showDocWith dflags md = renderStyle mstyle
  where
    mstyle = style { mode = md, lineLength = pprCols dflags }
