module Hhp.Doc (
    showPage,
    showOneLine,
    getStyle,
    styleUnqualified,
) where

import GHC (Ghc)
import GHC.Utils.Outputable (
    Depth (..),
    PprStyle,
    SDoc,
    SDocContext,
    mkUserStyle,
    neverQualify,
    runSDoc,
    sdocLineLength,
 )
import GHC.Utils.Ppr (Mode (..), Style (..), renderStyle, style)

import Hhp.Gap

----------------------------------------------------------------

showPage :: SDocContext -> SDoc -> String
showPage = showSDocWithMode pagemode

showOneLine :: SDocContext -> SDoc -> String
showOneLine = showSDocWithMode OneLineMode

showSDocWithMode :: Mode -> SDocContext -> SDoc -> String
showSDocWithMode md ctx sdoc = renderStyle style' doc
  where
    doc = runSDoc sdoc ctx
    style' = style{mode = md, lineLength = sdocLineLength ctx}

----------------------------------------------------------------

getStyle :: Ghc PprStyle
getStyle = makeUserStyle <$> getNamePprCtx

styleUnqualified :: PprStyle
styleUnqualified = makeUserStyle neverQualify

makeUserStyle :: NamePprCtx -> PprStyle
makeUserStyle pu = mkUserStyle pu AllTheWay
