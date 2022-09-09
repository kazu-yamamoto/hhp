{-# LANGUAGE BangPatterns #-}

module Hhp.Logger (
    withLogger
  , checkErrorPrefix
  , getSrcSpan
  ) where

import GHC (Ghc, DynFlags(..), SrcSpan(..))
import qualified GHC as G
import GHC.Data.Bag (bagToList)
import GHC.Data.FastString (unpackFS)
import GHC.Driver.Flags (WarnReason)
import GHC.Driver.Session (dopt, DumpFlag(Opt_D_dump_splices))
import GHC.Utils.Error (Severity(..), errMsgSpan)
import GHC.Utils.Monad (liftIO)
import GHC.Utils.Outputable (PprStyle, SDoc, defaultDumpStyle)

import Control.Monad.Catch (handle)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import System.FilePath (normalise)

import Hhp.Doc (showPage, getStyle)
import Hhp.Gap
import Hhp.GHCApi (withDynFlags, withCmdFlags)
import Hhp.Types (Options(..), convert)

----------------------------------------------------------------

type LogInfo = (DynFlags,WarnReason,Severity,SrcSpan,SDoc)

newtype LogRef = LogRef (IORef ([LogInfo] -> [LogInfo]))

newLogRef :: IO LogRef
newLogRef = LogRef <$> newIORef id

readAndClearLogRef :: Options -> LogRef -> IO String
readAndClearLogRef opt (LogRef ref) = do
    build <- readIORef ref
    writeIORef ref id
    let logInfos = build []
        logmsg = concat $ map ppMsg logInfos
    return $! convert opt logmsg

appendLogRef :: LogRef -> LogAction
appendLogRef (LogRef ref) df wr sev src msg = do
    let !l = (df, wr, sev, src, msg)
    modifyIORef ref (\b -> b . (l:))

----------------------------------------------------------------

-- | Set the session flag (e.g. "-Wall" or "-w:") then
--   executes a body. Log messages are returned as 'String'.
--   Right is success and Left is failure.
withLogger :: Options -> (DynFlags -> DynFlags) -> Ghc () -> Ghc (Either String String)
withLogger opt setDF body = handle (sourceError opt) $ do
    logref <- liftIO newLogRef
    withDynFlags setDF $ do
        withCmdFlags wflags $ do
            setLogger $ appendLogRef logref
            body
            liftIO $ Right <$> readAndClearLogRef opt logref
  where
    wflags = filter ("-fno-warn" `isPrefixOf`) $ ghcOpts opt

----------------------------------------------------------------

-- | Converting 'SourceError' to 'String'.
sourceError :: Options -> SourceError -> Ghc (Either String String)
sourceError opt err = do
    dflag <- G.getSessionDynFlags
    style <- getStyle
    let ret = convert opt . errBagToStrList dflag style . srcErrorMessages $ err
    return (Left ret)

errBagToStrList :: DynFlags -> PprStyle -> ErrorMessages -> [String]
errBagToStrList dflag style = map (ppErrMsg style) . reverse . bagToList
  where
    ppErrMsg _style_fixme err = ppMsg (dflag,undefined,SevError,spn, msg) -- ++ ext
       where
         spn = errMsgSpan err
         msg = pprLocErrMessage err
         -- fixme
    --     ext = showPage dflag style (pprLocErrMsg $ errMsgReason err)

ppMsg :: LogInfo -> String
ppMsg (dflag,_,sev,spn,msg)
  | isDumpSplices dflag = cts
  | otherwise           = prefix ++ cts
  where
    cts = showPage dflag defaultDumpStyle msg
    prefix = fromMaybe checkErrorPrefix $ do
        (line,col,_,_) <- getSrcSpan spn
        file <- normalise <$> getSrcFile spn
        let severityCaption = showSeverityCaption sev
        return $ file ++ ":" ++ show line ++ ":" ++ show col ++ ":" ++ severityCaption

checkErrorPrefix :: String
checkErrorPrefix = "Dummy:0:0:Error:"

showSeverityCaption :: Severity -> String
-- showSeverityCaption SevError is not necessary for historical reasons
showSeverityCaption SevWarning = "Warning: "
showSeverityCaption _          = ""

getSrcFile :: SrcSpan -> Maybe String
getSrcFile (G.RealSrcSpan spn _) = Just . unpackFS . G.srcSpanFile $ spn
getSrcFile _                     = Nothing

isDumpSplices :: DynFlags -> Bool
isDumpSplices dflag = dopt Opt_D_dump_splices dflag

getSrcSpan :: SrcSpan -> Maybe (Int,Int,Int,Int)
getSrcSpan (RealSrcSpan spn _) = Just ( G.srcSpanStartLine spn
                                      , G.srcSpanStartCol spn
                                      , G.srcSpanEndLine spn
                                      , G.srcSpanEndCol spn)
getSrcSpan _ = Nothing
