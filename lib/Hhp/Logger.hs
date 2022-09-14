{-# LANGUAGE CPP #-}
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
import GHC.Driver.Session (initSDocContext)
import GHC.Utils.Error (Severity(..)) -- errMsgSpan
import GHC.Utils.Monad (liftIO)
import GHC.Utils.Outputable (SDoc, SDocContext)

#if __GLASGOW_HASKELL__ >= 904
import GHC.Utils.Error (MessageClass(..))
import GHC.Utils.Logger (LogFlags(..))
#else
import GHC.Driver.Session (dopt, DumpFlag(Opt_D_dump_splices))
import Hhp.Doc (styleUnqualified)
#endif

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

type LogInfo = (Bool,SDocContext,Severity,SrcSpan,SDoc)

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
#if __GLASGOW_HASKELL__ >= 904
appendLogRef (LogRef ref) flag mc src msg = do
    let (dump,sev) = case mc of
          MCDiagnostic sev0 _ -> (False, sev0)
          _                   -> (True,  SevError) -- dummy
        ctx = log_default_user_context flag
        !l = (dump, ctx, sev, src, msg)
#else
appendLogRef (LogRef ref) flag _ sev src msg = do
    let ctx = initSDocContext flag styleUnqualified
        dump = isDumpSplices flag
        !l = (dump, ctx, sev, src, msg)
#endif
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
    let ctx = initSDocContext dflag style
        ret = convert opt . errBagToStrList ctx . srcErrorMessages $ err
    return (Left ret)

errBagToStrList :: SDocContext -> ErrorMessages -> [String]
errBagToStrList ctx = map ppErrMsg . reverse . bagToList . getMessages
  where
    ppErrMsg err = showPage ctx msg
       where
--         spn = errMsgSpan err
         msg = pprLocErrMessage err

ppMsg :: (Bool, SDocContext, Severity, SrcSpan, SDoc) -> String
ppMsg (True,ctx,_   ,_ ,msg) =           showPage ctx msg
ppMsg (_,   ctx,sev,spn,msg) = prefix ++ showPage ctx msg
  where
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

#if __GLASGOW_HASKELL__ < 904
isDumpSplices :: DynFlags -> Bool
isDumpSplices dflag = dopt Opt_D_dump_splices dflag
#endif

getSrcSpan :: SrcSpan -> Maybe (Int,Int,Int,Int)
getSrcSpan (RealSrcSpan spn _) = Just ( G.srcSpanStartLine spn
                                      , G.srcSpanStartCol spn
                                      , G.srcSpanEndLine spn
                                      , G.srcSpanEndCol spn)
getSrcSpan _ = Nothing
