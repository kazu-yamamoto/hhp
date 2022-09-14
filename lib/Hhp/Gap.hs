{-# LANGUAGE CPP #-}

module Hhp.Gap (
#if __GLASGOW_HASKELL__ >= 902
    module GHC.Types.TyThing.Ppr
#else
    module GHC.Core.Ppr.TyThing
#endif
  , implicitTyThings
  , pagemode
  , getUnitState
  , languagesAndExtensions
  , setEmptyLogger
  , setLinkerOptions
  , setLogger
  , LogAction
  , SourceError
  , srcErrorMessages
  , pprLocErrMessage
  , ErrorMessages
  , locA
  , LOC
  , guessTarget
  , pprSigmaType
  , getMessages
  , hsPatType
  ) where

import GHC (Ghc, DynFlags(..), GhcLink(..))
import qualified GHC as G
import GHC.Utils.Ppr(Mode(..))
import GHC.Unit.State (UnitState)
import GHC.Driver.Session (supportedLanguagesAndExtensions)
import GHC.Utils.Outputable (SDoc)

#if __GLASGOW_HASKELL__ >= 904
----------------------------------------------------------------
import GHC (Backend(..), pushLogHookM)
import GHC.Driver.Env (hsc_units)
import GHC.Driver.Errors.Types (ErrorMessages, GhcMessage)
import GHC.Hs.Syn.Type (hsPatType)
import GHC.Parser.Annotation (locA, LocatedA)
import GHC.Platform.Host (hostPlatformArchOS)
import GHC.Tc.Utils.TcType (pprSigmaType)
import GHC.Types.Error (getMessages)
import GHC.Types.SourceError (SourceError, srcErrorMessages)
import GHC.Types.TyThing (implicitTyThings)
import GHC.Types.TyThing.Ppr
import GHC.Utils.Error (pprLocMsgEnvelope, MsgEnvelope)
import GHC.Utils.Logger (LogAction)

pagemode :: Mode
pagemode = PageMode True

getUnitState :: Ghc UnitState
getUnitState = hsc_units <$> G.getSession

languagesAndExtensions :: [String]
languagesAndExtensions = supportedLanguagesAndExtensions hostPlatformArchOS

setEmptyLogger :: DynFlags -> DynFlags
setEmptyLogger = id

-- we don't want to generate object code so we compile to bytecode
-- (HscInterpreted) which implies LinkInMemory
-- HscInterpreted
setLinkerOptions :: DynFlags -> DynFlags
setLinkerOptions df = df {
    ghcLink = LinkInMemory
  , backend = Interpreter
  }

setLogger :: LogAction -> Ghc ()
setLogger logaction = pushLogHookM (\__defaultLogAction -> logaction)

pprLocErrMessage :: MsgEnvelope GhcMessage -> SDoc
pprLocErrMessage = pprLocMsgEnvelope

type LOC = LocatedA

guessTarget :: G.GhcMonad m => String -> m G.Target
guessTarget file = G.guessTarget file Nothing Nothing
----------------------------------------------------------------
#elif __GLASGOW_HASKELL__ >= 902
----------------------------------------------------------------
import GHC (Backend(..), pushLogHookM)
import GHC.Driver.Env (hsc_units)
import GHC.Parser.Annotation (locA, LocatedA)
import GHC.Platform.Host (hostPlatformArchOS)
import GHC.Tc.Utils.Zonk (hsPatType)
import GHC.Types.SourceError (SourceError, srcErrorMessages)
import GHC.Types.TyThing (implicitTyThings)
import GHC.Types.TyThing.Ppr
import GHC.Utils.Error (ErrorMessages, DecoratedSDoc, pprLocMsgEnvelope, MsgEnvelope)
import GHC.Utils.Logger (LogAction)

pagemode :: Mode
pagemode = PageMode True

getUnitState :: Ghc UnitState
getUnitState = hsc_units <$> G.getSession

languagesAndExtensions :: [String]
languagesAndExtensions = supportedLanguagesAndExtensions hostPlatformArchOS

setEmptyLogger :: DynFlags -> DynFlags
setEmptyLogger = id

-- we don't want to generate object code so we compile to bytecode
-- (HscInterpreted) which implies LinkInMemory
-- HscInterpreted
setLinkerOptions :: DynFlags -> DynFlags
setLinkerOptions df = df {
    ghcLink = LinkInMemory
  , backend = Interpreter
  }

setLogger :: LogAction -> Ghc ()
setLogger logaction = pushLogHookM (\__defaultLogAction -> logaction)

pprLocErrMessage :: MsgEnvelope DecoratedSDoc -> SDoc
pprLocErrMessage = pprLocMsgEnvelope

type LOC = LocatedA

guessTarget :: G.GhcMonad m => String -> m G.Target
guessTarget file = G.guessTarget file Nothing

pprSigmaType :: G.Type -> SDoc
pprSigmaType = pprTypeForUser

getMessages :: a -> a
getMessages = id

----------------------------------------------------------------
#else
----------------------------------------------------------------
import GHC (HscTarget(..), getSessionDynFlags, setSessionDynFlags, Located)
import GHC.Core.Ppr.TyThing
import GHC.Data.Bag (Bag)
import GHC.Driver.Session (LogAction)
import GHC.Driver.Types (SourceError, srcErrorMessages, implicitTyThings)
import GHC.Platform.Host (cHostPlatformMini)
import GHC.Tc.Utils.Zonk (hsPatType)
import GHC.Utils.Error (ErrMsg, pprLocErrMsg)

import Control.Monad (void)

pagemode :: Mode
pagemode = PageMode

getUnitState :: Ghc UnitState
getUnitState = G.unitState <$> G.getSessionDynFlags

languagesAndExtensions :: [String]
languagesAndExtensions = supportedLanguagesAndExtensions cHostPlatformMini

setEmptyLogger :: DynFlags -> DynFlags
setEmptyLogger df = df { G.log_action =  \_ _ _ _ _ -> return () }

-- we don't want to generate object code so we compile to bytecode
-- (HscInterpreted) which implies LinkInMemory
-- HscInterpreted
setLinkerOptions :: DynFlags -> DynFlags
setLinkerOptions df = df {
    ghcLink = LinkInMemory
  , hscTarget = HscInterpreted
  }

setLogger :: LogAction -> Ghc ()
setLogger logaction = do
    dflag <- getSessionDynFlags
    void $ setSessionDynFlags dflag { log_action = logaction }

pprLocErrMessage :: ErrMsg -> SDoc
pprLocErrMessage = pprLocErrMsg

type ErrorMessages = Bag ErrMsg

locA :: a -> a
locA = id

type LOC = Located

guessTarget :: G.GhcMonad m => String -> m G.Target
guessTarget file = G.guessTarget file Nothing

pprSigmaType :: G.Type -> SDoc
pprSigmaType = pprTypeForUser

getMessages :: a -> a
getMessages = id
----------------------------------------------------------------
#endif
