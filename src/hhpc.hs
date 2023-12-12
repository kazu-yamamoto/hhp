{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Control.Exception (ErrorCall (..), Exception, Handler (..))
import qualified Control.Exception as E
import Data.Typeable (Typeable)
import Data.Version (showVersion)
import System.Console.GetOpt (ArgDescr (..), ArgOrder (..), OptDescr (..))
import qualified System.Console.GetOpt as O
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStr, hPutStrLn, hSetEncoding, stderr, stdout, utf8)

import Hhp
import Paths_hhp

----------------------------------------------------------------

progVersion :: String
progVersion =
    "hhpc version "
        ++ showVersion version
        ++ " compiled by GHC "
        ++ cProjectVersion
        ++ "\n"

ghcOptHelp :: String
ghcOptHelp = " [-g GHC_opt1 -g GHC_opt2 ...] "

usage :: String
usage =
    progVersion
        ++ "Usage:\n"
        ++ "\t hhpc list"
        ++ ghcOptHelp
        ++ "[-l] [-d]\n"
        ++ "\t hhpc lang [-l]\n"
        ++ "\t hhpc flag [-l]\n"
        ++ "\t hhpc browse"
        ++ ghcOptHelp
        ++ "[-l] [-o] [-d] [-q] [<package>:]<module> [[<package>:]<module> ...]\n"
        ++ "\t hhpc check"
        ++ ghcOptHelp
        ++ "<HaskellFiles...>\n"
        ++ "\t hhpc expand"
        ++ ghcOptHelp
        ++ "<HaskellFiles...>\n"
        ++ "\t hhpc debug"
        ++ ghcOptHelp
        ++ "\n"
        ++ "\t hhpc info"
        ++ ghcOptHelp
        ++ "<HaskellFile> <expression>\n"
        ++ "\t hhpc type"
        ++ ghcOptHelp
        ++ "<HaskellFile> <line-no> <column-no>\n"
        ++ "\t hhpc find <symbol>\n"
        ++ "\t hhpc lint [-h opt] <HaskellFile>\n"
        ++ "\t hhpc root\n"
        ++ "\t hhpc doc <module>\n"
        ++ "\t hhpc boot\n"
        ++ "\t hhpc version\n"
        ++ "\t hhpc help\n"

----------------------------------------------------------------

argspec :: [OptDescr (Options -> Options)]
argspec =
    [ Option
        "l"
        ["tolisp"]
        (NoArg (\opts -> opts{outputStyle = LispStyle}))
        "print as a list of Lisp"
    , Option
        "h"
        ["hlintOpt"]
        (ReqArg (\h opts -> opts{hlintOpts = h : hlintOpts opts}) "hlintOpt")
        "hlint options"
    , Option
        "g"
        ["ghcOpt"]
        (ReqArg (\g opts -> opts{ghcOpts = g : ghcOpts opts}) "ghcOpt")
        "GHC options"
    , Option
        "o"
        ["operators"]
        (NoArg (\opts -> opts{operators = True}))
        "print operators, too"
    , Option
        "d"
        ["detailed"]
        (NoArg (\opts -> opts{detailed = True}))
        "print detailed info"
    , Option
        "q"
        ["qualified"]
        (NoArg (\opts -> opts{qualified = True}))
        "show qualified names"
    , Option
        "b"
        ["boundary"]
        (ReqArg (\s opts -> opts{lineSeparator = LineSeparator s}) "sep")
        "specify line separator (default is Nul string)"
    ]

parseArgs :: [OptDescr (Options -> Options)] -> [String] -> (Options, [String])
parseArgs spec argv =
    case O.getOpt Permute spec argv of
        (o, n, []) -> (foldr id defaultOptions o, n)
        (_, _, errs) -> E.throw (CmdArg errs)

----------------------------------------------------------------

data HhpcError
    = SafeList
    | TooManyArguments String
    | NoSuchCommand String
    | CmdArg [String]
    | FileNotExist String
    deriving (Show, Typeable)

instance Exception HhpcError

----------------------------------------------------------------

main :: IO ()
main = flip E.catches handlers $ do
    hSetEncoding stdout utf8
    args <- getArgs
    let (opt, cmdArg) = parseArgs argspec args
    cradle <- findCradle
    let cmdArg0 = cmdArg !. 0
        cmdArg1 = cmdArg !. 1
        cmdArg2 = cmdArg !. 2
        cmdArg3 = cmdArg !. 3
        remainingArgs = tail cmdArg
        nArgs n f =
            if length remainingArgs == n
                then f
                else E.throw (TooManyArguments cmdArg0)
    res <- case cmdArg0 of
        "list" -> listModules opt cradle
        "lang" -> listLanguages opt
        "flag" -> listFlags opt
        "browse" -> concat <$> mapM (browseModule opt cradle) remainingArgs
        "check" -> checkSyntax opt cradle remainingArgs
        "expand" -> expandTemplate opt cradle remainingArgs
        "debug" -> debugInfo opt cradle
        "info" -> nArgs 2 infoExpr opt cradle cmdArg1 cmdArg2
        "type" -> nArgs 3 $ typeExpr opt cradle cmdArg1 (read cmdArg2) (read cmdArg3)
        "find" -> nArgs 1 $ findSymbol opt cradle cmdArg1
        "lint" -> nArgs 1 withFile (lintSyntax opt) cmdArg1
        "root" -> rootInfo opt cradle
        "doc" -> nArgs 1 $ packageDoc opt cradle cmdArg1
        "boot" -> bootInfo opt cradle
        "version" -> return progVersion
        "help" -> return $ O.usageInfo usage argspec
        cmd -> E.throw (NoSuchCommand cmd)
    putStr res
  where
    handlers = [Handler (handleThenExit handler1), Handler (handleThenExit handler2)]
    handleThenExit handler e = handler e >> exitFailure
    handler1 :: ErrorCall -> IO ()
    handler1 = print -- for debug
    handler2 :: HhpcError -> IO ()
    handler2 SafeList = printUsage
    handler2 (TooManyArguments cmd) = do
        hPutStrLn stderr $ "\"" ++ cmd ++ "\": Too many arguments"
        printUsage
    handler2 (NoSuchCommand cmd) = do
        hPutStrLn stderr $ "\"" ++ cmd ++ "\" not supported"
        printUsage
    handler2 (CmdArg errs) = do
        mapM_ (hPutStr stderr) errs
        printUsage
    handler2 (FileNotExist file) = do
        hPutStrLn stderr $ "\"" ++ file ++ "\" not found"
        printUsage
    printUsage = hPutStrLn stderr $ '\n' : O.usageInfo usage argspec
    withFile cmd file = do
        exist <- doesFileExist file
        if exist
            then cmd file
            else E.throw (FileNotExist file)
    xs !. idx
        | length xs <= idx = E.throw SafeList
        | otherwise = xs !! idx
