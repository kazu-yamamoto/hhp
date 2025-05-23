{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- Commands:
--  check <file>
--  find <symbol>
--  info <file> <expr>
--  type <file> <line> <column>
--  lint [hlint options] <file>
--     the format of hlint options is [String] because they may contain
--     spaces and also <file> may contain spaces.
--  boot
--  browse [<package>:]<module>
--  quit
--
-- Session separators:
--   OK -- success
--   NG -- failure

module Main where

import Control.Concurrent (MVar, forkIO, newEmptyMVar, putMVar, readMVar)
import Control.Exception (Exception, SomeException (..))
import qualified Control.Exception as E
import Control.Monad (void, when)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Version (showVersion)
import System.Console.GetOpt
import System.Directory (setCurrentDirectory)
import System.Environment (getArgs)
import System.IO (hFlush, stdout)

import Hhp
import Hhp.Ghc

-- import Hhp.Internal
import Paths_hhp

----------------------------------------------------------------

type Logger = IO String

----------------------------------------------------------------

progVersion :: String
progVersion =
    "hhpi version "
        ++ showVersion version
        ++ " compiled by GHC "
        ++ cProjectVersion
        ++ "\n"

argspec :: [OptDescr (Options -> Options)]
argspec =
    [ Option
        "b"
        ["boundary"]
        (ReqArg (\s opts -> opts{lineSeparator = LineSeparator s}) "sep")
        "specify line separator (default is Nul string)"
    , Option
        "l"
        ["tolisp"]
        (NoArg (\opts -> opts{outputStyle = LispStyle}))
        "print as a list of Lisp"
    , Option
        "g"
        []
        (ReqArg (\s opts -> opts{ghcOpts = s : ghcOpts opts}) "flag")
        "specify a ghc flag"
    ]

usage :: String
usage =
    progVersion
        ++ "Usage:\n"
        ++ "\t hhpi [-l] [-b sep] [-g flag]\n"
        ++ "\t hhpi version\n"
        ++ "\t hhpi help\n"

parseArgs :: [OptDescr (Options -> Options)] -> [String] -> (Options, [String])
parseArgs spec argv =
    case getOpt Permute spec argv of
        (o, n, []) -> (foldr id defaultOptions o, n)
        (_, _, errs) -> E.throw (CmdArg errs)

----------------------------------------------------------------

newtype HhpiError = CmdArg [String] deriving (Show)

instance Exception HhpiError

----------------------------------------------------------------

-- Running two GHC monad threads disables the handling of
-- C-c since installSignalHandlers is called twice, sigh.

main :: IO ()
main =
    E.handle cmdHandler $
        go =<< parseArgs argspec <$> getArgs
  where
    cmdHandler (CmdArg _) = putStr $ usageInfo usage argspec
    go (_, "help" : _) = putStr $ usageInfo usage argspec
    go (_, "version" : _) = putStr progVersion
    go (opt, _) = E.handle someHandler $ do
        cradle0 <- findCradle
        let rootdir = cradleRootDir cradle0
            cradle = cradle0{cradleCurrentDir = rootdir}
        setCurrentDirectory rootdir
        mvar <- liftIO newEmptyMVar
        mlibdir <- getSystemLibDir
        void $ forkIO $ setupDB cradle mlibdir opt mvar
        run cradle mlibdir opt $ loop opt S.empty mvar
      where
        -- this is just in case.
        -- If an error is caught here, it is a bug of Hhp library.
        someHandler (SomeException e) = do
            putStrLn $ "NG " ++ replace (show e)

replace :: String -> String
replace [] = []
replace ('\n' : xs) = ';' : replace xs
replace (x : xs) = x : replace xs

----------------------------------------------------------------

run :: Cradle -> Maybe FilePath -> Options -> Ghc a -> IO a
run cradle mlibdir opt body = runGhc mlibdir $ do
    initializeFlagsWithCradle opt cradle
    body

----------------------------------------------------------------

setupDB :: Cradle -> Maybe FilePath -> Options -> MVar SymMdlDb -> IO ()
setupDB cradle mlibdir opt mvar = E.handle handler $ do
    db <- run cradle mlibdir opt getSymMdlDb
    putMVar mvar db
  where
    handler (SomeException _) = return () -- fixme: put emptyDb?

----------------------------------------------------------------

loop :: Options -> Set FilePath -> MVar SymMdlDb -> Ghc ()
loop opt set mvar = do
    cmdArg <- liftIO getLine
    let (cmd, arg') = break (== ' ') cmdArg
        arg = dropWhile (== ' ') arg'
    (ret, ok, set') <- case cmd of
        "check" -> checkStx opt set arg
        "find" -> findSym opt set arg mvar
        "lint" -> lintStx opt set arg
        "info" -> showInfo opt set arg
        "type" -> showType opt set arg
        "boot" -> bootIt opt set
        "browse" -> browseIt opt set arg
        "quit" -> return ("quit", False, set)
        "" -> return ("quit", False, set)
        _ -> return ([], True, set)
    if ok
        then do
            liftIO $ putStr ret
            liftIO $ putStrLn "OK"
        else do
            liftIO $ putStrLn $ "NG " ++ replace ret
    liftIO $ hFlush stdout
    when ok $ loop opt set' mvar

----------------------------------------------------------------

checkStx
    :: Options
    -> Set FilePath
    -> FilePath
    -> Ghc (String, Bool, Set FilePath)
checkStx opt set file = do
    set' <- newFileSet set file
    let files = S.toList set'
    eret <- check opt files
    case eret of
        Right ret -> return (ret, True, set')
        Left ret -> return (ret, True, set) -- fxime: set

newFileSet :: Set FilePath -> FilePath -> Ghc (Set FilePath)
newFileSet set file = do
    let set1
            | S.member file set = set
            | otherwise = S.insert file set
    mf <- getMainFileToBeDeleted file
    return $ case mf of
        Nothing -> set1
        Just mainfile -> S.delete mainfile set1

----------------------------------------------------------------

findSym
    :: Options
    -> Set FilePath
    -> String
    -> MVar SymMdlDb
    -> Ghc (String, Bool, Set FilePath)
findSym opt set sym mvar = do
    db <- liftIO $ readMVar mvar
    let ret = lookupSym opt sym db
    return (ret, True, set)

lintStx
    :: Options
    -> Set FilePath
    -> FilePath
    -> Ghc (String, Bool, Set FilePath)
lintStx opt set optFile = liftIO $ do
    ret <- lintSyntax opt' file
    return (ret, True, set)
  where
    (opts, file) = parseLintOptions optFile
    hopts = if opts == "" then [] else read opts
    opt' = opt{hlintOpts = hopts}

-- |
-- >>> parseLintOptions "[\"--ignore=Use camelCase\", \"--ignore=Eta reduce\"] file name"
-- (["--ignore=Use camelCase", "--ignore=Eta reduce"], "file name")
-- >>> parseLintOptions "file name"
-- ([], "file name")
parseLintOptions :: String -> (String, String)
parseLintOptions optFile = case brk (== ']') (dropWhile (/= '[') optFile) of
    ("", "") -> ([], optFile)
    (opt', file') -> (opt', dropWhile (== ' ') file')
  where
    brk _ [] = ([], [])
    brk p (x : xs')
        | p x = ([x], xs')
        | otherwise = let (ys, zs) = brk p xs' in (x : ys, zs)

----------------------------------------------------------------

showInfo
    :: Options
    -> Set FilePath
    -> FilePath
    -> Ghc (String, Bool, Set FilePath)
showInfo opt set fileArg = do
    let (file, expr) = case words fileArg of
            [file0, expr0] -> (file0, expr0)
            _ -> error "showInfo"
    set' <- newFileSet set file
    ret <- info opt file expr
    return (ret, True, set')

showType
    :: Options
    -> Set FilePath
    -> FilePath
    -> Ghc (String, Bool, Set FilePath)
showType opt set fileArg = do
    let (file, line, column) = case words fileArg of
            [file0, line0, column0] -> (file0, line0, column0)
            _ -> error "showInfo"
    set' <- newFileSet set file
    ret <- types opt file (read line) (read column)
    return (ret, True, set')

----------------------------------------------------------------

bootIt
    :: Options
    -> Set FilePath
    -> Ghc (String, Bool, Set FilePath)
bootIt opt set = do
    ret <- boot opt
    return (ret, True, set)

browseIt
    :: Options
    -> Set FilePath
    -> ModuleString
    -> Ghc (String, Bool, Set FilePath)
browseIt opt set mdl = do
    ret <- browse opt mdl
    return (ret, True, set)
