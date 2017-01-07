module Main where

import System.Environment (getArgs)
import System.FilePath.Posix (replaceExtension)
import System.Cmd (system)
import System.IO
import System.Exit

import ParLatte
import AbsLatte
import Optimizer (optimizeLatteProgram)
import SemanticAnalyzer (analyzeLatteProgram)
import LlvmGenerator (generateLlvmProgram)
import ErrM

compilationError :: String -> IO ()
compilationError e = 
    hPutStrLn stderr ("ERROR\n" ++ e) >> exitWith (ExitFailure 17)

generate :: Program -> FilePath -> IO ()
generate program path = do
    optimizedProgram <- optimizeLatteProgram program
    (analyzeResult, symTable) <- analyzeLatteProgram optimizedProgram
    case analyzeResult of
        (Right ()) -> do
            code <- generateLlvmProgram optimizedProgram symTable
            fileHandle <- openFile llFilePath WriteMode
            mapM_ (hPutStrLn fileHandle) code
            hClose fileHandle
            system $ unwords ["llvm-as", llFilePath]
            system $ unwords ["llvm-link -o", bcFilePath, bcFilePath, "lib/runtime.bc"]
            hPutStrLn stderr "OK"
        (Left e) -> compilationError e
    where
        bcFilePath = replaceExtension path ".bc"
        llFilePath = replaceExtension path ".ll"

main :: IO ()
main = do
    args <- getArgs
    if length args < 1 then
        error "Missing file argument"
    else do
        s <- readFile $ args !! 0
        case pProgram (myLexer s) of
            Ok p -> generate p $ args !! 0
            Bad e -> compilationError e
