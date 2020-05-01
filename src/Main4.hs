module Main where

import Parser
import Syntax
import Emit

import Control.Monad.Trans

import System.IO
import System.Environment
import System.Console.Haskeline

import qualified LLVM.AST as AST

-- initModule :: AST.Module
-- initModule = emptyModule "my cool jit"

processFile :: String -> IO [Expr]
processFile fname = readFile fname >>= process

process :: String -> IO [Expr]
process source = do
  let res = parseToplevel source
  case res of
    Left err -> return []
    Right ex -> return ex

repl :: IO ()
repl = runInputT defaultSettings loop
  where
  loop = do
    minput <- getInputLine "ready> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just input -> do
        outputStrLn input
        exprs <- liftIO $ process input
        outputStrLn $ show exprs
        loop

main :: IO ()
main = do
  args <- getArgs
  case args of
    []      -> repl
    [fname] -> do
      ast <- processFile fname
      print ast