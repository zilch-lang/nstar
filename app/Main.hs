{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main where

import Language.NStar.Syntax (lexFile, parseFile)
import Language.NStar.Typechecker (typecheck)
import Text.Diagnose (printDiagnostic, (<~<))
import System.IO (stderr, stdout, hPrint, hPutStr)
import GHC.ResponseFile (getArgsWithResponseFiles)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

main :: IO ()
main = do
  -- TODO: proper error handling
  -- TODO: handle command-line arguments correctly (flags, options, etc)
 
  (file : _) <- getArgsWithResponseFiles
  content <- Text.readFile file

  let withColor = True

  tokens <- case lexFile file content of
    Left diag -> do
      printDiagnostic withColor stderr (diag <~< (file, lines $ Text.unpack content))
      error "Lexer failed with exit code -1"
    Right res -> pure res

  ast <- case parseFile file tokens of
    Left diag -> do
      printDiagnostic withColor stderr (diag <~< (file, lines $ Text.unpack content))
      error "Parser failed with exit code -1"
    Right res -> pure res

  (tast, warnings) <- case typecheck ast of
    Left diag -> do
      printDiagnostic withColor stderr (diag <~< (file, lines $ Text.unpack content))
      error "Typechecker failed with exit code -1"
    Right res -> pure res
  printDiagnostic withColor stderr (warnings <~< (file, lines $ Text.unpack content))

  pure ()
