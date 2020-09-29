{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImplicitParams #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Main where

import Language.NStar.Syntax (lexFile, parseFile)
import Language.NStar.Typechecker (typecheck)
import Text.Diagnose (printDiagnostic, (<~<))
import System.IO (stderr, stdout)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Console.NStar.Flags
import Control.Monad (forM_, guard)
import Control.Applicative ((<|>))
import System.Exit (exitFailure)
import Data.Char (toLower)
import qualified Data.Map as Map

main :: IO ()
main = do
  flags <- extractFlags
  print flags

  checkSanity flags

  forM_ (files flags) (tryCompile flags)

------------------------------------------------------------------------------------------------

checkSanity :: CompilerFlags -> IO ()
checkSanity CFlags{..} = do
  flip Map.traverseWithKey flags \ n@(fmap toLower -> name) v@(fmap toLower -> val) -> do
    guard (name `elem` knownConfigFlags)
      <|> (putStrLn ("Unrecognized configuration flag '" <> n <> "'.") *> exitFailure)

    case name of
      "color-diagnostics" ->
        guard (val == "yes" || val == "no")
          <|> (putStrLn ("Expected either 'yes' or 'no' for configuration flag '" <> n <> "', but got '" <> v <> "'.") *> exitFailure)
      _ -> error "configuration flag name already filtered out."

  pure ()

knownConfigFlags :: [String]
knownConfigFlags = [ "color-diagnostics" ]


tryCompile :: CompilerFlags -> String -> IO ()
tryCompile flags file = do
  content <- Text.readFile file

  let withColor = maybe "yes" id (lookupFlag "color-diagnostics" flags) == "yes"

  -- TODO: construct implicit structures for each compiler step

  -- TODO: proper error handling

  let ?lexerFlags  = LexerFlags {}
  let ?parserFlags = ParserFlags {}
  let ?tcFlags     = TypecheckerFlags {}

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
