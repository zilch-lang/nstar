{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}

module Main (main) where

import Test.Hspec
import System.FilePath.Glob (glob)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Language.NStar.Syntax (lexFile, parseFile, postProcessAST)
import Language.NStar.Typechecker (typecheck)
import Language.NStar.CodeGen
import Data.List (isInfixOf)
import Text.Diagnose ((<~<), prettyText, Diagnostic, reportError, Marker(..), diagnostic, (<++>))
import Data.Bifunctor (first)
import Console.NStar.Flags (LexerFlags(..), ParserFlags(..), TypecheckerFlags(..))
import Control.DeepSeq (deepseq, NFData)
import Control.Spoon (teaspoon)

data Error
  = Lx
  | Ps
  | Tc
  | Cg
  | Any
  | No
 deriving Eq

instance Show Error where
  show Lx = "lexing"
  show Ps = "parsing"
  show Tc = "type-checking"
  show Cg = "codegen"
  show Any = "any"
  show No = "no"

-----------------------

main :: IO ()
main = glob "./test/**/*.nst" >>= hspec . tests
  -- Assuming that we run `stack test` in the root of the project.

-----------------------

tests :: [FilePath] -> Spec
tests = parallel . foldl (*>) (pure ()) . fmap check

check :: FilePath -> Spec
check file = do
  content <- runIO $ Text.readFile file

  -- We have six cases at the moment:
  -- - a lexing error (error_lx_*)
  -- - a parsing error (error_ps_*)
  -- - a typechecking error (error_tc_*)
  -- - any error (error_*)
  -- - no error (*)
  -- - pending test (pending_*)
  --
  -- We have to handle those

  let expectedError =
        if | "error_lx_" `isInfixOf` file -> Lx
           | "error_ps_" `isInfixOf` file -> Ps
           | "error_tc_" `isInfixOf` file -> Tc
           | "error_" `isInfixOf` file    -> Any
           | otherwise                    -> No

  let result = do
        let ?lexerFlags  = LexerFlags {}
        let ?parserFlags = ParserFlags {}
        let ?tcFlags     = TypecheckerFlags {}

        (tokens, _) <- first (, Lx) $! lexFile file content
        (ast, _) <- first (, Ps) $! parseFile file tokens
        ast <- pure $! postProcessAST ast
        (ast, _) <- first (, Tc) $! typecheck ast
        _ <- maybeToEither errorCallCodeGen $ teaspoon (compileToElf @S64 X64 ast `deepseq` ())
        pure ast

  specify file $
    if "pending_" `isInfixOf` file
    then pendingWith "Failing test is being fixed."
    else case result of
      Left (_, Any)                    -> pure () -- test fails as expected
      Left (d, e) | expectedError == e -> pure ()
                  | otherwise          ->
                      expectationFailure ("Expected to fail in " <> show expectedError <>
                                               " phase but failed in " <> show e <> " phase with error:\n" <>
                                               show (prettyText (d <~< (file, lines $ Text.unpack content))))
      Right _ | expectedError /= No    -> expectationFailure ("Test should have failed in " <> show expectedError <> " phase, but passed all phases!")
              | otherwise              -> pure () -- test passes as expected

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e = maybe (Left e) Right

errorCallCodeGen :: (Diagnostic [] String Char, Error)
errorCallCodeGen = (diagnostic <++> reportError "Error call during code generation" [] [], Cg)

-- Orphan instances for 'deepseq'

instance NFData (ElfObject n)

instance NFData (ElfHeader n)
instance NFData (EFlags n)
instance NFData (Class)
instance NFData (Encoding)
instance NFData (OSABI)
instance NFData (ObjFileType)
instance NFData (Arch)
instance NFData (Version)

instance NFData (ProgramHeader n)
instance NFData (PFlags n)

instance NFData (SectionHeader n)
instance NFData (SFlags n)

instance NFData (RelocationSymbol n)
instance NFData (RelocationOrigin)
instance NFData (RelocationType)

instance NFData (ElfSymbol n)
instance NFData (SymbolType)
instance NFData (SymbolBinding)
instance NFData (SymbolVisibility)
