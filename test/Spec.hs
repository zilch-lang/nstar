{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}

module Main (main) where

import Test.Hspec
import System.FilePath.Glob (glob)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Language.NStar.Syntax (lexFile, parseFile)
import Language.NStar.Typechecker (typecheck)
import Data.List (isInfixOf)
import Text.Diagnose ((<~<), prettyText)
import Data.Bifunctor (first)

data Error
  = Lx
  | Ps
  | Tc
  | Any
  | No
 deriving Eq

instance Show Error where
  show Lx = "lexing"
  show Ps = "parsing"
  show Tc = "type-checking"
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
        tokens <- first (, Lx) $ lexFile file content
        ast <- first (, Ps) $ parseFile file tokens
        ast <- first (, Tc) $ typecheck ast
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
