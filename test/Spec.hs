module Main (main) where

import Test.Hspec
import System.FilePath.Glob (glob)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Language.NStar.Syntax (lexFile, parseFile)
import Language.NStar.Typechecker (typecheck)
import Data.List (isInfixOf)
import Text.Diagnose ((<~<), prettyText)

-----------------------

main :: IO ()
main = glob "./test/**/*.nst" >>= hspec . tests

-----------------------

tests :: [FilePath] -> Spec
tests = parallel . foldl (*>) (pure ()) . fmap check

check :: FilePath -> Spec
check file = do
  content <- runIO $ Text.readFile file

  let result = do
        tokens <- lexFile file content
        ast <- parseFile file tokens
        ast <- typecheck ast
        pure ast

  specify file $
    case result of
      Left _ | "error_" `isInfixOf` file -> pure () -- test fails as expected
      Right _ | "error_" `isInfixOf` file -> expectationFailure ("Test on file '" <> file <> "' should have failed, but passed!")
      Left diag -> expectationFailure ("Test on file '" <> file <> "' failed with error:\n" <> show (prettyText (diag <~< (file, lines $ Text.unpack content))))
      Right _ -> pure () -- test passes as expected
