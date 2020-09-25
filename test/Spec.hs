module Main (main) where

import Test.Hspec
import System.FilePath.Glob (glob)

main :: IO ()
main = glob "./**/*.nst" >>= hspec . tests

tests :: [FilePath] -> Spec
tests = parallel . foldl (*>) (pure ()) . fmap check

check :: FilePath -> Spec
check file = pure ()
