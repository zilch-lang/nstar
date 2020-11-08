{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Main (main) where

import Language.NStar.Syntax (lexFile, parseFile)
import Language.NStar.Typechecker (typecheck)
import Language.NStar.CodeGen (SupportedArch(..), compileToElf)
-- ! Experimental; remove once tested
import Data.Elf as Elf (compile, Size(..), Endianness(..), writeFile)
-- ! end
import Text.Diagnose (printDiagnostic, (<~<))
import System.IO (stderr, stdout)
import Data.Text (Text)
import qualified Data.Text as Text (unpack)
import Data.Text.Encoding (decodeUtf8)
import System.IO (utf8, hSetEncoding, hGetContents)
import Console.NStar.Flags
import Control.Monad (forM_)
import System.Exit (exitFailure, exitSuccess)
import Data.ByteString (readFile, ByteString)

main :: IO ()
main = do
  flags <- extractFlags
  -- print flags

  forM_ (files flags) (tryCompile flags)

------------------------------------------------------------------------------------------------

tryCompile :: Flags -> FilePath -> IO ()
tryCompile flags file = do
  content <- readFileUtf8 file

  let withColor = diagnostic_color (configuration flags)

  let ?lexerFlags  = LexerFlags {}
  let ?parserFlags = ParserFlags {}
  let ?tcFlags     = TypecheckerFlags {}

  let result = lexFile file content >>= parseFile file >>= typecheck
  case result of
    Left diag    -> do
      printDiagnostic withColor stderr (diag <~< (file, lines $ Text.unpack content))
      exitFailure
    Right (p, _) -> do
      -- ! Experimental codegen
      --   For now, only write ELF output in a file named "test.o".

      let elfObject = compileToElf X64 p
      let bytes = compile @S64 LE elfObject   -- we want little endian as a test
      Elf.writeFile "./test.o" bytes
      exitSuccess

-- | Strictly read a file into a 'ByteString'.
readFile :: FilePath -> IO ByteString
readFile = Data.ByteString.readFile

-- | Strictly read a file into a 'Text' using a UTF-8 character
-- encoding. In the event of a character encoding error, a Unicode
-- replacement character will be used (a.k.a., @lenientDecode@).
readFileUtf8 :: FilePath -> IO Text
readFileUtf8 = fmap decodeUtf8 . Main.readFile
