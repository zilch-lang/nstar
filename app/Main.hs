{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Main (main) where

import Language.NStar.Syntax (parseFile)
import Language.NStar.Typechecker (typecheck)
import Language.NStar.CodeGen (SupportedArch(..), compileToElf)
-- ! Experimental; remove once tested
import Data.Elf as Elf (compile, Size(..), Endianness(..), writeFile)
-- ! end
import Text.Diagnose (printDiagnostic, (<~<), prettyText)
import System.IO (stderr, stdout)
import Data.Text (Text)
import qualified Data.Text as Text (unpack)
import Data.Text.Encoding (decodeUtf8)
import System.IO (utf8, hSetEncoding, hGetContents)
import Console.NStar.Flags
import Control.Monad (forM_, when)
import System.Exit (exitFailure, exitSuccess)
import Data.ByteString (readFile, ByteString)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Except (runExceptT, liftEither)
import System.Directory (createDirectoryIfMissing, getCurrentDirectory)
import System.FilePath.Posix (joinPath)
import Data.IORef

main :: IO ()
main = do
  flags <- extractFlags
  -- print flags

  forM_ (files flags) (tryCompile flags)

------------------------------------------------------------------------------------------------

tryCompile :: Flags -> FilePath -> IO ()
tryCompile flags file = do
  let withColor = diagnostic_color (configuration flags)
      dumpAST = dump_ast (debugging flags)
      dumpTypedAST = dump_tast (debugging flags)

  let ?lexerFlags  = LexerFlags {}
  let ?parserFlags = ParserFlags {}
  let ?tcFlags     = TypecheckerFlags {}

  let ?includePath = includePath flags

  allFiles <- newIORef []

  result <- runExceptT do
        (files, res) <- liftIO (parseFile file)
        liftIO $ writeIORef allFiles files
        ast <- liftEither res

        when dumpAST do
          liftIO $ createDirectoryIfMissing True (joinPath [".nsc", "dump"])
          liftIO $ Prelude.writeFile (joinPath [".nsc", "dump", "ast.debug"]) (show $ prettyText ast)

        (tast, tcWarnings)    <- liftEither $ typecheck ast
        liftIO (printDiagnostic withColor stderr (foldl (<~<) tcWarnings files))

        when dumpTypedAST do
          liftIO $ createDirectoryIfMissing True (joinPath [".nsc", "dump"])
          liftIO $ Prelude.writeFile (joinPath [".nsc", "dump", "typed-ast.debug"]) (show $ prettyText tast)

        pure tast
  case result of
    Left diag    -> do
      files <- readIORef allFiles

      printDiagnostic withColor stderr (foldl (<~<) diag files)
      exitFailure
    Right p     -> do
      -- ! Experimental codegen
      --   For now, only write ELF output in a file named "test.o".

      let elfObject = compileToElf X64 p
      bytes <- compile @S64 LE elfObject   -- we want little endian as a test
      Elf.writeFile (output flags) bytes
      exitSuccess
