{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Main (main) where

import Language.NStar.Syntax (Section(IncludeS), Program(Program), parseAST)
import Language.NStar.Typechecker (typecheck)
import Language.NStar.CodeGen (SupportedArch(..), compileToElf)
-- ! Experimental; remove once tested
import Data.Elf as Elf (compile, Size(..), Endianness(..), writeFile)
-- ! end
import Error.Diagnose (printDiagnostic, addFile)
import System.IO (stderr)
import Console.NStar.Flags
import Control.Monad (when)
import System.Exit (exitFailure, exitSuccess)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Except (runExceptT, liftEither)
import System.Directory (createDirectoryIfMissing)
import System.FilePath.Posix (joinPath)
import Data.IORef
import Data.Located
import qualified Data.Text as Text
import Prettyprinter (pretty)

main :: IO ()
main = do
  flags <- extractFlags
  -- print flags

  tryCompile flags (files flags)

------------------------------------------------------------------------------------------------

tryCompile :: Flags -> [FilePath] -> IO ()
tryCompile flags files = do
  let withColor = diagnostic_color (configuration flags)
      dumpAST = dump_ast (debugging flags)
      dumpTypedAST = dump_tast (debugging flags)

  let ?lexerFlags  = LexerFlags {}
  let ?parserFlags = ParserFlags {}
  let ?tcFlags     = TypecheckerFlags {}

  let ?includePath = includePath flags

  allFiles <- newIORef []

  let dummyPos = Position (0, 0) (0, 0) "<command-line>"
      file = "command-line" :@ dummyPos
      program = Program [IncludeS ((:@ dummyPos) . Text.pack <$> files) :@ dummyPos]

  result <- runExceptT do
        (files, res) <- liftIO (parseAST file program)
        liftIO $ writeIORef allFiles files
        ast <- liftEither res

        when dumpAST do
          liftIO $ createDirectoryIfMissing True (joinPath [".nsc", "dump"])
          liftIO $ Prelude.writeFile (joinPath [".nsc", "dump", "ast.debug"]) (show $ pretty ast)

        (tast, tcWarnings)    <- liftEither $ typecheck ast
        liftIO (printDiagnostic stderr True withColor (foldl (uncurry . addFile) tcWarnings files))

        when dumpTypedAST do
          liftIO $ createDirectoryIfMissing True (joinPath [".nsc", "dump"])
          liftIO $ Prelude.writeFile (joinPath [".nsc", "dump", "typed-ast.debug"]) (show $ pretty tast)

        pure tast
  case result of
    Left diag    -> do
      files <- readIORef allFiles

      printDiagnostic stderr True withColor (foldl (uncurry . addFile) diag files)
      exitFailure
    Right p     -> do
      -- ! Experimental codegen
      --   For now, only write ELF output in a file named "test.o".

      let elfObject = compileToElf X64 p
      bytes <- compile @'S64 LE elfObject   -- we want little endian as a test
      Elf.writeFile (output flags) bytes
      exitSuccess
