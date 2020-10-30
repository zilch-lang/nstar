module Language.NStar.CodeGen.Machine.X64 (compileX64) where

import Language.NStar.CodeGen.Compiler
import Language.NStar.Typechecker.Core
import Language.NStar.CodeGen.Machine.Internal.Intermediate (InterOpcodes(..))

compileX64 :: TypedProgram -> Compiler ()
compileX64 p = compileInterX64 p >>= fixupAddressesX64

compileInterX64 :: TypedProgram -> Compiler [InterOpcodes]
compileInterX64 _ = pure []

fixupAddressesX64 :: [InterOpcodes] -> Compile ()
fixupAddressesX64 _ = pure ()
