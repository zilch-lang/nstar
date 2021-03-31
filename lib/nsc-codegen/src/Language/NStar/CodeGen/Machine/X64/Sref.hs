module Language.NStar.CodeGen.Machine.X64.Sref where

import Language.NStar.Typechecker.Core (Register)
import Language.NStar.CodeGen.Compiler (Compiler)
import Language.NStar.CodeGen.Machine.Internal.Intermediate (InterOpcode(Byte), InterOpcode)
import Language.NStar.CodeGen.Machine.Internal.X64.REX (rexW)
import Language.NStar.CodeGen.Machine.Internal.X64.ModRM (modRM)
import Language.NStar.CodeGen.Machine.Internal.X64.RegisterEncoding (registerNumber)
import Language.NStar.CodeGen.Machine.Internal.X64.SIB (sib)
import Language.NStar.CodeGen.Machine.X64.Expression (int8)

compileSref :: Integer -> Integer -> Register -> Compiler [InterOpcode]
compileSref n _ r = pure $ [rexW, Byte 0x8B, modRM 0b01 (registerNumber r) 0x4, sib 0x0 0x4 0x4] <> (Byte <$> int8 n)
