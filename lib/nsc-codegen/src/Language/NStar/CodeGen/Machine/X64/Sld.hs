module Language.NStar.CodeGen.Machine.X64.Sld where

import Language.NStar.Typechecker.Core (Register)
import Language.NStar.CodeGen.Compiler (Compiler)
import Language.NStar.CodeGen.Machine.Internal.Intermediate (InterOpcode(Byte))
import Language.NStar.CodeGen.Machine.Internal.X64.REX (rexW)
import Language.NStar.CodeGen.Machine.Internal.X64.ModRM (modRM)
import Language.NStar.CodeGen.Machine.Internal.X64.RegisterEncoding (registerNumber)
import Language.NStar.CodeGen.Machine.X64.Expression (int8)
import Language.NStar.CodeGen.Machine.Internal.X64.SIB (sib)

compileSld :: Integer -> Register -> Compiler [InterOpcode]
compileSld n r = pure $ [rexW, Byte 0x8B, modRM 0x1 (registerNumber r) 0x4 {- RSP -}, sib 0x0 0x4 0x4] <> (Byte <$> int8 n)
