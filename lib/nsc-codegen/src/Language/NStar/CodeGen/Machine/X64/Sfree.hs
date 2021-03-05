module Language.NStar.CodeGen.Machine.X64.Sfree where

import Language.NStar.CodeGen.Compiler (Compiler)
import Language.NStar.CodeGen.Machine.Internal.Intermediate (InterOpcode(Byte))
import Language.NStar.CodeGen.Machine.Internal.X64.ModRM (modRM)
import Language.NStar.CodeGen.Machine.X64.Expression (int8)
import Language.NStar.CodeGen.Machine.Internal.X64.REX (rexW)

compileSfree :: Integer -> Compiler [InterOpcode]
compileSfree n = pure $ [rexW, Byte 0x83, modRM 0b11 0x0 0x4 {- RSP -}] <> (Byte <$> int8 n)
