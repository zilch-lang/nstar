module Language.NStar.CodeGen.Machine.X64.Sst where

import Language.NStar.Typechecker.Core (Expr(ImmE, NameE, RegE))
import Language.NStar.CodeGen.Compiler (Compiler)
import Language.NStar.CodeGen.Machine.Internal.Intermediate (InterOpcode(Byte, Symbol32))
import Language.NStar.CodeGen.Machine.Internal.X64.REX (rexW)
import Language.NStar.CodeGen.Machine.Internal.X64.ModRM (modRM)
import Language.NStar.CodeGen.Machine.Internal.X64.SIB (sib)
import Language.NStar.CodeGen.Machine.X64.Expression (compileExprX64, int8)
import Internal.Error (internalError)
import Language.NStar.CodeGen.Machine.Internal.X64.RegisterEncoding (registerNumber)
import Data.Located (unLoc)

compileSst :: Expr -> Integer -> Compiler [InterOpcode]
compileSst src@(ImmE _) n = mappend ([rexW, Byte 0xC7, modRM 0b1 0x0 0x4 {- RSP -}, sib 0x0 0x4 0x4] <> (Byte <$> int8 (-n))) <$> compileExprX64 32 src
compileSst (RegE r) n = pure $ [rexW, Byte 0x89, modRM 0b1 0x4 (registerNumber (unLoc r)), sib 0x0 0x4 0x4] <> (Byte <$> int8 (-n))
compileSst (NameE l _) n = pure $ [rexW, Byte 0xC7, modRM 0b1 0x0 0x4 {- RSP -}, sib 0x0 0x4 0x4] <> (Byte <$> int8 (-n)) <> [Symbol32 (unLoc l) 0]
compileSst s n = internalError $ "Unsupported instruction 'sst " <> show s <> ", " <> show n <> "'."
