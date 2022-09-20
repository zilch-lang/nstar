module Language.NStar.CodeGen.Machine.X64.And where

import Data.Functor ((<&>))
import Data.Located (unLoc)
import Language.NStar.CodeGen.Compiler (Compiler)
import Language.NStar.CodeGen.Machine.Internal.Intermediate (InterOpcode (Byte))
import Language.NStar.CodeGen.Machine.Internal.X64.ModRM (modRM)
import Language.NStar.CodeGen.Machine.Internal.X64.REX (rexW)
import Language.NStar.CodeGen.Machine.Internal.X64.RegisterEncoding (registerNumber)
import Language.NStar.CodeGen.Machine.Internal.X64.SIB (sib)
import Language.NStar.CodeGen.Machine.X64.Expression (compileExprX64)
import Language.NStar.CodeGen.Machine.X64.Mv (compileMv)
import Language.NStar.Typechecker.Core (Expr (..), Register)

compileAnd :: Expr -> Expr -> Register -> Compiler [InterOpcode]
compileAnd x@(RegE r1) (RegE s) r2
  | unLoc r1 == r2 = pure bytes
  -- when the destination and the source are the same register,
  -- we actually don't need to move anything
  | otherwise = compileMv x r2 <&> (<> bytes)
  where
    bytes = [rexW, Byte 0x21, modRM 0b11 (registerNumber r2) (registerNumber $ unLoc s)]
compileAnd x (RegE s) r = compileMv x r <&> (<> [rexW, Byte 0x21, modRM 0b11 (registerNumber r) (registerNumber $ unLoc s)])
compileAnd x y@(ImmE _) r = compileMv x r >>= \mv -> (mv <>) . ([rexW, Byte 0x21, modRM 0b00 (registerNumber r) 0x4, sib 0b00 0x4 0x5] <>) <$> compileExprX64 32 y
-- we only allow 32-bits immediate values on x64
-- if you want 64-bits immediate values, you'll need to move them inside registers
compileAnd _ _ _ = undefined
