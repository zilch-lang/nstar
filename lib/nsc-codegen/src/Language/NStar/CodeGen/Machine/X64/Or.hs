module Language.NStar.CodeGen.Machine.X64.Or
  ( -- * Instruction encoding
    -- $encoding

    -- * Compiling
    compileOr,
  )
where

import Data.Functor ((<&>))
import Data.Located (unLoc)
import Language.NStar.CodeGen.Compiler (Compiler)
import Language.NStar.CodeGen.Machine.Internal.Intermediate (InterOpcode (Byte))
import Language.NStar.CodeGen.Machine.Internal.X64.ModRM (modRM)
import Language.NStar.CodeGen.Machine.Internal.X64.REX (rexW)
import Language.NStar.CodeGen.Machine.Internal.X64.RegisterEncoding (registerNumber)
import Language.NStar.CodeGen.Machine.X64.Expression (compileExprX64)
import Language.NStar.CodeGen.Machine.X64.Mv (compileMv)
import Language.NStar.Typechecker.Core (Expr (..), Register)

-- $encoding
--
--
-- - * In 64-bit mode, r/m8 can not be encoded to access the following byte registers if a REX prefix is used: AH, BH, CH, DH.
--
-- < >

compileOr :: Expr -> Expr -> Register -> Compiler [InterOpcode]
compileOr x@(RegE r1) (RegE s) r2
  | unLoc r1 == r2 = pure bytes
  -- when the destination and the source are the same register,
  -- we actually don't need to move anything
  | otherwise = compileMv x r2 <&> (<> bytes)
  where
    bytes = [rexW, Byte 0x09, modRM 0b11 (registerNumber r2) (registerNumber $ unLoc s)]
compileOr x (RegE s) r = compileMv x r <&> (<> [rexW, Byte 0x09, modRM 0b11 (registerNumber r) (registerNumber $ unLoc s)])
compileOr x@(RegE r1) y@(ImmE _) r2
  | unLoc r1 == r2 = bytes
  -- when the destination and the source are the same register,
  -- we actually don't need to move anything
  | otherwise = compileMv x r2 >>= \mv -> (mv <>) <$> bytes
  where
    bytes = ([Byte 0x81, modRM 0b11 0x1 (registerNumber r2)] <>) <$> compileExprX64 32 y
-- we only allow 32-bits immediate values on x64
-- if you want 64-bits immediate values, you'll need to move them inside registers
-- TODO: perhaps throw a warning or error for this case
compileOr _ _ _ = undefined
