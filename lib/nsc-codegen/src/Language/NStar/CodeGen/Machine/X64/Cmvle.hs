module Language.NStar.CodeGen.Machine.X64.Cmvle
  ( -- * Instruction encoding
    -- $encoding

    -- * Compiling
    compileCmvle,
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
import Language.NStar.Syntax.Core (Immediate (I))
import Language.NStar.Typechecker.Core (Expr (..), Register)

-- $encoding

compileCmvle :: Expr -> Expr -> Expr -> Expr -> Register -> Compiler [InterOpcode]
compileCmvle (RegE a) (RegE b) (RegE c) (RegE d) r =
  pure $
    -- cmp a b
    [rexW, Byte 0x39, modRM 0b11 (registerNumber $ unLoc b) (registerNumber $ unLoc a)]
      <>
      -- cmovle c r
      [rexW, Byte 0x0F, Byte 0x4E, modRM 0b11 (registerNumber $ unLoc c) (registerNumber r)]
      <>
      -- cmovg d r
      [rexW, Byte 0x0F, Byte 0x4F, modRM 0b11 (registerNumber $ unLoc d) (registerNumber r)]
compileCmvle (RegE a) i@(ImmE _) (RegE c) (RegE d) r =
  -- cmp a i
  (([rexW, Byte 0x81, modRM 0b11 0x7 (registerNumber $ unLoc a)] <>) <$> compileExprX64 32 i)
    <&> ( <>
            -- cmovle c r
            [rexW, Byte 0x0F, Byte 0x4E, modRM 0b11 (registerNumber $ unLoc c) (registerNumber r)]
              <>
              -- cmovg d r
              [rexW, Byte 0x0F, Byte 0x4F, modRM 0b11 (registerNumber $ unLoc d) (registerNumber r)]
        )
compileCmvle i@(ImmE _) (RegE b) (RegE c) (RegE d) r =
  -- cmp b i
  (([rexW, Byte 0x81, modRM 0b11 0x7 (registerNumber $ unLoc b)] <>) <$> compileExprX64 32 i)
    <&> ( <>
            -- cmovle d r
            [rexW, Byte 0x0F, Byte 0x4E, modRM 0b11 (registerNumber $ unLoc d) (registerNumber r)]
              <>
              -- cmovg c r
              [rexW, Byte 0x0F, Byte 0x4F, modRM 0b11 (registerNumber $ unLoc c) (registerNumber r)]
        )
compileCmvle (ImmE m) (ImmE n) i@(RegE c) j@(RegE d) r = case (unLoc m, unLoc n) of
  (I m, I n) | m <= n -> compileMv i r
  _ -> compileMv j r
compileCmvle _ _ _ _ _ = undefined
