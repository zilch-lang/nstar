module Language.NStar.CodeGen.Machine.X64.Cmvl
  ( -- * Instruction encoding
    -- $encoding

    -- * Compiling
    compileCmvl,
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

compileCmvl :: Expr -> Expr -> Expr -> Expr -> Register -> Compiler [InterOpcode]
compileCmvl (RegE a) (RegE b) (RegE c) (RegE d) r =
  pure $
    -- cmp a b
    [rexW, Byte 0x39, modRM 0b11 (registerNumber $ unLoc b) (registerNumber $ unLoc a)]
      <>
      -- cmovl c r
      [rexW, Byte 0x0F, Byte 0x4C, modRM 0b11 (registerNumber $ unLoc c) (registerNumber r)]
      <>
      -- cmovge d r
      [rexW, Byte 0x0F, Byte 0x4D, modRM 0b11 (registerNumber $ unLoc d) (registerNumber r)]
compileCmvl (RegE a) i@(ImmE _) (RegE c) (RegE d) r =
  -- cmp a i
  (([rexW, Byte 0x81, modRM 0b11 0x7 (registerNumber $ unLoc a)] <>) <$> compileExprX64 32 i)
    <&> ( <>
            -- cmovl c r
            [rexW, Byte 0x0F, Byte 0x4C, modRM 0b11 (registerNumber $ unLoc c) (registerNumber r)]
              <>
              -- cmovge d r
              [rexW, Byte 0x0F, Byte 0x4D, modRM 0b11 (registerNumber $ unLoc d) (registerNumber r)]
        )
compileCmvl i@(ImmE _) (RegE b) (RegE c) (RegE d) r =
  -- cmp b i
  (([rexW, Byte 0x81, modRM 0b11 0x7 (registerNumber $ unLoc b)] <>) <$> compileExprX64 32 i)
    <&> ( <>
            -- cmovl d r
            [rexW, Byte 0x0F, Byte 0x4C, modRM 0b11 (registerNumber $ unLoc d) (registerNumber r)]
              <>
              -- cmovge c r
              [rexW, Byte 0x0F, Byte 0x4D, modRM 0b11 (registerNumber $ unLoc c) (registerNumber r)]
        )
compileCmvl (ImmE m) (ImmE n) i@(RegE c) j@(RegE d) r = case (unLoc m, unLoc n) of
  (I m, I n) | m < n -> compileMv i r
  _ -> compileMv j r
compileCmvl _ _ _ _ _ = undefined
