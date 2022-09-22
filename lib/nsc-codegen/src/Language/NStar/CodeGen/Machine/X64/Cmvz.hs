module Language.NStar.CodeGen.Machine.X64.Cmvz
  ( -- * Instruction encoding
    -- $encoding

    -- * Compiling
    compileCmvz,
  )
where

import Data.Located (unLoc)
import Language.NStar.CodeGen.Compiler (Compiler)
import Language.NStar.CodeGen.Machine.Internal.Intermediate (InterOpcode (Byte))
import Language.NStar.CodeGen.Machine.Internal.X64.ModRM (modRM)
import Language.NStar.CodeGen.Machine.Internal.X64.REX (rexW)
import Language.NStar.CodeGen.Machine.Internal.X64.RegisterEncoding (registerNumber)
import Language.NStar.CodeGen.Machine.X64.Expression (int8)
import Language.NStar.Typechecker.Core (Expr (..), Register)

-- $encoding

compileCmvz :: Expr -> Expr -> Expr -> Register -> Compiler [InterOpcode]
compileCmvz (RegE a) (RegE b) (RegE c) r =
  pure $
    -- cmp a 0
    ([rexW, Byte 0x83, modRM 0b11 0x7 (registerNumber $ unLoc a)] <> (Byte <$> int8 0))
      <>
      -- cmovz b r
      [rexW, Byte 0x0F, Byte 0x44, modRM 0b11 (registerNumber $ unLoc b) (registerNumber r)]
      <>
      -- cmovnz c r
      [rexW, Byte 0x0F, Byte 0x45, modRM 0b11 (registerNumber $ unLoc c) (registerNumber r)]
compileCmvz _ _ _ _ = undefined

-- X64 does not seem to allow CMP with two immediates, nor CMOVcc with an immediate as first operand...
