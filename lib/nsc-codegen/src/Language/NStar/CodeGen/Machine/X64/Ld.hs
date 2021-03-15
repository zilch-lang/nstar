module Language.NStar.CodeGen.Machine.X64.Ld where

import Language.NStar.Typechecker.Core (Expr(RegE), Register, Expr)
import Language.NStar.CodeGen.Compiler (Compiler)
import Language.NStar.CodeGen.Machine.Internal.Intermediate (InterOpcode(..))
import Internal.Error (internalError)
import Language.NStar.Typechecker.Core (Expr(ImmE))
import Data.Located (unLoc, Located((:@)))
import Language.NStar.CodeGen.Machine.Internal.X64.REX (rexW)
import Language.NStar.CodeGen.Machine.Internal.X64.ModRM (modRM)
import Language.NStar.CodeGen.Machine.Internal.X64.RegisterEncoding (registerNumber)
import Language.NStar.CodeGen.Machine.X64.Expression (int8)
import Language.NStar.Typechecker.Core (Expr(NameE))
import Language.NStar.CodeGen.Machine.Internal.X64.SIB (sib)
import Language.NStar.Syntax.Core (Immediate(I))

compileLd :: Expr -> Expr -> Register -> Compiler [InterOpcode]
compileLd (ImmE (I disp :@ _)) (RegE p) r = pure $ [rexW, Byte 0x8B, modRM 0b1 (registerNumber r) (registerNumber (unLoc p))] <> (Byte <$> int8 disp)
compileLd (ImmE (I disp :@ _)) (NameE l _) r = pure [rexW, Byte 0x8B, modRM 0b0 (registerNumber r) 0x4, sib 0x0 0x4 0x5, Symbol32 (unLoc l) disp]
compileLd (RegE o) (NameE l _) r = pure [rexW, Byte 0x8B, modRM 0x2 (registerNumber r) (registerNumber (unLoc o)), Symbol32 (unLoc l) 0]
compileLd (RegE o) (RegE p) r = pure [rexW, Byte 0x8B, modRM 0x0 (registerNumber r) 0x4, sib 0x0 (registerNumber (unLoc o)) (registerNumber (unLoc p))]
compileLd o p r = internalError $ "unsupported instruction 'ld " <> show o <> ", " <> show p <> ", " <> show r
