module Language.NStar.CodeGen.Machine.X64.St where

import Language.NStar.Typechecker.Core (Expr(RegE, NameE, ImmE))
import Language.NStar.CodeGen.Compiler (Compiler)
import Language.NStar.CodeGen.Machine.Internal.Intermediate (InterOpcode(Symbol32, Byte))
import Internal.Error (internalError)
import Language.NStar.CodeGen.Machine.Internal.X64.REX (rexW)
import Language.NStar.CodeGen.Machine.Internal.X64.ModRM (modRM)
import Language.NStar.CodeGen.Machine.Internal.X64.RegisterEncoding (registerNumber)
import Data.Located (Located((:@)), unLoc)
import Language.NStar.CodeGen.Machine.Internal.X64.SIB (sib)
import Language.NStar.CodeGen.Machine.X64.Expression (int8, compileExprX64)
import Language.NStar.Syntax.Core (Immediate(I))

compileSt :: Expr -> Expr -> Expr -> Compiler [InterOpcode]
compileSt (RegE src) (RegE o) (RegE p) = pure [rexW, Byte 0x89, modRM 0x0 (registerNumber (unLoc src)) 0x4, sib 0x0 (registerNumber (unLoc o)) (registerNumber (unLoc p))]
compileSt (RegE src) (RegE o) (NameE l _) = pure [rexW, Byte 0x89, modRM 0x2 (registerNumber (unLoc src)) (registerNumber (unLoc o)), Symbol32 (unLoc l) 0]
compileSt src@(ImmE _) (RegE o) (RegE p) = mappend [rexW, Byte 0xC7, modRM 0x0 0x0 0x4, sib 0x0 (registerNumber (unLoc o)) (registerNumber (unLoc p))] <$> compileExprX64 32 src
compileSt src@(ImmE _) (RegE o) (NameE l _) = mappend [rexW, Byte 0xC7, modRM 0x0 0x0 0x4, sib 0x0 (registerNumber (unLoc o)) 0x5, Symbol32 (unLoc l) 0] <$> compileExprX64 32 src
compileSt (RegE src) (ImmE (I o :@ _)) (RegE p) = pure $ [rexW, Byte 0x89, modRM 0x1 (registerNumber (unLoc p)) (registerNumber (unLoc src))] <> (Byte <$> int8 o)
compileSt (RegE src) (ImmE (I o :@ _)) (NameE l _) = pure $ [rexW, Byte 0x89, modRM 0x0 (registerNumber (unLoc src)) 0x4, sib 0x0 0x4 0x5, Symbol32 (unLoc l) o]
compileSt src@(ImmE _) (ImmE (I o :@ _)) (RegE p) = mappend ([rexW, Byte 0xC7, modRM 0x1 0x0 (registerNumber (unLoc p))] <> (Byte <$> int8 o)) <$> compileExprX64 32 src
compileSt src@(ImmE _) (ImmE (I o :@ _)) (NameE l _) = mappend [rexW, Byte 0xC7, modRM 0x0 0x0 0x4, sib 0x0 0x4 0x5, Symbol32 (unLoc l) o] <$> compileExprX64 32 src
compileSt r o p = internalError $ "Unsupported instruction 'st " <> show r <> ", " <> show o <> ", " <> show p <> "'"
