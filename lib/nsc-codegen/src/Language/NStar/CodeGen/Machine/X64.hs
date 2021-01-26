{-# LANGUAGE BinaryLiterals #-}

module Language.NStar.CodeGen.Machine.X64 (compileX64) where

import Language.NStar.CodeGen.Compiler
import Language.NStar.Syntax.Core hiding (Label)
import Language.NStar.Typechecker.Core
import Language.NStar.CodeGen.Machine.Internal.Intermediate (InterOpcode(..))
import Language.NStar.CodeGen.Machine.Internal.X64.SIB
import Language.NStar.CodeGen.Machine.Internal.X64.ModRM
import Language.NStar.CodeGen.Machine.Internal.X64.RegisterEncoding (registerNumber)
import Data.Located (unLoc, Located(..))
import Language.NStar.CodeGen.Errors
import Control.Monad.Except (throwError)
import Internal.Error (internalError)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Word (Word8)
import Data.Bits ((.&.), shiftR)
import Control.Monad.Writer (tell)
import Data.Text (Text)
import Data.Binary.Put
import qualified Data.ByteString.Lazy as BS (unpack)
import Data.Char (ord)
import Data.Bits ((.&.), (.|.), shiftL)
import Debug.Trace (traceShow)
import Data.Bifunctor (second)
import Data.Elf (RelocationType(..))

compileX64 :: TypedProgram -> Compiler ()
compileX64 prog@(TProgram (TData dataSect :@ _) _ _ _) = do
  intermediateOpcodes <- compileInterX64 prog
  generateDataX64 dataSect
  fixupAddressesX64 intermediateOpcodes

compileInterX64 :: TypedProgram -> Compiler [InterOpcode]
compileInterX64 (TProgram (TData _ :@ _) (TROData _ :@ _) (TUData _ :@ _) (TCode stmts :@ _)) =
  mconcat <$> mapM (compileStmtInterX64 . unLoc) stmts

compileStmtInterX64 :: TypedStatement -> Compiler [InterOpcode]
compileStmtInterX64 (TLabel name) = pure [Label (unLoc name)]
compileStmtInterX64 (TInstr i ts) = compileInstrInterX64 (unLoc i) (unLoc <$> ts)

-- Opcode*	        Instruction	        Op/En	64-Bit Mode	Compat/Leg Mode	Description
compileInstrInterX64 :: Instruction -> [Type] -> Compiler [InterOpcode]
-- C3	                RET	                ZO	Valid	        Valid	                Near return to calling procedure.
compileInstrInterX64 RET []                                    =
  pure [Byte 0xC3]
compileInstrInterX64 RET args                                  =
  internalError $ "Expected [] but got " <> show args <> " as arguments for " <> show RET
-- REX.W + B8+ rd io	MOV r64, imm64	        OI	Valid	        N.E.	                Move imm64 to r64.
compileInstrInterX64 (MOV (Name n :@ _) (Reg dst :@ _)) [_, _] =
  pure [rexW, Byte $ 0xB8 + registerNumber (unLoc dst), Symbol64 (unLoc n)]
-- REX.W + 8B /r	MOV r64,r/m64	        RM	Valid	        N.E.	                Move r/m64 to r64.
compileInstrInterX64 (MOV (Indexed (Imm (I disp :@ _) :@ _) (Name l :@ _) :@ _) (Reg dst :@ _)) [_, _] =
  pure [rexW, Byte 0x8B, modRM 0x0 (registerNumber (unLoc dst)) 0x4, sib 0x0 0x4 0x5, Symbol32 (unLoc l) disp]
-- REX.W + 8B /r	MOV r64,r/m64	        RM	Valid	        N.E.	                Move r/m64 to r64.
compileInstrInterX64 (MOV (Indexed (Imm (I disp :@ _) :@ _) (Reg src :@ _) :@ _) (Reg dst :@ _)) [_, _] =
  pure $ [rexW, Byte 0x8B, modRM 0x1 (registerNumber (unLoc dst)) (registerNumber (unLoc src))] <> (Byte <$> int8 disp)
-- REX.W + 8B /r	MOV r64,r/m64	        RM	Valid	        N.E.	                Move r/m64 to r64.
compileInstrInterX64 (MOV (Indexed (Reg r1 :@ _) (Reg r2 :@ _) :@ _) (Reg dst :@ _)) [_, _] =
  pure $ [rexW, Byte 0x8B, modRM 0x0 (registerNumber (unLoc dst)) 0x4, sib 0x0 (registerNumber (unLoc r1)) (registerNumber (unLoc r2))]
-- REX.W + 89 /r	MOV r/m64,r64	        MR	Valid	        N.E.	                Move r64 to r/m64.
compileInstrInterX64 (MOV (Reg src :@ _) (Indexed (Reg r1 :@ _) (Reg r2 :@ _) :@ _)) [_, _] =
  pure $ [rexW, Byte 0x89, modRM 0x0 (registerNumber (unLoc src)) 0x4, sib 0x0 (registerNumber (unLoc r1)) (registerNumber (unLoc r2))]
-- REX.W + 89 /r	MOV r/m64,r64	        MR	Valid	        N.E.	                Move r64 to r/m64.
compileInstrInterX64 (MOV (Indexed (Reg r1 :@ _) (Name n :@ _) :@ _) (Reg dst :@ _)) [_, _] =
  pure $ [rexW, Byte 0x8B, modRM 0x2 (registerNumber (unLoc dst)) (registerNumber (unLoc r1)), Symbol32 (unLoc n) 0]
-- REX.W + B8+ rd io	MOV r64, imm64	        OI	Valid	        N.E.	                Move imm64 to r64.
compileInstrInterX64 (MOV src@(Imm _ :@ _) (Reg dst :@ _)) [_, _] =
  ([rexW, Byte (0xB8 + registerNumber (unLoc dst))] <>) <$> compileExprX64 64 (unLoc src)
-- REX.W + 8B /r	MOV r64,r/m64	        RM	Valid	        N.E.	                Move r/m64 to r64.
compileInstrInterX64 (MOV (Reg src :@ _) (Reg dst :@ _)) [_, _] =
  pure [rexW, Byte 0x8B, modRM 0x3 (registerNumber (unLoc dst)) (registerNumber (unLoc src))]
-- E9 cd	        JMP rel32	        D	Valid	        Valid	                Jump near, relative, RIP = RIP + 32-bit displacement sign extended to 64-bits
compileInstrInterX64 (JMP (Name n :@ _) _) []                              =
  pure [Byte 0xE9, Jump (unLoc n)]
-- E8 cd	        CALL rel32	        D	Valid	        Valid	                Call near, relative, displacement relative to next instruction. 32-bit displacement sign extended to 64-bits in 64-bit mode.
compileInstrInterX64 (CALL (Name n :@ _) _) []                             =
  pure [Byte 0xE8, Jump (unLoc n)]
-- 50+rd	        PUSH r64	        O	Valid	        N.E.	                Push r64.
compileInstrInterX64 (PUSH (Reg src :@ _)) [_] =
  pure [Byte $ 0x50 + registerNumber (unLoc src)]
-- 68 id	        PUSH imm32	        I	Valid	        Valid	                Push imm32.
compileInstrInterX64 (PUSH src) [_] =
  ([Byte 0x68] <>) <$> compileExprX64 32 (unLoc src)
-- 58+ rd	        POP r64	        O	Valid	        N.E.	                Pop top of stack into r64; increment stack pointer. Cannot encode 32-bit operand size.
compileInstrInterX64 (POP (Reg src :@ _)) [_] =
  pure [Byte $ 0x58 + registerNumber (unLoc src)]
-- 8F /0	        POP r/m64	        M	Valid	        N.E.	                Pop top of stack into m64; increment stack pointer. Cannot encode 32-bit operand size.
compileInstrInterX64 (POP (Indexed (Imm (I disp :@ _) :@ _) (Name l :@ _) :@ _)) [_] =
  pure [Byte 0x8F, modRM 0x0 0x0 0x4, sib 0x0 0x4 0x5, Symbol32 (unLoc l) disp]
-- REX.W + 01 /r	ADD r/m64, r64	        MR	Valid	        N.E.	                Add r64 to r/m64.
compileInstrInterX64 (ADD (Reg src :@ _) (Reg dst :@ _)) [_] =
  pure [rexW, Byte 0x01, modRM 0x3 (registerNumber (unLoc dst)) (registerNumber (unLoc src))]
-- REX.W + 81 /0 id	ADD r/m64, imm32	MI	Valid	        N.E.	                Add imm32 sign-extended to 64-bits to r/m64.
compileInstrInterX64 (ADD src@(Imm _ :@ _) (Reg dst :@ _)) [_, _] =
  ([rexW, Byte 0x81, modRM 0x3 (registerNumber (unLoc dst)) 0x0] <>) <$> compileExprX64 64 (unLoc src)
-- REX.W + 81 /5 id	SUB r/m64, imm32	MI	Valid	        N.E.	                Subtract imm32 sign-extended to 64-bits from r/m64.
compileInstrInterX64 (SUB src@(Imm _ :@ _) (Reg dst :@ _)) [_, _] =
  ([rexW, Byte 0x81, modRM 0x3 (registerNumber (unLoc dst)) 0x5] <>) <$> compileExprX64 64 (unLoc src)
-- NP 90	        NOP	                ZO	Valid	        Valid	                One byte no-operation instruction.
compileInstrInterX64 NOP [] =
  pure [Byte 0x90]
compileInstrInterX64 i args                                    =
  error $ "not yet implemented: compileInterInstrX64 " <> show i <> " " <> show args

compileExprX64 :: Integer -> Expr -> Compiler [InterOpcode]
compileExprX64 n (Imm i) = case (n, unLoc i) of
  (32, I int) -> pure (Byte <$> int32 int)
  (64, I int) -> pure (Byte <$> int64 int)
  (64, C c)   -> pure (Byte <$> char8 c)
-- ^^^^^ all these matches are intentionally left incomplete.

int64 :: Integer -> [Word8]
int64 = BS.unpack . runPut . putInt64le . fromIntegral

int32 :: Integer -> [Word8]
int32 = BS.unpack . runPut . putInt32le . fromIntegral

int8 :: Integer -> [Word8]
int8 = BS.unpack . runPut . putInt8 . fromIntegral

char8 :: Char -> [Word8]
char8 = BS.unpack . runPut . putWord8 . fromIntegral . ord

compileConstantX64 :: Constant -> [Word8]
compileConstantX64 (CInteger (i :@ _))   = int64 i
compileConstantX64 (CCharacter (c :@ _)) = char8 c
compileConstantX64 (CArray csts)         = mconcat (compileConstantX64 . unLoc <$> csts)

---------------------------------------------------------------------------------------------------------------------

fixupAddressesX64 :: [InterOpcode] -> Compiler ()
fixupAddressesX64 os = fixupAddressesX64Internal (findLabelsAddresses os) os 0
 where
   fixupAddressesX64Internal :: Map Text Integer -> [InterOpcode] -> Integer -> Compiler ()
   fixupAddressesX64Internal labelsAddresses [] _             = tell (MInfo mempty (second Function <$> Map.assocs labelsAddresses) mempty mempty)
   fixupAddressesX64Internal labelsAddresses (Byte b:os) i    = tell (MInfo [b] mempty mempty mempty) *> fixupAddressesX64Internal labelsAddresses os (i + 1)
   fixupAddressesX64Internal labelsAddresses (Label n:os) i   = fixupAddressesX64Internal labelsAddresses os i
   fixupAddressesX64Internal labelsAddresses (Jump n:os) i    = do
     let addr = maybe (internalError $ "Label " <> show n <> " not found during codegen.") (int32 . (subtract (i + 4))) (Map.lookup n labelsAddresses)
     tell (MInfo addr mempty mempty mempty) *> fixupAddressesX64Internal labelsAddresses os (i + 4)
   -- TODO: implement this
   fixupAddressesX64Internal labelsAddresses (Symbol32 s o:os) i  = do
     tell (MInfo (int32 0x0) mempty mempty [RelocText s ".data" o i R_x86_64_32s])
                                               --       ^^^^^^^ FIXME: do not hardcode origin section
     fixupAddressesX64Internal labelsAddresses os (i + 4)
   fixupAddressesX64Internal labelsAddresses (Symbol64 s:os) i = do
     tell (MInfo (int64 0x0) mempty mempty [RelocText s ".data" 0x0 i R_x86_64_64])
                                              --        ^^^^^^^ FIXME: do not hardcode origin section
     fixupAddressesX64Internal labelsAddresses os (i + 8)

findLabelsAddresses :: [InterOpcode] -> Map Text Integer
findLabelsAddresses = findLabelsAddressesInternal 0
  where
    findLabelsAddressesInternal :: Integer -> [InterOpcode] -> Map Text Integer
    findLabelsAddressesInternal n []                = mempty
    findLabelsAddressesInternal n (Byte _:os)       = findLabelsAddressesInternal (n + 1) os
    findLabelsAddressesInternal n (Label l:os)      = Map.insert l n (findLabelsAddressesInternal n os)
    findLabelsAddressesInternal n (Jump l:os)       = findLabelsAddressesInternal (n + 4) os
    findLabelsAddressesInternal n (Symbol32 s o:os) = findLabelsAddressesInternal (n + 4) os
    findLabelsAddressesInternal n (Symbol64 s:os)   = findLabelsAddressesInternal (n + 8) os

--------------------------------------------------------------------------------------------------------------------

generateDataX64 :: [Located Binding] -> Compiler ()
generateDataX64 = generateDataX64Internal 0
  where
    generateDataX64Internal :: Integer -> [Located Binding] -> Compiler ()
    generateDataX64Internal _ [] = pure ()
    generateDataX64Internal off ((Bind (name :@ _) _ (val :@ _) :@ _) : bs) = do
      let cst = compileConstantX64 val
      tell (MInfo mempty mempty (DataTable [(name, off)] cst) mempty)
      generateDataX64Internal (off + fromIntegral (length cst)) bs
