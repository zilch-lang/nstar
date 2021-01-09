{-# LANGUAGE BinaryLiterals #-}

module Language.NStar.CodeGen.Machine.X64 (compileX64) where

import Language.NStar.CodeGen.Compiler
import Language.NStar.Syntax.Core hiding (Label)
import Language.NStar.Typechecker.Core
import Language.NStar.CodeGen.Machine.Internal.Intermediate (InterOpcode(..))
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

-- | REX with only the W bit set, so it indicates 64-bit operand size, but no high registers.
rexW :: InterOpcode
rexW = Byte 0x48

-- | Creates the ModR/M byte from the mode, the destination register and the source register\/memory.
modRM :: Word8    -- ^ [0b11]      register-direct addressing mode
                  --   [otherwise] register-indirect addressing mode
      -> Word8    -- ^ Destination register encoding, see 'registerNumber' for how to obtain it.
      -> Word8    -- ^ Source register encoding, see 'registerNumber' for how to obtain it.
      -> InterOpcode
modRM mod reg rm = Byte $
      ((mod .&. 0b11)  `shiftL` 6)
  .|. ((reg .&. 0b111) `shiftL` 3)
  .|. ((rm  .&. 0b111) `shiftL` 0)

-- | Associates registers with their 4-bits encoding.
--
--   See <https://wiki.osdev.org/X86-64_Instruction_Encoding#Registers the osdev wiki on registers encoding> for more information.
registerNumber :: Register -> Word8
registerNumber R0 = 0x0  -- rax
registerNumber R1 = 0x1  -- rcx
registerNumber R2 = 0x2  -- rdx
registerNumber R3 = 0x3  -- rbx
registerNumber SP = 0x4  -- rsp
registerNumber BP = 0x5  -- rbp
registerNumber R4 = 0x6  -- rsi
registerNumber R5 = 0x7  -- rdi

compileX64 :: TypedProgram -> Compiler ()
compileX64 = (fixupAddressesX64 =<<) . compileInterX64

compileInterX64 :: TypedProgram -> Compiler [InterOpcode]
compileInterX64 (TProgram stmts) = mconcat <$> mapM (compileStmtInterX64 . unLoc) stmts

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
compileInstrInterX64 (MOV src (Reg dst :@ _)) [Unsigned 64, Register 64] =
  ([rexW, Byte (0xB8 + registerNumber (unLoc dst))] <>) <$> compileExprX64 64 (unLoc src)
-- REX.W + 8B /r	MOV r64,r/m64	        RM	Valid	        N.E.	                Move r/m64 to r64.
compileInstrInterX64 (MOV src (Reg dst :@ _)) [Register 64, Register 64] = do
  let Reg s :@ _ = src
  pure [rexW, Byte 0x8B, modRM 0x3 (registerNumber (unLoc dst)) (registerNumber (unLoc s))]
-- E9 cd	        JMP rel32	        D	Valid	        Valid	                Jump near, relative, RIP = RIP + 32-bit displacement sign extended to 64-bits
compileInstrInterX64 (JMP (Name n :@ _) _) []                              =
  pure [Byte 0xE9, Jump (unLoc n)]
-- E8 cd	        CALL rel32	        D	Valid	        Valid	                Call near, relative, displacement relative to next instruction. 32-bit displacement sign extended to 64-bits in 64-bit mode.
compileInstrInterX64 (CALL (Name n :@ _) _) []                             =
  pure [Byte 0xE8, Jump (unLoc n)]
compileInstrInterX64 i args                                    =
  error $ "not yet implemented: compileInterInstrX64 " <> show i <> " " <> show args

compileExprX64 :: Integer -> Expr -> Compiler [InterOpcode]
compileExprX64 n (Imm i) = case (n, unLoc i) of
  (64, I int) -> pure (Byte <$> int64 int)
  (64, C c)   -> pure (Byte <$> char8 c)
-- ^^^^^ all these matches are intentionally left incomplete.

int64 :: Integer -> [Word8]
int64 = BS.unpack . runPut . putWord64le . fromIntegral

int32 :: Integer -> [Word8]
int32 = BS.unpack . runPut . putWord32le . fromIntegral

char8 :: Char -> [Word8]
char8 = BS.unpack . runPut . putWord8 . fromIntegral . ord

---------------------------------------------------------------------------------------------------------------------

fixupAddressesX64 :: [InterOpcode] -> Compiler ()
fixupAddressesX64 os = fixupAddressesX64Internal (findLabelsAddresses os) os 0
 where
   fixupAddressesX64Internal :: Map Text Integer -> [InterOpcode] -> Integer -> Compiler ()
   fixupAddressesX64Internal labelsAddresses [] _             = tell (MInfo [] labelsAddresses)
   fixupAddressesX64Internal labelsAddresses (Byte b:os) i    = tell (MInfo [b] mempty) *> fixupAddressesX64Internal labelsAddresses os (i + 1)
   fixupAddressesX64Internal labelsAddresses (Label n:os) i   = fixupAddressesX64Internal labelsAddresses os i
   fixupAddressesX64Internal labelsAddresses (Jump n:os) i    = do
     let addr = maybe (internalError $ "Label " <> show n <> " not found during codegen.") (int32 . (subtract (i + 4))) (Map.lookup n labelsAddresses)
     tell (MInfo addr mempty) *> fixupAddressesX64Internal labelsAddresses os (i + 4)

findLabelsAddresses :: [InterOpcode] -> Map Text Integer
findLabelsAddresses = findLabelsAddressesInternal 0
  where
    findLabelsAddressesInternal :: Integer -> [InterOpcode] -> Map Text Integer
    findLabelsAddressesInternal n []            = mempty
    findLabelsAddressesInternal n (Byte _:os)   = findLabelsAddressesInternal (n + 1) os
    findLabelsAddressesInternal n (Label l:os)  = Map.insert l n (findLabelsAddressesInternal n os)
    findLabelsAddressesInternal n (Jump l:os)   = findLabelsAddressesInternal (n + 4) os
