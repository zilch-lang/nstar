module Language.NStar.CodeGen.Machine.Internal.X64.RegisterEncoding where

import Language.NStar.Syntax.Core (Register(..))
import Data.Word (Word8)

-- | Associates registers with their 4 bit encoding.
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
