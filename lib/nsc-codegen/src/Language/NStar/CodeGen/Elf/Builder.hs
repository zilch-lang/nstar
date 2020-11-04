{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE KindSignatures #-}

module Language.NStar.CodeGen.Elf.Builder (compileToElf) where

import Data.Elf hiding (compile)
import Language.NStar.CodeGen.Machine
import Data.Bits ((.|.))
import Language.NStar.CodeGen.Arch (SupportedArch(..))
import Language.NStar.Typechecker.Core (TypedProgram)
import Language.NStar.CodeGen.Compiler (MachineInfo(..))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text (unpack)

class CompileToElf (n :: Size) where
  compileToElf :: SupportedArch -> TypedProgram -> ElfObject n

instance CompileToElf S64 where
  compileToElf arch prog =
    let MInfo opcodes syms = compile arch prog
    in ElfObject
        (ElfHeader (supportedArchToClass arch) (supportedArchToEncoding arch) OSABI_None 0x0 ET_Exec (supportedArchToArch arch) EV_Current ef_none)
        [PLoad (section ".text") (pf_r .|. pf_x)]
        [ SProgBits ".text" opcodes (shf_alloc .|. shf_execinstr)
        , SSymTab ".strtab" (generateSymbolTableFrom @S64 syms) ]


generateSymbolTableFrom :: forall (n :: Size). Map Text Integer -> [ElfSymbol]
generateSymbolTableFrom _ = []

supportedArchToArch :: SupportedArch -> Arch
supportedArchToArch X64 = EM_x86_64

supportedArchToClass :: SupportedArch -> Class
supportedArchToClass X64 = C_64

supportedArchToEncoding :: SupportedArch -> Encoding
supportedArchToEncoding X64 = D_2LSB
