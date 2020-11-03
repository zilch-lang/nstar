{-# LANGUAGE DataKinds #-}

module Language.NStar.CodeGen.Elf.Builder where

import Data.Elf hiding (compile)
import Language.NStar.CodeGen.Machine
import Data.Bits ((.|.))
import Language.NStar.CodeGen.Arch (SupportedArch(..))
import Language.NStar.Typechecker.Core (TypedProgram)

compileToElf :: SupportedArch -> TypedProgram -> ElfObject S64
compileToElf arch prog =
  ElfObject
    (ElfHeader (supportedArchToClass arch) (supportedArchToEncoding arch) OSABI_None 0x0 ET_Exec (supportedArchToArch arch) EV_Current ef_none)
    [PLoad (section ".text") pf_r]
    [SProgBits ".text" [] {- (compile arch prog) -} (shf_alloc .|. shf_execinstr)]
    [] -- Leave empty for now, will populate later


supportedArchToArch :: SupportedArch -> Arch
supportedArchToArch X64 = EM_x86_64

supportedArchToClass :: SupportedArch -> Class
supportedArchToClass X64 = C_64

supportedArchToEncoding :: SupportedArch -> Encoding
supportedArchToEncoding X64 = D_2LSB
