{-# LANGUAGE TypeApplications #-}

module Data.Elf.Internal.Compile.FileHeader where

-- compile an abstracted FileHeader into an unabstracted one

import Data.Elf.FileHeader
import Data.Elf.Internal.FileHeader
import Data.Elf.Internal.SectionHeader (Elf_Shdr)   --  ↓
import Data.Elf.Internal.ProgramHeader (Elf_Phdr)   -- Only to compute their sizes
import Data.Elf.Types
import Foreign.Storable (sizeOf)
import Data.Elf.Internal.BusSize (Size(..))

-- | Compiles an abstracted 'ElfHeader' into an unabstracted 'Elf64_Ehdr'.
--
--   __NOTE:__ The generated ELF header needs to undergo various fixup steps because of the dummy values
--             put here (like the number of section, their size, etc). There isn't enough information here
--             to be able to fill some of those fields, namely:
--
--             - 'e_entry'
--             - 'e_phoff'
--             - 'e_shoff'
--             - 'e_phnum'
--             - 'e_shnum'
--             - 'e_shstrndx'
compileFileHeader64bits :: ElfHeader -> Elf_Ehdr S64
compileFileHeader64bits (ElfHeader cls enc abi abiVer fileType arch fileVer flags) =
  Elf64_Ehdr
    [ 0x7F, 0x45, 0x4C, 0x46, compileClass64bits, compileEncoding64bits, fromIntegral compileFileVersion64bits, compileABI64bits, abiVer, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0 ]
    compileFileType64bits
    compileArch64bits
    compileFileVersion64bits
    0x0
    0x0
    0x0
    (fromIntegral flags)
    (fromIntegral $ sizeOf @(Elf_Ehdr S64) undefined)
    (fromIntegral $ sizeOf @(Elf_Phdr S64) undefined)
    0x0
    (fromIntegral $ sizeOf @(Elf_Shdr S64) undefined)
    0x0
    0x0
  where
    compileClass64bits = case cls of
      C_None -> elfclassnone
      C_32   -> elfclass32
      C_64   -> elfclass64
    compileEncoding64bits = case enc of
      D_None -> elfdatanone
      D_2LSB -> elfdata2lsb
      D_2MSB -> elfdata2msb
    compileFileVersion64bits = case fileVer of
      EV_None    -> ev_none
      EV_Current -> ev_current
    compileABI64bits = case abi of
      OSABI_None -> elfosabi_none
      OSABI_SysV -> elfosabi_sysv
    compileFileType64bits = case fileType of
      ET_None -> et_none
      ET_Rel  -> et_rel
      ET_Exec -> et_exec
      ET_Dyn  -> et_dyn
      ET_Core -> et_core
    compileArch64bits = case arch of
      EM_None   -> em_none
      EM_sparc  -> em_sparc
      EM_x86_64 -> em_x86_64
      EM_arm    -> em_arm
