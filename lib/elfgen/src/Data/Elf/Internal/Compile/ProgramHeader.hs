module Data.Elf.Internal.Compile.ProgramHeader where

import Data.Elf.ProgramHeader
import Data.Elf.Internal.ProgramHeader

-- | Compiles an abstract 'ProgramHeader' into an 'Elf64_Phdr' ready to be inserted into an object file.
--
--   __NOTE:__ We need to apply multiple fixup steps to fill the given fields:
--
--             - 'p_offset'
--             - 'p_vaddr'
--             - 'p_paddr'
--             - 'p_filesz'
--             - 'p_memsz'
compileProgramHeader64bits :: ProgramHeader -> Elf_Phdr 64
compileProgramHeader64bits prog =
  Elf64_Phdr
    compileType64bits
    (fromIntegral compileFlags64bits)
    0x0
    0x0
    0x0
    0x0
    0x0
    compileAlignment64bits
  where
    compileType64bits = case prog of
      PPhdr       -> pt_phdr
      PNull       -> pt_null
      PLoad _ _   -> pt_load
      PInterp _ _ -> pt_interp
    compileFlags64bits = case prog of
      PPhdr       -> pf_r
      PNull       -> 0x0
      PLoad _ f   -> f
      PInterp _ f -> f
    compileAlignment64bits = case prog of
      PPhdr       -> 0x8
      PNull       -> 0x0
      PLoad _ _   -> 0x1000
      PInterp _ _ -> 0x1
