module Data.Elf.Internal.Compile.ProgramHeader where

import Data.Elf.ProgramHeader
import Data.Elf.Internal.ProgramHeader
import Data.Elf.Internal.BusSize (Size(..))
import Data.Elf.Internal.Compile.ForArch

instance CompileFor S64 ProgramHeader Elf_Phdr where
  -- | Compiles an abstract 'ProgramHeader' into an 'Elf_Phdr' ready to be inserted into an object file.
  --
  --   __NOTE:__ We need to apply multiple fixup steps to fill the given fields:
  --
  --             - 'p_offset'
  --             - 'p_vaddr'
  --             - 'p_paddr'
  --             - 'p_filesz'
  --             - 'p_memsz'
  compileFor prog =
    Elf_Phdr @S64
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
        PPhdr       -> pt_phdr @S64
        PNull       -> pt_null @S64
        PLoad _ _   -> pt_load @S64
        PInterp _ _ -> pt_interp @S64
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
