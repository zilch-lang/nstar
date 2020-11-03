module Data.Elf.Internal.Compile.SectionHeader where

import Data.Elf.SectionHeader
import Data.Elf.Internal.SectionHeader
import Data.Elf.Internal.BusSize (Size(..))
import Data.Elf.Internal.Compile.ForArch

instance CompileFor S64 SectionHeader Elf_Shdr where
  -- | Compiles an abstract 'SectionHeader' into a concrete 'Elf_Shdr'.
  --
  --   __NOTE:__ Some fields are not filled, because of the lack of information at that current point.
  --             These are listed here:
  --
  --             - 'sh_name'
  --             - 'sh_addr'
  --             - 'sh_offset'
  --             - 'sh_size'
  --             - 'sh_link'
  --             - 'sh_info'
  --             - 'sh_addralign'
  --             - 'sh_entsize' (may not be filled if the section does not hold a table)
  compileFor sect =
    Elf_Shdr @S64
      0x0
      compileType64bits
      (fromIntegral compileFlags64bits)
      0x0
      0x0
      0x0
      0x0
      0x0
      compileAlignment64bits
      0x0
    where
      compileType64bits = case sect of
        SNull           -> sht_null @S64
        SProgBits _ _ _ -> sht_progbits @S64
        SNoBits _ _ _   -> sht_nobits @S64
        SStrTab _ _     -> sht_strtab @S64
      compileFlags64bits = case sect of
        SNull           -> 0x0
        SProgBits _ _ f -> f
        SNoBits _ _ f   -> f
        SStrTab _ _     -> 0x0
      compileAlignment64bits = case sect of
        SNull           -> 0x0
        SProgBits _ _ _ -> 0x1
        SNoBits _ _ _   -> 0x20
        SStrTab _ _     -> 0x1
