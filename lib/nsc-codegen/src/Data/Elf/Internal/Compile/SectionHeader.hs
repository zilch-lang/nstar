module Data.Elf.Internal.Compile.SectionHeader where

import Data.Elf.SectionHeader
import Data.Elf.Internal.SectionHeader

-- | Compiles an abstract 'SectionHeader' into a concrete 'Elf64_Shdr'.
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
compileSectionHeader64bits :: SectionHeader -> Elf64_Shdr
compileSectionHeader64bits sect =
  Elf64_Shdr
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
      SNull           -> sht_null
      SProgBits _ _ _ -> sht_progbits
      SNoBits _ _ _   -> sht_nobits
    compileFlags64bits = case sect of
      SNull           -> 0x0
      SProgBits _ _ f -> f
      SNoBits _ _ f   -> f
    compileAlignment64bits = case sect of
      SNull           -> 0x0
      SProgBits _ _ _ -> 0x1
      SNoBits _ _ _   -> 0x20
