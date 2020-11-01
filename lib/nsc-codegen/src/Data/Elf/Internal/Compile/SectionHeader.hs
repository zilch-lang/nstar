module Data.Elf.Internal.Compile.SectionHeader where

import Data.Elf.SectionHeader
import Data.Elf.Internal.SectionHeader

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
