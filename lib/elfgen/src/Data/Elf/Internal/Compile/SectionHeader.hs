module Data.Elf.Internal.Compile.SectionHeader where

import Data.Elf.SectionHeader
import Data.Elf.Internal.SectionHeader
import Data.Elf.Internal.BusSize (Size(..))
import Data.Elf.Internal.Compile.ForArch
import Data.Elf.Internal.Symbol
import Foreign.Storable (sizeOf)

instance CompileFor S64 SectionHeader Elf_Shdr where
  -- | Compiles an abstract 'SectionHeader' into a concrete 'Elf_Shdr'.
  --
  --   __NOTE:__ Some fields are not filled, because of the lack of information at that current point.
  --             These are listed here:
  --
  --             - 'sh_name'
  --             - 'sh_addr'
  --             - 'sh_offset'
  --             - 'sh_size' (may not be filled if size cannot be determined here)
  --             - 'sh_link'
  --             - 'sh_info'
  --             - 'sh_addralign'
  compileFor sect =
    Elf_Shdr @S64
      0x0
      compileType64bits
      (fromIntegral compileFlags64bits)
      0x0
      0x0
      compileSize64bits
      0x0
      0x0
      compileAlignment64bits
      compileEntsize64bits
    where
      compileType64bits = case sect of
        SNull           -> sht_null @S64
        SProgBits _ _ _ -> sht_progbits @S64
        SNoBits _ _ _   -> sht_nobits @S64
        SStrTab _ _     -> sht_strtab @S64
        SSymTab _ _     -> sht_symtab @S64
        SRela _ _       -> sht_rela @S64
      compileFlags64bits = case sect of
        SNull           -> 0x0
        SProgBits _ _ f -> f
        SNoBits _ _ f   -> f
        SStrTab _ _     -> 0x0
        SSymTab _ _     -> 0x0
        SRela _ _       -> 0x0
      compileAlignment64bits = case sect of
        SNull           -> 0x0
        SProgBits _ _ _ -> 0x1
        SNoBits _ _ _   -> 0x20
        SStrTab _ _     -> 0x1
        SSymTab _ _     -> 0x8
        SRela _ _       -> 0x8
      compileEntsize64bits = case sect of
        SNull           -> 0x0
        SProgBits _ _ _ -> 0x0
        SNoBits _ _ _   -> 0x0
        SStrTab _ _     -> 0x0
        SSymTab _ _     -> fromIntegral $ sizeOf @(Elf_Sym S64) undefined
        SRela _ _       -> fromIntegral $ sizeOf @(Elf_Rela S64) undefined
      compileSize64bits = case sect of
        SNull           -> 0x0
        SProgBits _ _ _ -> 0x0
        SNoBits _ _ _   -> 0x0
        SStrTab _ _     -> 0x0
        SSymTab _ s     -> fromIntegral (sizeOf @(Elf_Sym S64) undefined) * (fromIntegral (length s) + 1)
        SRela _ s       -> fromIntegral $ sizeOf @(Elf_Rela S64) undefined * fromIntegral (length s)
