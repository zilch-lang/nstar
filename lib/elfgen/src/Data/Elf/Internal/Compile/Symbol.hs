module Data.Elf.Internal.Compile.Symbol where

import Data.Elf.Internal.Compile.ForArch
import Data.Elf.Symbol
import Data.Elf.Internal.Symbol
import Data.Bits (shiftL, shiftR, (.&.))
import Data.Elf.Internal.BusSize (Size(..))

instance CompileFor S64 ElfSymbol Elf_Sym where
  -- | Compiles an abstract ELF symbol into a concrete symbol.
  --
  --   Fields 'st_name', 'st_value', 'st_shndx', 'st_size' must be fixed afterwards.
  compileFor (ElfSymbol name symType symBind symVis) =
    Elf_Sym @S64
      0x0
      compileTypeAndBind64bits
      compileVis64bits
      0x0
      0x0
      0x0
     where
       compileTypeAndBind64bits =
         let ty = case symType of
               ST_NoType     -> stt_notype @S64
               ST_Object     -> stt_object @S64
               ST_Func _     -> stt_func @S64
               ST_Section _  -> stt_section @S64
               s             -> error "not yet implemented: ST_FILE | ST_Common | ST_TLS"
             bind = case symBind of
               SB_Local  -> stb_local @S64
               SB_Global -> stb_global @S64
               SB_Weak   -> stb_weak @S64
         in (bind `shiftL` 4) + (ty .&. 0xF)
       compileVis64bits = case symVis of
         SV_Default   -> stv_default @S64
         SV_Internal  -> stv_internal @S64
         SV_Hidden    -> stv_hidden @S64
         SV_Protected -> stv_protected @S64

instance CompileFor S64 RelocationSymbol Elf_Rela where
  -- | Compiles an abstract relocation table entry into a concrete table entry.
  --
  --   Fields 'r_info' and 'r_addend' must be fixed afterwards.
  compileFor (RelocationSymbol name relType offset) =
    Elf_Rela @S64
      (fromIntegral offset)
      compileInfo64bits
      0x0
    where
      compileInfo64bits =
        fromIntegral case relType of
          R_x86_64_None -> r_x86_64_none @S64
          R_x86_64_64   -> r_x86_64_64 @S64
          R_x86_64_32   -> r_x86_64_32 @S64
          R_x86_64_32S  -> r_x86_64_32s @S64
