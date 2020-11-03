{-# LANGUAGE RecordWildCards #-}

module Data.Elf.Internal.Compile (unabstract) where

import Data.Elf.Types
import Data.Elf.Object
import qualified Data.Elf.Internal.Object as Internal
import Data.Elf.Internal.Compile.FileHeader
import Data.Elf.Internal.Compile.ProgramHeader
import Data.Elf.Internal.Compile.SectionHeader
import qualified Data.Elf.Internal.Compile.Fixups as Fix (runFixup, allFixes, FixupEnvironment(..))
import Unsafe.Coerce (unsafeCoerce)
import Data.Map (Map)
import qualified Data.Map as Map (fromList, mapKeys, keys, elems, insert)
import Data.Elf.SectionHeader (SectionHeader(..))
import Data.Elf.ProgramHeader (ProgramHeader(PPhdr, PLoad), section, pf_r)
import Data.Elf.FileHeader (ElfHeader)
import Data.Elf.Internal.SectionHeader (Elf_Shdr)
import Data.Elf.Internal.ProgramHeader (Elf_Phdr)
import Data.Elf.Internal.FileHeader (Elf_Ehdr)
import qualified Data.Text as Text (pack)
import Data.Maybe (mapMaybe)
import Data.List (intersperse)
import Data.Word (Word8)
import Data.Elf.Internal.BusSize (Size(..))
import Data.Elf.Internal.Compile.ForArch

-- | Transforms an abstract ELF object into a concrete ELF object.
--
--   This is essentially a simple alias on 'compileFor' specialized for ELF objects.
--
--   >>> unabstract = compileFor
unabstract :: ( ValueSet n
              , CompileFor n ElfHeader Elf_Ehdr
              , CompileFor n SectionHeader Elf_Shdr
              , CompileFor n ProgramHeader Elf_Phdr
              , CompileFor n ElfObject Internal.Object
              ) => ElfObject n -> Internal.Object n
unabstract = compileFor

instance ( ValueSet n
         , CompileFor n ElfHeader Elf_Ehdr
         , CompileFor n SectionHeader Elf_Shdr
         , CompileFor n ProgramHeader Elf_Phdr
         ) => CompileFor n ElfObject Internal.Object where
  compileFor ElfObject{..} =
    let elfheader = compileFor @n fileHeader

        sectNames = fetchSectionNamesFrom sections

        segs      = toSnd (compileFor @n) <$> (PPhdr : PLoad (section "PHDR") pf_r : segments)
                                                        --                      ^^^^ Special identifier, to refer to the PHDR segment

        allSectionNames = ".shstrtab" : Map.keys sectNames
        shstrtab  = SStrTab ".shstrtab" allSectionNames
        sects     = toSnd (compileFor @n) <$>
          (sections <> [shstrtab, SNull])
        newSectNames = Map.insert ".shstrtab" shstrtab sectNames

    in
      let Fix.FixupEnv fileHeader sections _ segments
                = Fix.runFixup Fix.allFixes $
                    Fix.FixupEnv @n elfheader (Map.fromList sects) (Map.mapKeys Text.pack newSectNames) (Map.fromList segs)

      in Internal.Obj @n fileHeader (Map.elems segments) (Map.elems sections) []

fetchSectionNamesFrom :: [SectionHeader n] -> Map String (SectionHeader n)
fetchSectionNamesFrom = Map.fromList . mapMaybe f
  where
    f SNull             = Nothing
    f h@(SProgBits n _ _) = Just (n, h)
    f h@(SNoBits n _ _)   = Just (n, h)
    f h@(SStrTab n _)     = Just (n, h)

c2w :: Char -> Word8
c2w = unsafeCoerce

toSnd :: (a -> b) -> a -> (a, b)
toSnd f x = (x, f x)
