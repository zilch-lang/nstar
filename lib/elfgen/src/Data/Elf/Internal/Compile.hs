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
import qualified Data.Map as Map (fromList, mapKeys, keys, elems)
import Data.Elf.SectionHeader
import Data.Elf.ProgramHeader
import qualified Data.Text as Text (pack)
import Data.Maybe (mapMaybe)
import Data.List (intersperse)

unabstract :: Object64 -> Internal.Object64
unabstract Object64{..} =
  let elfheader = compileFileHeader64bits fileHeader

      sectNames = fetchSectionNamesFrom sections

      segs      = toSnd compileProgramHeader64bits <$> (PPhdr : PLoad (section "PHDR") pf_r : segments)
                                                        --                      ^^^^ Special identifier, to refer to the PHDR segment

      allSectionNames = 0x0 : intersperse 0x0 (c2w <$> mconcat (".shstrtab" : Map.keys sectNames)) <> [0x0]
      sects     = toSnd compileSectionHeader64bits <$>
        (sections <> [SStrTab ".shstrtab" allSectionNames])


  in
  let Fix.FixupEnv fileHeader sections _ segments
                = Fix.runFixup Fix.allFixes $
                    Fix.FixupEnv elfheader (Map.fromList sects) (Map.mapKeys Text.pack sectNames) (Map.fromList segs)

  in Internal.Obj64 fileHeader (Map.elems segments) (Map.elems sections) []

fetchSectionNamesFrom :: [SectionHeader] -> Map String SectionHeader
fetchSectionNamesFrom = Map.fromList . mapMaybe f
  where
    f SNull             = Nothing
    f h@(SProgBits n _ _) = Just (n, h)
    f h@(SNoBits n _ _)   = Just (n, h)
    f h@(SStrTab n _)     = Just (n, h)

c2w :: Char -> UChar
c2w = unsafeCoerce

toSnd :: (a -> b) -> a -> (a, b)
toSnd f x = (x, f x)
