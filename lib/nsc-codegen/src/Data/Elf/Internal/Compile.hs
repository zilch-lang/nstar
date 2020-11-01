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
import qualified Data.Text as Text (pack)
import Data.Maybe (mapMaybe)
import Data.List (intercalate)

unabstract :: Object64 -> Internal.Object64
unabstract Object64{..} =
  let elfheader = compileFileHeader64bits fileHeader

      sectNames = fetchSectionNamesFrom sections

      segs      = toSnd compileProgramHeader64bits <$> segments

      sects     = toSnd compileSectionHeader64bits <$> (sections <> [SStrTab ".shstrtab" (intercalate [0x0] (fmap c2w <$> Map.keys sectNames))])


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
