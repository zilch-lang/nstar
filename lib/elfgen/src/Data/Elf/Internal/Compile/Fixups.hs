{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

module Data.Elf.Internal.Compile.Fixups
( FixupEnvironment(..), Fixup
, SectionAList, SegmentAList, SectionByName
, runFixup
  -- * All fixup steps
, allFixes, fixupHeaderCount, fixupShstrtabIndex, fixupHeadersOffsets, fixupPHDREntry
) where

import Data.Elf.SectionHeader
import Data.Elf.Internal.SectionHeader
import Data.Elf.ProgramHeader
import Data.Elf.Internal.ProgramHeader
import Data.Elf.Internal.FileHeader
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import Control.Monad.State (State, get, put, execState)
import Data.Functor ((<&>))
import Data.Elf.Internal.BusSize (Size(..))
import Data.List (elemIndex)
import Data.Elf.Types (ValueSet)

-- | Associative list between abstract and concrete section header structures.
type SectionAList n = Map (SectionHeader n) (Elf_Shdr n)
-- | Associative list between abstract and concrete program header structures.
type SegmentAList n = Map (ProgramHeader n) (Elf_Phdr n)
-- | Mapping from section names to abstract section headers.
type SectionByName n = Map Text (SectionHeader n)

-- | The fixup environment, containing all sections to be fixed up.
data FixupEnvironment (n :: Size)
  = FixupEnv
      (Elf_Ehdr n)         -- ^ The ELF file header
      (SectionAList n)     -- ^ An association list associating abstract and concrete section headers
      (SectionByName n)    -- ^ All segments mapped by their respective names
      (SegmentAList n)     -- ^ An association from abstract to concrete segment headers

type Fixup (n :: Size) a = State (FixupEnvironment n) a

-- | Run a given fixup step (or multiple) with the given initial environment.
runFixup :: ValueSet n => Fixup n a -> FixupEnvironment n -> FixupEnvironment n
runFixup = execState

-- | Contains all fixes to do.
allFixes :: ValueSet n => Fixup n ()
allFixes = do
  fixupHeaderCount
  fixupShstrtabIndex
  fixupHeadersOffsets
  fixupPHDREntry
  fixupSectionNames

-- | A fix for headers count in the ELF file header (fields 'e_phnum' and 'e_shnum').
fixupHeaderCount :: ValueSet n => Fixup n ()
fixupHeaderCount = do
  FixupEnv fileHeader sects sectsNames segs <- get
  let newHeader = fileHeader
        { e_phnum = fromIntegral (Map.size segs)
        , e_shnum = fromIntegral (Map.size sects) }
  put (FixupEnv newHeader sects sectsNames segs)

fixupShstrtabIndex :: ValueSet n => Fixup n ()
fixupShstrtabIndex = do
  FixupEnv fileHeader sects sectsNames segs <- get
  let Just e_shstrtabndx  = elemIndex ".shstrtab" (getName <$> Map.keys sects)
      newHeader           = fileHeader
        { e_shstrndx = fromIntegral e_shstrtabndx }
  put (FixupEnv newHeader sects sectsNames segs)
 where
   getName SNull             = ""
   getName (SProgBits n _ _) = n
   getName (SNoBits n _ _)   = n
   getName (SStrTab n _)     = n

fixupHeadersOffsets :: ValueSet n => Fixup n ()
fixupHeadersOffsets = do
  FixupEnv fileHeader sects sectsNames segs <- get
  let phnum     = fromIntegral (e_phnum fileHeader)
      phentsize = fromIntegral (e_phentsize fileHeader)
      phoff     = fromIntegral (e_ehsize fileHeader)    -- we want to have program headers right after the file header
      shoff     = phoff + phnum * phentsize             -- and section headers right after program headers
  let newHeader = fileHeader
        { e_shoff = shoff
        , e_phoff = phoff }

  put (FixupEnv newHeader sects sectsNames segs)

fixupPHDREntry :: ValueSet n => Fixup n ()
fixupPHDREntry = do
  FixupEnv fileHeader sects sectsNames segs <- get

  let phoff    = fromIntegral (e_phoff fileHeader)
      phdrSize = fromIntegral (e_phnum fileHeader) * fromIntegral (e_phentsize fileHeader)
  let phdr = Map.lookup PPhdr segs <&>
        \ p -> p { p_offset = phoff
                 , p_filesz = phdrSize
                 , p_memsz = phdrSize }
  let phdrFileSize = phdrSize + fromIntegral phoff
  let phdrLoad = Map.lookup (PLoad (section "PHDR") pf_r) segs <&>
        \ p -> p { p_offset = 0x0
                 , p_filesz = phdrFileSize
                 , p_memsz = phdrFileSize }
  let newSegs = Map.update (const phdr) PPhdr $
                Map.update (const phdrLoad) (PLoad (section "PHDR") pf_r) $
                segs

  put (FixupEnv fileHeader sects sectsNames newSegs)

fixupSectionNames :: ValueSet n => Fixup n ()
fixupSectionNames = do
  FixupEnv fileHeader sects sectsNames segs <- get


  let newSects = sects

  put (FixupEnv fileHeader newSects sectsNames segs)

