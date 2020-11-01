{-# LANGUAGE RecordWildCards #-}

module Data.Elf.Internal.Compile.Fixups where

import Data.Elf.SectionHeader
import Data.Elf.Internal.SectionHeader
import Data.Elf.ProgramHeader
import Data.Elf.Internal.ProgramHeader
import Data.Elf.Internal.FileHeader
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import Control.Monad.State (State, get, put, execState)

-- | Associative list between abstract and concrete section header structures.
type Section64AList = Map SectionHeader Elf64_Shdr
-- | Associative list between abstract and concrete program header structures.
type Segment64AList = Map ProgramHeader Elf64_Phdr
-- | Mapping from section names to abstract section headers.
type SectionByName = Map Text SectionHeader

-- | The fixup environment, containing all sections to be fixed up.
data FixupEnvironment
  = FixupEnv
      Elf64_Ehdr       -- ^ The ELF file header
      Section64AList   -- ^ An association list associating abstract and concrete section headers
      SectionByName    -- ^ All segments mapped by their respective names
      Segment64AList   -- ^ An association from abstract to concrete segment headers

type Fixup a = State FixupEnvironment a

-- | Run a given fixup step (or multiple) with the given initial environment.
runFixup :: Fixup a -> FixupEnvironment -> FixupEnvironment
runFixup = execState

-- | Contains all fixes to do.
allFixes :: Fixup ()
allFixes = do
  fixupHeaderCount

-- | A fix for headers count in the ELF file header (fields 'e_phnum' and 'e_shnum').
fixupHeaderCount :: Fixup ()
fixupHeaderCount = do
  FixupEnv fileHeader sects sectsNames segs <- get
  let newHeader = fileHeader
        { e_phnum = fromIntegral (Map.size segs)
        , e_shnum = fromIntegral (Map.size sects) }
  put (FixupEnv newHeader sects sectsNames segs)
