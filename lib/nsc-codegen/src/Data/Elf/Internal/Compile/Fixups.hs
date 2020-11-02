{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MagicHash #-}

module Data.Elf.Internal.Compile.Fixups
( FixupEnvironment(..), Fixup
, Section64AList, Segment64AList, SectionByName
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
import GHC.Base (Int (..), Int#, (+#))
import Data.Functor ((<&>))

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
  fixupShstrtabIndex
  fixupHeadersOffsets
  fixupPHDREntry

-- | A fix for headers count in the ELF file header (fields 'e_phnum' and 'e_shnum').
fixupHeaderCount :: Fixup ()
fixupHeaderCount = do
  FixupEnv fileHeader sects sectsNames segs <- get
  let newHeader = fileHeader
        { e_phnum = fromIntegral (Map.size segs)
        , e_shnum = fromIntegral (Map.size sects) }
  put (FixupEnv newHeader sects sectsNames segs)

fixupShstrtabIndex :: Fixup ()
fixupShstrtabIndex = do
  FixupEnv fileHeader sects sectsNames segs <- get
  let sectionsWithIndexes = indexed (Map.keys sects)
      e_shstrtabndx:_     = fst <$> filter (\ (i, s) -> getName s == Just ".shstrtab") sectionsWithIndexes
      newHeader           = fileHeader
        { e_shstrndx = fromIntegral e_shstrtabndx }
  put (FixupEnv newHeader sects sectsNames segs)
 where
   getName SNull             = Nothing
   getName (SProgBits n _ _) = Just n
   getName (SNoBits n _ _)   = Just n
   getName (SStrTab n _)     = Just n

fixupHeadersOffsets :: Fixup ()
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

fixupPHDREntry :: Fixup ()
fixupPHDREntry = do
  FixupEnv fileHeader sects sectsNames segs <- get

  let phoff    = e_phoff fileHeader
      phdrSize = fromIntegral (e_phnum fileHeader) * fromIntegral (e_phentsize fileHeader)
  let phdr = Map.lookup PPhdr segs <&>
        \ p -> p { p_offset = phoff
                 , p_filesz = phdrSize
                 , p_memsz = phdrSize }
  let phdrLoad = Map.lookup (PLoad (section "PHDR") pf_r) segs <&>
        \ p -> p { p_offset = 0x0
                 , p_filesz = phdrSize + phoff
                 , p_memsz = phdrSize + phoff }
  let newSegs = Map.update (const phdr) PPhdr $
                Map.update (const phdrLoad) (PLoad (section "PHDR") pf_r) $
                segs

  put (FixupEnv fileHeader sects sectsNames newSegs)

{- |
'indexed' pairs each element with its index.

>>> indexed "hello"
[(0,'h'),(1,'e'),(2,'l'),(3,'l'),(4,'o')]

/Subject to fusion./
-}
indexed :: [a] -> [(Int, a)]
indexed xs = go 0# xs
  where
    go i (a:as) = (I# i, a) : go (i +# 1#) as
    go _ _      = []
{-# NOINLINE [1] indexed #-}
