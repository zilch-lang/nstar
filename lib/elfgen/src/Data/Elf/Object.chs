{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Elf.Object where

import Data.Elf.Types
import Data.Elf.FileHeader (ElfHeader, C_ElfFileHeader, peekFileHeader, newFileHeader, freeFileHeader)
import Data.Elf.SectionHeader (SectionHeader, C_ElfSectionHeader, peekSectionHeader, newSectionHeader, freeSectionHeader)
import Data.Elf.ProgramHeader (ProgramHeader, C_ElfProgramHeader, peekProgramHeader, newProgramHeader, freeProgramHeader)
import Data.Elf.Internal.BusSize (Size)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.C.Types (CULong)
import Foreign.Storable (Storable(..))
import Foreign.Marshal.Array (peekArray, newArray)
import Foreign.Marshal.Alloc (malloc, free)

#include "object.h"

-- | An object file layout.
data ElfObject n
  = ElfObject
  { fileHeader    :: ElfHeader n           -- ^ The ELF header
  , segments      :: [ProgramHeader n]     -- ^ Program headers
  , sections      :: [SectionHeader n]     -- ^ Section headers
  }

data C_ElfObject (n :: Size)
  = C_ElfObject
      (Ptr (C_ElfFileHeader n))
      (Ptr (Ptr (C_ElfProgramHeader n)))
      (Ptr (Ptr (C_ElfSectionHeader n)))
      CULong
      CULong

instance Storable (C_ElfObject n) where
  sizeOf _ = {#sizeof elf_object#}
  alignment _ = {#alignof elf_object#}
  peek ptr =
    C_ElfObject <$> (castPtr <$> {#get struct elf_object->file_header#} ptr)
                <*> (castPtr <$> {#get struct elf_object->segments#} ptr)
                <*> (castPtr <$> {#get struct elf_object->sections#} ptr)
                <*> (fromIntegral <$> {#get struct elf_object->segments_len#} ptr)
                <*> (fromIntegral <$> {#get struct elf_object->sections_len#} ptr)
  poke ptr (C_ElfObject fh ps ss pl sl) = do
    {#set struct elf_object->file_header#} ptr (castPtr fh)
    {#set struct elf_object->segments#} ptr (castPtr ps)
    {#set struct elf_object->sections#} ptr (castPtr ss)
    {#set struct elf_object->segments_len#} ptr (fromIntegral pl)
    {#set struct elf_object->sections_len#} ptr (fromIntegral sl)

peekObject :: Ptr (C_ElfObject n) -> IO (ElfObject n)
peekObject ptr = do
  C_ElfObject fh phs shs pl sl <- peek ptr
  fileHeader <- peekFileHeader fh
  programHeaders <- traverse peekProgramHeader =<< peekArray (fromIntegral pl) phs
  sectionHeaders <- traverse peekSectionHeader =<< peekArray (fromIntegral sl) shs
  pure $ ElfObject fileHeader programHeaders sectionHeaders

newObject :: forall (n :: Size). ElfObject n -> IO (Ptr (C_ElfObject n))
newObject ElfObject{..} = do
  let segmentsLen = fromIntegral $ length segments
      sectionsLen = fromIntegral $ length sections

  ptr <- malloc @(C_ElfObject n)

  fh <- newFileHeader fileHeader
  phs <- newArray =<< traverse newProgramHeader segments
  shs <- newArray =<< traverse newSectionHeader sections

  poke ptr $ C_ElfObject fh phs shs segmentsLen sectionsLen

  pure ptr

freeObject :: Ptr (C_ElfObject n) -> IO ()
freeObject ptr = do
  C_ElfObject fh phs shs pl sl <- peek ptr

  freeFileHeader fh
  mapM_ freeProgramHeader =<< peekArray (fromIntegral pl) phs
  free phs
  mapM_ freeSectionHeader =<< peekArray (fromIntegral sl) shs
  free shs

  free ptr
