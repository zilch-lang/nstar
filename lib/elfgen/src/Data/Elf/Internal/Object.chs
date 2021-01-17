{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Elf.Internal.Object where

import Data.Elf.Internal.FileHeader
import Data.Elf.Internal.ProgramHeader
import Data.Elf.Internal.SectionHeader
import Data.Elf.Internal.Symbol
import Data.Word (Word8)
import GHC.TypeNats (Nat)
import Data.Elf.Internal.BusSize (Size(..))
import Data.Elf.Internal.Serialize (Serializable(..))
import Foreign.Ptr (Ptr, castPtr)
import Foreign.C.Types (CUChar, CULong)
import Foreign.Storable (Storable(..))
import Foreign.Marshal.Array (peekArray)
import Debug.Trace (trace, traceShow)

#include "object.h"

data Object (n :: Size)
  = Obj
      (Elf_Ehdr n)   -- ^ The file header
      [Elf_Phdr n]   -- ^ Programs headers
      [Elf_Shdr n]   -- ^ Section headers
      [Elf_Sym n]    -- ^ Symbols
      [Elf_Rela n]   -- ^ Relocations
      [Word8]        -- ^ Raw data

instance ( Serializable n e (Elf_Ehdr n)
         , Serializable n e (Elf_Phdr n)
         , Serializable n e (Elf_Shdr n)
         , Serializable n e (Elf_Sym n)
         , Serializable n e (Elf_Rela n)
         ) => Serializable n e (Object n) where
  put e (Obj fileHeader programHeaders sectionHeaders symbols relocs binaryData) = do
    put @n e fileHeader
    put @n e programHeaders
    put @n e sectionHeaders
    put @n e relocs
    put @n e binaryData
    put @n e symbols

data C_Object (n :: Size)
  = C_Obj
      (Ptr (Elf_Ehdr n))
      (Ptr (Ptr (Elf_Phdr n)))
      (Ptr (Ptr (Elf_Shdr n)))
      (Ptr (Ptr (Elf_Rela n)))
      (Ptr (Ptr (Elf_Sym n)))
      (Ptr CUChar)
      CULong
      CULong
      CULong
      CULong
      CULong

instance Storable (C_Object S64) where
  sizeOf _ = {#sizeof Elf64_Object#}
  alignment _ = {#alignof Elf64_Object#}
  peek ptr = do
    C_Obj <$> (castPtr <$> {#get struct Elf64_Object->file_header#} ptr)
          <*> (castPtr <$> {#get struct Elf64_Object->segment_headers#} ptr)
          <*> (castPtr <$> {#get struct Elf64_Object->section_headers#} ptr)
          <*> (castPtr <$> {#get struct Elf64_Object->relocations#} ptr)
          <*> (castPtr <$> {#get struct Elf64_Object->symbols#} ptr)
          <*> (castPtr <$> {#get struct Elf64_Object->binary_data#} ptr)
          <*> {#get struct Elf64_Object->segments_len#} ptr
          <*> {#get struct Elf64_Object->sections_len#} ptr
          <*> {#get struct Elf64_Object->symbols_len#} ptr
          <*> {#get struct Elf64_Object->relocations_len#} ptr
          <*> {#get struct Elf64_Object->binary_data_len#} ptr
  poke ptr (C_Obj fh phs shs rels syms d pl sl syml relsl dl) = do
    {#set struct Elf64_Object->file_header#} ptr (castPtr fh)
    {#set struct Elf64_Object->segment_headers#} ptr (castPtr phs)
    {#set struct Elf64_Object->section_headers#} ptr (castPtr shs)
    {#set struct Elf64_Object->relocations#} ptr (castPtr rels)
    {#set struct Elf64_Object->symbols#} ptr (castPtr syms)
    {#set struct Elf64_Object->binary_data#} ptr (castPtr d)
    {#set struct Elf64_Object->segments_len#} ptr pl
    {#set struct Elf64_Object->sections_len#} ptr sl
    {#set struct Elf64_Object->symbols_len#} ptr syml
    {#set struct Elf64_Object->relocations_len#} ptr relsl
    {#set struct Elf64_Object->binary_data_len#} ptr dl

peekObject :: ( Storable (C_Object n)
              , Storable (Elf_Ehdr n)
              , Storable (Elf_Shdr n)
              , Storable (Elf_Phdr n)
              , Storable (Elf_Sym n)
              , Storable (Elf_Rela n) ) => Ptr (C_Object n) -> IO (Object n)
peekObject ptr = do
  C_Obj fh phs shs rels syms d pl sl syml relsl dl <- peek ptr
  fileHeader <- peek fh
  segmentHeaders <- traverse peek =<< peekArray (fromIntegral pl) phs
  sectionHeaders <- traverse peek =<< peekArray (fromIntegral sl) shs
  relocations <- traceShow relsl $ traverse peek =<< peekArray (fromIntegral relsl) rels
  symbols <- traverse peek =<< peekArray (fromIntegral syml) syms
  binData <- peekArray (fromIntegral dl) (castPtr d :: Ptr Word8)
  pure (Obj fileHeader segmentHeaders sectionHeaders symbols relocations binData)
