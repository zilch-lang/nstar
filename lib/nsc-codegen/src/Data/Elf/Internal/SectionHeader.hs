{-# LANGUAGE BangPatterns #-}

module Data.Elf.Internal.SectionHeader where

import Data.Elf.Types

-- | Section header
data Elf64_Shdr
  = Elf64_Shdr
  { sh_name        :: !Elf64_Word    -- ^ Section name (string table index)
  , sh_type        :: !Elf64_Word    -- ^ Section type
  , sh_flags       :: !Elf64_Xword   -- ^ Section flags
  , sh_addr        :: !Elf64_Addr    -- ^ Section virtual address at execution
  , sh_offset      :: !Elf64_Off     -- ^ Section file offset
  , sh_size        :: !Elf64_Xword   -- ^ Section size in bytes
  , sh_link        :: !Elf64_Word    -- ^ Link to another section
  , sh_info        :: !Elf64_Word    -- ^ Additional section information
  , sh_addralign   :: !Elf64_Xword   -- ^ Section alignment
  , sh_entsize     :: !Elf64_Xword   -- ^ Entry size if section holds table
  }
