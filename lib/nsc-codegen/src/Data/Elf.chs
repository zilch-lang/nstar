{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}

{-|

  For a complete documentation, please see the one in the <https://code.woboq.org/userspace/glibc/elf/elf.h.html elf.h> header on linux.
-}
module Data.Elf
( -- * Types
  module Data.Elf.Types
  -- * File header
, module Data.Elf.FileHeader
  -- * Section header
, module Data.Elf.SectionHeader
  -- * Program header
, module Data.Elf.ProgramHeader


  -- * ELF structure
, Object(..)
) where

#include <elf.h>

-- For now, let's just focus on the 64 bits format, no matter what the HOST/TARGET arch is.

import Data.Elf.Types
import Data.Elf.FileHeader
import Data.Elf.SectionHeader
import Data.Elf.ProgramHeader
import Data.ByteString (ByteString)


{- $e_ident

   The 'e_ident' field contains the ELF magic number and some other info laid out as:

   - Byte number 0: @0x7f@
   - Bytes number 1-3: @\'E\', \'L\', \'F\'@
   - Byte number 4: ELF class (32-bit or 64-bit object)
   - Byte number 5: Data encoding (little endian or big endian)
   - Byte number 6: File version (must always be @'ev_current'@)
   - Byte number 7: OS ABI identification (one of 'elfosabi_none', 'elfosabi_sysv')
   - Byte number 8: ABI version
   - Bytes number 9-15: @0@-ed padding bytes
-}

-------------------------------------------------------------

-- | An object file layout.
data Object
  = Object
  { o_header    :: Elf64_Ehdr   -- ^ The ELF header
  , p_table     :: Elf64_Phdr   -- ^ The program header table
  , s_table     :: Elf64_Shdr   -- ^ The section header table
  , bytes       :: ![UChar]     -- ^ Data stored in the object file
  }
