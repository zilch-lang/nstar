{-# LANGUAGE RecordWildCards #-}

module Data.Elf.Internal.Symbol
( Elf_Sym(..)
  -- * 'st_info'
  -- ** Symbol binding
, stb_local, stb_global, stb_weak
  -- ** Symbol type
, stt_notype, stt_object, stt_func, stt_section
) where

import Data.Elf.Internal.BusSize (Size(..))
import Data.Elf.Types
import Data.Elf.Internal.Serialize (SerializableValueSet, Serializable(..))
import Foreign.Storable (Storable(..))

#include <elf.h>

-- | Symbol table entry.
data Elf_Sym (n :: Size)
  = Elf_Sym
  { st_name   :: Elf_Word n      -- ^ Symbol name (string table index)
  , st_info   :: Elf_UChar n     -- ^ Symbol type and binding
  , st_other  :: Elf_UChar n     -- ^ Symbol visibility
  , st_shndx  :: Elf_Section n   -- ^ Section index
  , st_value  :: Elf_Addr n      -- ^ Symbol value
  , st_size   :: Elf_Xword n     -- ^ Symbol size
  }

instance Storable (Elf_Sym S64) where
  sizeOf _ = {#sizeof Elf64_Sym#}
  alignment _ = {#alignof Elf64_Sym#}
  poke _ _ = undefined
  peek _ = undefined

instance SerializableValueSet S64 e => Serializable S64 e (Elf_Sym S64) where
  put e Elf_Sym{..} = do
    put @S64 e st_name
    put @S64 e st_info
    put @S64 e st_other
    put @S64 e st_shndx
    put @S64 e st_value
    put @S64 e st_size

-- Symbol binding type

-- | Local symbol
stb_local :: ValueSet n => Elf_UChar n
stb_local = {#const STB_LOCAL#}
-- | Global symbol
stb_global :: ValueSet n => Elf_UChar n
stb_global = {#const STB_GLOBAL#}
-- | Weak symbol
stb_weak :: ValueSet n => Elf_UChar n
stb_weak = {#const STB_WEAK#}


-- Symbol type

-- | Symbol type is unspecified
stt_notype :: ValueSet n => Elf_UChar n
stt_notype = {#const STT_NOTYPE#}
-- | Symbol is a data object
stt_object :: ValueSet n => Elf_UChar n
stt_object = {#const STT_OBJECT#}
-- | Symbol is a code object
stt_func :: ValueSet n => Elf_UChar n
stt_func = {#const STT_FUNC#}
-- | Symbol associated with a section
stt_section :: ValueSet n => Elf_UChar n
stt_section = {#const STT_SECTION#}
