{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module Data.Elf.Types
( -- * ELF types
  Elf_Half, Elf_Word, Elf_Sword, Elf_Xword, Elf_Sxword, Elf_Addr, Elf_Off, Elf_UChar
  -- * Set of values depending on the bus size
, ValueSet
) where

import Data.Word (Word8, Word16, Word32, Word64)
import Data.Int (Int32, Int64)
import GHC.TypeNats (Nat)
import qualified Data.Binary.Put as B
import Data.Bool (bool)
import Data.Elf.Internal.BusSize (Size(..))
import Data.Bits (Bits)


type IsInteger n = (Num n, Integral n, Real n, Enum n, Bits n, Eq n, Ord n, Show n)

type ValueSet n =
  ( IsInteger (Elf_UChar n)
  , IsInteger (Elf_Half n)
  , IsInteger (Elf_Word n)
  , IsInteger (Elf_Sword n)
  , IsInteger (Elf_Xword n)
  , IsInteger (Elf_Sxword n)
  , IsInteger (Elf_Addr n)
  , IsInteger (Elf_Off n)
  )

-- | Unsigned 8-bits integer
type family Elf_UChar (n :: Size)
-- | Unsigned 16-bits integer
type family Elf_Half (n :: Size)
-- | Unsigned 32-bits integer
type family Elf_Word (n :: Size)
-- | Signed 32-bits integer
type family Elf_Sword (n :: Size)
-- | [64 bits] Unsigned 64-bits integer
--   [32 bits] 64-bits wide integers do not exist, so we just alias on 'Elf_Word'
type family Elf_Xword (n :: Size)
-- | [64 bits] Signed 64-bits integer
--   [32 bits] 64-bits wide integers do not exist, so we just alias on 'Elf_Sword'
type family Elf_Sxword (n :: Size)
-- | [32 bits] Unsigned 32-bits integer
--   [64 bits] Unsigned 64-bits integer
type family Elf_Addr (n :: Size)
-- | [32 bits] Unsigned 32-bits integer
--   [64 bits] Unsigned 64-bits integer
type family Elf_Off (n :: Size)

type instance Elf_UChar S64 = Word8
type instance Elf_Half S64 = Word16
type instance Elf_Word S64 = Word32
type instance Elf_Sword S64 = Int32
type instance Elf_Xword S64 = Word64
type instance Elf_Sxword S64 = Int64
type instance Elf_Addr S64 = Word64
type instance Elf_Off S64 = Word64
