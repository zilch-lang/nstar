{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Elf.Internal.Serialize
( ElfSerializable(..)
) where

import Data.Elf.Types
import GHC.TypeNats (Nat)
import qualified Data.Binary.Put as B

-- | A class of ELF serializable values depending on the endianness (lowE) and the data size (n).
--
--   n> @n@ should be equal to @32@ or @64@.
class ( Num (ElfUChar n lowE)
      , Num (ElfHalf n lowE)
      , Num (ElfWord n lowE)
      , Num (ElfSword n lowE)
      , Num (ElfXword n lowE)
      , Num (ElfSxword n lowE)
      , Num (ElfAddr n lowE)
      , Num (ElfOff n lowE)
      ) => ElfSerializable (n :: Nat) (lowE :: Bool) where
  -- | The type of unsigned characters
  type ElfUChar n lowE
  -- | The type of half words
  type ElfHalf n lowE
  -- | The type of words
  type ElfWord n lowE
  -- | The type of signed words
  type ElfSword n lowE
  -- | The type of double (large) words
  type ElfXword n lowE
  -- | The type of signed double words
  type ElfSxword n lowE
  -- | The type of addresses
  type ElfAddr n lowE
  -- | The type of offsets
  type ElfOff n lowE

  -- | Puts an unsigned char into the stream.
  putUChar  :: ElfUChar n lowE  -> B.Put
  -- | Puts a half word into the stream.
  putHalf   :: ElfHalf n lowE   -> B.Put
  -- | Puts a word into the stream.
  putWord   :: ElfWord n lowE   -> B.Put
  -- | Puts a signed word into the stream.
  putSword  :: ElfSword n lowE  -> B.Put
  -- | Puts a large word into the stream.
  putXword  :: ElfXword n lowE  -> B.Put
  -- | Puts a signed large word into the stream.
  putSxword :: ElfSxword n lowE -> B.Put
  -- | Puts an address into the stream.
  putAddr   :: ElfAddr n lowE   -> B.Put
  -- | Puts an offset into the stream.
  putOff    :: ElfOff n lowE    -> B.Put

-- | Serialize for little-endian 64-bits platforms
instance ElfSerializable 64 True where
  type ElfUChar 64 True = Elf64_UChar
  type ElfHalf 64 True = Elf64_Half
  type ElfSword 64 True = Elf64_Sword
  type ElfWord 64 True = Elf64_Word
  type ElfXword 64 True = Elf64_Xword
  type ElfSxword 64 True = Elf64_Sxword
  type ElfAddr 64 True = Elf64_Addr
  type ElfOff 64 True = Elf64_Off

  putUChar = B.putWord8
  putHalf = B.putWord16le
  putWord = B.putWord32le
  putSword = putWord @64 @True . fromIntegral    -- sign does not matter
  putXword = B.putWord64le
  putSxword = putXword @64 @True . fromIntegral  -- sign does not matter
  putAddr = putXword @64 @True
  putOff = putXword @64 @True
