{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Elf.Types
( -- * ELF types
  Elf_Half, Elf_Word, Elf_Sword, Elf_Xword, Elf_Sxword, Elf_Addr(..), Elf_Off(..), Elf_UChar, Elf_Section, Elf_Rel_Info(..), Elf_Rel_Addend(..)
  -- * Internal export
, ReifySize
) where

import Data.Word (Word8, Word16, Word32, Word64)
import Data.Int (Int32, Int64)
import GHC.TypeNats (Nat)
import qualified Data.Binary.Put as B
import Data.Bool (bool)
import Data.Elf.Internal.BusSize (Size(..))
import Data.Bits (Bits)
import Data.Some
import Unsafe.Coerce (unsafeCoerce)


type IsInteger n = (Num n, Integral n, Real n, Enum n, Bits n, Eq n, Ord n, Show n)


-- | Unsigned 8-bits integer
type Elf_UChar (n :: Size) = Word8
-- | Unsigned 16-bits integer
type Elf_Half (n :: Size) = Word16
-- | Unsigned 32-bits integer
type Elf_Word (n :: Size) = Word32
-- | Signed 32-bits integer
type Elf_Sword (n :: Size) = Int32
-- | Unsigned 64-bits integer
type Elf_Xword (n :: Size) = Word64
-- | Signed 64-bits integer
type Elf_Sxword (n :: Size) = Int64
-- | [32 bits] Unsigned 32-bits integer
--   [64 bits] Unsigned 64-bits integer
data Elf_Addr (n :: Size) where
  Elf32_Addr :: Word32 -> Elf_Addr S32
  Elf64_Addr :: Word64 -> Elf_Addr S64
-- | [32 bits] Unsigned 32-bits integer
--   [64 bits] Unsigned 64-bits integer
data Elf_Off (n :: Size) where
  Elf32_Off :: Word32 -> Elf_Off S32
  Elf64_Off :: Word64 -> Elf_Off S64
-- | Unsigned 16-bits integer
type Elf_Section (n :: Size) = Word16
-- | [32 bits] Unsigned 32-bits integer
--   [64 bits] Unsigned 64-bits integer
data Elf_Rel_Info (n :: Size) where
  Elf32_Rel_Info :: Word32 -> Elf_Rel_Info S32
  Elf64_Rel_Info :: Word64 -> Elf_Rel_Info S64
-- | [32 bits] Signed 32-bits integer
--   [64 bits] Signed 64-bits integer
data Elf_Rel_Addend (n :: Size) where
  Elf32_Rel_Addend :: Word32 -> Elf_Rel_Addend S32
  Elf64_Rel_Addend :: Word64 -> Elf_Rel_Addend S64


class ReifySize (n :: Size) where
  reifySize :: Size
instance ReifySize S32 where
  reifySize = S32
instance ReifySize S64 where
  reifySize = S64

instance ReifySize n => Num (Elf_Addr n) where
  Elf32_Addr a1 + Elf32_Addr a2 = Elf32_Addr (a1 + a2)
  Elf64_Addr a1 + Elf64_Addr a2 = Elf64_Addr (a1 + a2)

  Elf32_Addr a1 * Elf32_Addr a2 = Elf32_Addr (a1 * a2)
  Elf64_Addr a1 * Elf64_Addr a2 = Elf64_Addr (a1 * a2)

  -- NOTE: we don't use those at all, and it makes no sense.
  abs _ = undefined
  signum _ = undefined

  fromInteger i =
    let Some (unsafeCoerce -> addr :: Elf_Addr n) = case reifySize @n of
          S32 -> Some $ Elf32_Addr (fromInteger i)
          S64 -> Some $ Elf64_Addr (fromInteger i)
    in addr

  negate (Elf32_Addr a) = Elf32_Addr (negate a)
  negate (Elf64_Addr a) = Elf64_Addr (negate a)


instance ReifySize n => Real (Elf_Addr n) where
  toRational (Elf32_Addr a) = toRational a
  toRational (Elf64_Addr a) = toRational a

instance ReifySize n => Enum (Elf_Addr n) where
  toEnum i =
    let Some (unsafeCoerce -> addr :: Elf_Addr n) = case reifySize @n of
          S32 -> Some $ Elf32_Addr (toEnum i)
          S64 -> Some $ Elf64_Addr (toEnum i)
    in addr

  fromEnum (Elf32_Addr a) = fromEnum a
  fromEnum (Elf64_Addr a) = fromEnum a

instance ReifySize n => Integral (Elf_Addr n) where
  quotRem (Elf32_Addr a1) (Elf32_Addr a2) =
    let (x, y) = quotRem a1 a2 in (Elf32_Addr x, Elf32_Addr y)
  quotRem (Elf64_Addr a1) (Elf64_Addr a2) =
    let (x, y) = quotRem a1 a2 in (Elf64_Addr x, Elf64_Addr y)

  toInteger (Elf32_Addr a) = toInteger a
  toInteger (Elf64_Addr a) = toInteger a

deriving instance Ord (Elf_Addr n)
deriving instance Eq (Elf_Addr n)
deriving instance Show (Elf_Addr n)

instance ReifySize n => Num (Elf_Off n) where
  Elf32_Off a1 + Elf32_Off a2 = Elf32_Off (a1 + a2)
  Elf64_Off a1 + Elf64_Off a2 = Elf64_Off (a1 + a2)

  Elf32_Off a1 * Elf32_Off a2 = Elf32_Off (a1 * a2)
  Elf64_Off a1 * Elf64_Off a2 = Elf64_Off (a1 * a2)

  -- NOTE: we don't use those at all, and it makes no sense.
  abs _ = undefined
  signum _ = undefined

  fromInteger i =
    let Some (unsafeCoerce -> addr :: Elf_Off n) = case reifySize @n of
          S32 -> Some $ Elf32_Off (fromInteger i)
          S64 -> Some $ Elf64_Off (fromInteger i)
    in addr

  negate (Elf32_Off a) = Elf32_Off (negate a)
  negate (Elf64_Off a) = Elf64_Off (negate a)

instance ReifySize n => Real (Elf_Off n) where
  toRational (Elf32_Off a) = toRational a
  toRational (Elf64_Off a) = toRational a

instance ReifySize n => Enum (Elf_Off n) where
  toEnum i =
    let Some (unsafeCoerce -> addr :: Elf_Off n) = case reifySize @n of
          S32 -> Some $ Elf32_Off (toEnum i)
          S64 -> Some $ Elf64_Off (toEnum i)
    in addr

  fromEnum (Elf32_Off a) = fromEnum a
  fromEnum (Elf64_Off a) = fromEnum a

instance ReifySize n => Integral (Elf_Off n) where
  quotRem (Elf32_Off a1) (Elf32_Off a2) =
    let (x, y) = quotRem a1 a2 in (Elf32_Off x, Elf32_Off y)
  quotRem (Elf64_Off a1) (Elf64_Off a2) =
    let (x, y) = quotRem a1 a2 in (Elf64_Off x, Elf64_Off y)

  toInteger (Elf32_Off a) = toInteger a
  toInteger (Elf64_Off a) = toInteger a

deriving instance Ord (Elf_Off n)
deriving instance Eq (Elf_Off n)
deriving instance Show (Elf_Off n)



instance ReifySize n => Num (Elf_Rel_Info n) where
  Elf32_Rel_Info i1 + Elf32_Rel_Info i2 = Elf32_Rel_Info (i1 + i2)
  Elf64_Rel_Info i1 + Elf64_Rel_Info i2 = Elf64_Rel_Info (i1 + i2)

  Elf32_Rel_Info i1 * Elf32_Rel_Info i2 = Elf32_Rel_Info (i1 * i2)
  Elf64_Rel_Info i1 * Elf64_Rel_Info i2 = Elf64_Rel_Info (i1 * i2)

  -- NOTE: we don't use those at all, and it makes no sense.
  abs _ = undefined
  signum _ = undefined

  fromInteger i =
    let Some (unsafeCoerce -> info :: Elf_Rel_Info n) = case reifySize @n of
          S32 -> Some $ Elf32_Rel_Info (fromInteger i)
          S64 -> Some $ Elf64_Rel_Info (fromInteger i)
    in info

  negate (Elf32_Rel_Info i) = Elf32_Rel_Info (negate i)
  negate (Elf64_Rel_Info i) = Elf64_Rel_Info (negate i)

instance ReifySize n => Real (Elf_Rel_Info n) where
  toRational (Elf32_Rel_Info a) = toRational a
  toRational (Elf64_Rel_Info a) = toRational a

instance ReifySize n => Enum (Elf_Rel_Info n) where
  toEnum i =
    let Some (unsafeCoerce -> addr :: Elf_Rel_Info n) = case reifySize @n of
          S32 -> Some $ Elf32_Rel_Info (toEnum i)
          S64 -> Some $ Elf64_Rel_Info (toEnum i)
    in addr

  fromEnum (Elf32_Rel_Info a) = fromEnum a
  fromEnum (Elf64_Rel_Info a) = fromEnum a

instance ReifySize n => Integral (Elf_Rel_Info n) where
  quotRem (Elf32_Rel_Info a1) (Elf32_Rel_Info a2) =
    let (x, y) = quotRem a1 a2 in (Elf32_Rel_Info x, Elf32_Rel_Info y)
  quotRem (Elf64_Rel_Info a1) (Elf64_Rel_Info a2) =
    let (x, y) = quotRem a1 a2 in (Elf64_Rel_Info x, Elf64_Rel_Info y)

  toInteger (Elf32_Rel_Info a) = toInteger a
  toInteger (Elf64_Rel_Info a) = toInteger a

deriving instance Ord (Elf_Rel_Info n)
deriving instance Eq (Elf_Rel_Info n)
deriving instance Show (Elf_Rel_Info n)


instance ReifySize n => Num (Elf_Rel_Addend n) where
  Elf32_Rel_Addend a1 + Elf32_Rel_Addend a2 = Elf32_Rel_Addend (a1 + a2)
  Elf64_Rel_Addend a1 + Elf64_Rel_Addend a2 = Elf64_Rel_Addend (a1 + a2)

  Elf32_Rel_Addend a1 * Elf32_Rel_Addend a2 = Elf32_Rel_Addend (a1 * a2)
  Elf64_Rel_Addend a1 * Elf64_Rel_Addend a2 = Elf64_Rel_Addend (a1 * a2)

  -- NOTE: we don't use those at all, and it makes no sense.
  abs _ = undefined
  signum _ = undefined

  fromInteger i =
    let Some (unsafeCoerce -> addend :: Elf_Rel_Addend n) = case reifySize @n of
          S32 -> Some $ Elf32_Rel_Addend (fromInteger i)
          S64 -> Some $ Elf64_Rel_Addend (fromInteger i)
    in addend

  negate (Elf32_Rel_Addend a) = Elf32_Rel_Addend (negate a)
  negate (Elf64_Rel_Addend a) = Elf64_Rel_Addend (negate a)

instance ReifySize n => Real (Elf_Rel_Addend n) where
  toRational (Elf32_Rel_Addend a) = toRational a
  toRational (Elf64_Rel_Addend a) = toRational a

instance ReifySize n => Enum (Elf_Rel_Addend n) where
  toEnum i =
    let Some (unsafeCoerce -> addr :: Elf_Rel_Addend n) = case reifySize @n of
          S32 -> Some $ Elf32_Rel_Addend (toEnum i)
          S64 -> Some $ Elf64_Rel_Addend (toEnum i)
    in addr

  fromEnum (Elf32_Rel_Addend a) = fromEnum a
  fromEnum (Elf64_Rel_Addend a) = fromEnum a

instance ReifySize n => Integral (Elf_Rel_Addend n) where
  quotRem (Elf32_Rel_Addend a1) (Elf32_Rel_Addend a2) =
    let (x, y) = quotRem a1 a2 in (Elf32_Rel_Addend x, Elf32_Rel_Addend y)
  quotRem (Elf64_Rel_Addend a1) (Elf64_Rel_Addend a2) =
    let (x, y) = quotRem a1 a2 in (Elf64_Rel_Addend x, Elf64_Rel_Addend y)

  toInteger (Elf32_Rel_Addend a) = toInteger a
  toInteger (Elf64_Rel_Addend a) = toInteger a

deriving instance Ord (Elf_Rel_Addend n)
deriving instance Eq (Elf_Rel_Addend n)
deriving instance Show (Elf_Rel_Addend n)
