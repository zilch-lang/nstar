{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.Elf.Internal.Serialize
( Serializable(..)
) where

import Data.Elf.Types
import GHC.TypeNats (Nat)
import Data.Kind (Type)
import qualified Data.Binary.Put as B
import Data.Word (Word8, Word16, Word32, Word64)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Elf.Internal.BusSize (Size(..))


data Endianness
  = LE     -- ^ little endian
  | BE     -- ^ big endian



-- | A class of serializable values depending on the endianness (le) and the data size (n).
--
--   n> @n@ should be equal to @32@ or @64@.
class Serializable (n :: Size) (le :: Endianness) (a :: Type) where
  put :: a -> B.Put

instance Serializable S32 e Word8 where
  put = B.putWord8

instance Serializable S64 e Word8 where
  put = B.putWord8

instance Serializable n e Word8 => Serializable n e Int8 where
  put = put @n @e @Word8 . fromIntegral

instance Serializable S64 LE Word16 where
  put = B.putWord16le

instance Serializable S64 BE Word16 where
  put = B.putWord16be

instance Serializable n e Word16 => Serializable n e Int16 where
  put = put @n @e @Word16 . fromIntegral

instance Serializable S64 LE Word32 where
  put = B.putWord32le

instance Serializable S64 BE Word32 where
  put = B.putWord32be

instance Serializable n e Word32 => Serializable n e Int32 where
  put = put @n @e @Word32 . fromIntegral

instance Serializable S64 LE Word64 where
  put = B.putWord64le

instance Serializable S64 BE Word64 where
  put = B.putWord64be

instance Serializable n e Word64 => Serializable n e Int64 where
  put = put @n @e @Word64 . fromIntegral
