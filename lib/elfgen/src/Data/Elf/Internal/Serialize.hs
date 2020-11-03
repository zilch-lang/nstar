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
  -- * Re-export
, B.runPut
) where

import Data.Elf.Types
import GHC.TypeNats (Nat)
import Data.Kind (Type)
import qualified Data.Binary.Put as B
import Data.Word (Word8, Word16, Word32, Word64)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Elf.Internal.BusSize (Size(..))
import Data.Elf.Internal.Endianness (Endianness(..))

-- | A class of serializable values depending on the endianness (le) and the data size (n).
--
--   n> @n@ should be equal to @32@ or @64@.
class Serializable (n :: Size) (le :: Endianness) (a :: Type) where
  put :: a -> B.Put

instance Serializable n e Word8 where
  put = B.putWord8

instance Serializable n e Int8 where
  put = B.putInt8

instance Serializable n LE Word16 where
  put = B.putWord16le

instance Serializable n BE Word16 where
  put = B.putWord16be

instance Serializable n LE Int16 where
  put = B.putInt16le

instance Serializable n BE Int16 where
  put = B.putInt16be

instance Serializable n LE Word32 where
  put = B.putWord32le

instance Serializable n BE Word32 where
  put = B.putWord32be

instance Serializable n LE Int32 where
  put = B.putInt32le

instance Serializable n BE Int32 where
  put = B.putInt32be

instance Serializable n LE Word64 where
  put = B.putWord64le

instance Serializable n BE Word64 where
  put = B.putWord64be

instance Serializable n LE Int64 where
  put = B.putInt64le

instance Serializable n BE Int64 where
  put = B.putInt64be

instance Serializable n e a => Serializable n e [a] where
  put = mapM_ (put @n @e)
    -- A little tradeoff happening here: most serializers put the size of the list in front of the list.
    -- While this works, this is not what I want because the ELF format does not do that.
    -- That's why we simply output everything concatenated.
    --
    -- NOTE: endianness does not do much regarding the order the list is serialized.
