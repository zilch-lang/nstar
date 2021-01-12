{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}

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
import Data.Elf.Internal.Endianness as E (Endianness(..), Order)

-- | A class of serializable values depending on the endianness (le) and the data size (n).
--
--   n> @n@ should be equal to @32@ or @64@.
class Serializable (n :: Size) (le :: E.Order) (a :: Type) where
  put :: Endianness le -> a -> B.Put

instance Serializable n e Word8 where
  put _ = B.putWord8

instance Serializable n e Int8 where
  put _ = B.putInt8

instance Serializable n e Word16 where
  put LE = B.putWord16le
  put BE = B.putWord16be

instance Serializable n e Int16 where
  put LE = B.putInt16le
  put BE = B.putInt16be

instance Serializable n e Word32 where
  put LE = B.putWord32le
  put BE = B.putWord32be

instance Serializable n e Int32 where
  put LE = B.putInt32le
  put BE = B.putInt32be

instance Serializable n e Word64 where
  put LE = B.putWord64le
  put BE = B.putWord64be

instance Serializable n e Int64 where
  put LE = B.putInt64le
  put BE = B.putInt64be

instance Serializable n e a => Serializable n e [a] where
  put le = mapM_ (put @n @e le)
    -- A little tradeoff happening here: most serializers put the size of the list in front of the list.
    -- While this works, this is not what I want because the ELF format does not do that.
    -- That's why we simply output everything concatenated.
    --
    -- NOTE: endianness does not do much regarding the order the list is serialized.

instance Serializable n1 e (Elf_Addr n) where
  put le (Elf32_Addr a) = put le a
  put le (Elf64_Addr a) = put le a

instance Serializable n1 e (Elf_Off n) where
  put le (Elf32_Off o) = put le o
  put le (Elf64_Off o) = put le o
