{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

-- | A class of serializable values depending on the endianness (le) and the data size (n).
--
--   n> @n@ should be equal to @32@ or @64@.
class Serializable (n :: Nat) (le :: Bool) (a :: Type) where
  put :: a -> B.Put

instance Serializable 64 b Word8 where
  put = B.putWord8

instance Serializable 64 b Int8 where
  put = put @64 @b @Word8 . fromIntegral

instance Serializable 64 True Word16 where
  put = B.putWord16le

instance Serializable 64 True Int16 where
  put = put @64 @True @Word16 . fromIntegral

instance Serializable 64 True Word32 where
  put = B.putWord32le

instance Serializable 64 True Int32 where
  put = put @64 @True @Word32 . fromIntegral

instance Serializable 64 True Word64 where
  put = B.putWord64le

instance Serializable 64 True Int64 where
  put = put @64 @True @Word64 . fromIntegral
