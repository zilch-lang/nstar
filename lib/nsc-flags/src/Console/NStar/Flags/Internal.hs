{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Console.NStar.Flags.Internal
( Flags(..), CompilerFlags(..)
) where

import Data.Data (Data)
import Data.Typeable (Typeable)
import Data.Map (Map)

data CompilerFlags
  = CFlags
  { files :: [FilePath]
  , flags :: Map String String
  }
 deriving (Show)



data Flags
  = Flags
  { files  :: [FilePath]      -- ^ The path to all the files to compile.
  , flags  :: [String]        -- ^ A set of compiler flags.
  }
 deriving (Data, Typeable, Show)
