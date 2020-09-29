{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Console.NStar.Flags.Internal where

import Data.Data (Data)
import Data.Typeable (Typeable)
import Data.Map (Map)
import qualified Data.Map as Map

data CompilerFlags
  = CFlags
  { files :: [FilePath]
  , flags :: Map String String
  }
 deriving (Show)

lookupFlag :: String -> CompilerFlags -> Maybe String
lookupFlag name CFlags{flags} = Map.lookup name flags


data Flags
  = Flags
  { files  :: [FilePath]      -- ^ The path to all the files to compile.
  , flags  :: [String]        -- ^ A set of compiler flags.
  }
 deriving (Data, Typeable, Show)
