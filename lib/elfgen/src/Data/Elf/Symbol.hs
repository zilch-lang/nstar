{-# LANGUAGE BangPatterns #-}

module Data.Elf.Symbol
( ElfSymbol(..)
, SymbolType(..)
, SymbolBinding(..)
, SymbolVisibility(..)
) where

import Data.Elf.Types

-- | Symbol table entry
data ElfSymbol
  = ElfSymbol
      String           -- ^ Symbol name
      SymbolType       -- ^ Symbol type
      SymbolBinding    -- ^ Symbol binding
      SymbolVisibility -- ^ Symbol visibility
  deriving (Eq, Ord)

-- | Symbol type
data SymbolType
  = ST_NoType          -- ^ Symbol type is unspecified
  | ST_Object          -- ^ Symbol is a data object
  | ST_Func Integer    -- ^ Symbol is a code object
  | ST_Section String  -- ^ Symbol associated with a section
  | ST_File String     -- ^ Symbol's name is file name
  | ST_Common          -- ^ Symbol is a common data object
  | ST_TLS             -- ^ Symbol is a thread-local data object
  deriving (Eq, Ord)

-- | Symbol binding type
data SymbolBinding
  = SB_Local       -- ^ Local symbol
  | SB_Global      -- ^ Global symbol
  | SB_Weak        -- ^ Weak symbol
  deriving (Eq, Ord)

-- | Symbol visibility specification
data SymbolVisibility
  = SV_Default         -- ^ Default symbol visibility rules
  | SV_Internal        -- ^ Processor specific hidden class
  | SV_Hidden          -- ^ Symbol unavailable in other modules
  | SV_Protected       -- ^ Not preemptible, not exported
  deriving (Eq, Ord)
