module Console.NStar.Flags.Internal where

data Flags
  = Flags
  { files         :: [FilePath]
  , output        :: FilePath
  , configuration :: ConfigurationFlags
  , debugging     :: DebugFlags
  }
 deriving (Show)

data ConfigurationFlags
  = ConfigFlags
  { diagnostic_color :: Bool
  }
  deriving (Show)

instance Semigroup ConfigurationFlags where
  ConfigFlags dc1 <> ConfigFlags dc2 = ConfigFlags (dc1 && dc2)

instance Monoid ConfigurationFlags where
  mempty = ConfigFlags True

data DebugFlags
  = DebugFlags
  { dump_ast  :: Bool
  , dump_tast :: Bool
  }
  deriving (Show)

instance Semigroup DebugFlags where
  DebugFlags d1 d2 <> DebugFlags d3 d4 = DebugFlags (d1 || d3) (d2 || d4)

instance Monoid DebugFlags where
  mempty = DebugFlags False False

-------------------------------------------------------------------------------

data LexerFlags
  = LexerFlags
  {  }

data ParserFlags
  = ParserFlags
  {  }

data TypecheckerFlags
  = TypecheckerFlags
  {  }
