module Console.NStar.Flags.Internal where

data Flags
  = Flags
  { files         :: [FilePath]
  , configuration :: ConfigurationFlags
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
