{-# LANGUAGE DuplicateRecordFields #-}

module Console.NStar.Flags.Process
( postProcessFlags
) where

import Console.NStar.Flags.Internal
import Data.List.Extra (stripInfix)
import qualified Data.Map as Map

postProcessFlags :: Flags -> IO CompilerFlags
postProcessFlags Flags{ files = _files
                      , flags = _flags
                      } = do
  pure $ CFlags
    { files = _files
    , flags = Map.fromList (parseFlag <$> _flags)
    }

parseFlag :: String -> (String, String)
parseFlag s = maybe (s, "") id (stripInfix "=" s)
