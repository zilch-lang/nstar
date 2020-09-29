{-# LANGUAGE DuplicateRecordFields #-}

module Console.NStar.Flags
( extractFlags
,  -- * Re-exports
  CompilerFlags(..)
, lookupFlag
, LexerFlags(..), ParserFlags(..), TypecheckerFlags(..)
) where

import Console.NStar.Flags.Internal
import Console.NStar.Flags.Process
import System.Console.CmdArgs

extractFlags :: IO CompilerFlags
extractFlags = postProcessFlags =<< cmdArgsRun (cmdArgsMode cli)

cli :: Flags
cli = Flags
  { files = def &= args &= typ "FILES..."
  , flags = def &= typ "NAME=VALUE" &= explicit &= name "f" &= name "flag" &= help "Sets a compiler flag to a given value"
  } &= versionArg [ignore]
    &= helpArg [explicit, name "help", name "h"]
    &= program "nsc"
