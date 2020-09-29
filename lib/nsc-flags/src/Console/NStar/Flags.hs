{-# LANGUAGE DuplicateRecordFields #-}

module Console.NStar.Flags
( extractFlags
,  -- * Re-exports
  CompilerFlags(..)
, lookupFlag
) where

import Console.NStar.Flags.Internal (Flags(..), CompilerFlags(..), lookupFlag)
import Console.NStar.Flags.Process
import System.Console.CmdArgs

extractFlags :: IO CompilerFlags
extractFlags = postProcessFlags =<< cmdArgsRun (cmdArgsMode cli)

cli :: Flags
cli = Flags
  { files = def &= args &= typ "FILES..."
  , flags = def &= typ "NAME=VALUE" &= explicit &= name "f" &= name "flag"
  } &= versionArg [ignore]
    &= helpArg [explicit, name "help", name "h"]
    &= program "nsc"
