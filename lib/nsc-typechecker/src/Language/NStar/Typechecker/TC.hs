{-# LANGUAGE RecordWildCards #-}

module Language.NStar.Typechecker.TC where

import Control.Monad.State (StateT, modify)
import Control.Monad.Writer (WriterT)
import Control.Monad.Except (Except)
import Data.Map (Map)
import Language.NStar.Typechecker.Env (Env)
import qualified Language.NStar.Typechecker.Env as Env
import Data.Located (Located)
import Data.Text (Text)
import Language.NStar.Typechecker.Core
import Data.Bifunctor (first, second)
import Language.NStar.Typechecker.Errors (TypecheckError)

type Typechecker a = StateT (Integer, Context) (WriterT [TypecheckError] (Except TypecheckError)) a

-- | The data type of contexts in typechecking.
data Context
  = Ctx
  { typeEnvironment    :: Env Type                               -- ^ An 'Env'ironment containing labels associated to their expected contexts
  , currentTypeContext :: Map (Located Register) (Located Type)  -- ^ The current typechecking context
  , currentKindContext :: Map (Located Text) (Located Kind)      -- ^ The current kindchecking context
  , currentLabel       :: Maybe (Located Text)                   -- ^ The last crossed label
  }

-- | Adds a type to the environment.
addType :: Located Text -> Located Type -> Typechecker ()
addType k v = modify $ second modifyTypeContext
  where modifyTypeContext ctx@Ctx{..} = ctx { typeEnvironment = Env.insert k v typeEnvironment }

-- | Increments the counter in the 'State' by one, effectively simulating a @counter++@ operation.
incrementCounter :: Typechecker ()
incrementCounter = modify $ first (+ 1)

setCurrentTypeContext :: Map (Located Register) (Located Type) -> Typechecker ()
setCurrentTypeContext newCtx = modify $ second putContext
  where putContext ctx = ctx { currentTypeContext = newCtx }

setCurrentKindContext :: Map (Located Text) (Located Kind) -> Typechecker ()
setCurrentKindContext newCtx = modify $ second putContext
  where putContext ctx = ctx { currentKindContext = newCtx }

setTypeEnvironment :: Env Type -> Typechecker ()
setTypeEnvironment newEnv = modify $ second setTypeEnv
  where setTypeEnv ctx = ctx { typeEnvironment = newEnv }

setLabel :: Located Text -> Typechecker ()
setLabel n = modify $ second setLbl
  where setLbl ctx = ctx { currentLabel = Just n }

--------------------------------------------------------
