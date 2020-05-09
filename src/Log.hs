{-# LANGUAGE RankNTypes #-}
{-  • Illegal qualified type: HasCallStack => Text -> m ()
      Perhaps you intended to use RankNTypes
    • In the definition of data constructor ‘Logger’
      In the data type declaration for ‘Logger’
-}
{-# LANGUAGE FlexibleContexts #-}
{-  • Non type-variable argument in the constraint: Has (Logger m) r
      (Use FlexibleContexts to permit this)
    • In the instance declaration for ‘Log (ReaderT r m)’
-}
{-# LANGUAGE UndecidableInstances #-}
{-  • The constraint ‘Has (Logger m) r’
        is no smaller than the instance head ‘Log (ReaderT r m)’
      (Use UndecidableInstances to permit this)
    • In the instance declaration for ‘Log (ReaderT r m)’
-}

module Log
  ( Loggable(..)
  , Log(..)
  , Logger(..)
  ) where

import Prelude

import Control.Monad.Reader (ReaderT, lift)
import Control.Monad.Reader.Class (asks)
import Data.Has (Has(..))
import Data.Text (Text)
import GHC.Stack (HasCallStack)

class Loggable a where
  fromLoggable :: a -> Text

class Monad m => Log m where
  logLn :: HasCallStack => Loggable a => a -> m ()

data Logger m = Logger
  { dologLn :: HasCallStack => Text -> m ()
  }

instance
  ( Has (Logger m) r
  , Monad m
  ) => Log (ReaderT r m) where
  logLn a =
    asks getter >>= \(Logger doLog) -> lift . doLog . fromLoggable $ a

instance Loggable Text where
  fromLoggable = id
