{-# LANGUAGE FlexibleContexts #-}
{-  • Non type-variable argument
        in the constraint: Has (Persistence m) r
      (Use FlexibleContexts to permit this)
    • In the instance declaration for ‘Persist (ReaderT r m)’
-}
{-# LANGUAGE UndecidableInstances #-}
{-  • The constraint ‘Has (Persistence m) r’
        is no smaller than the instance head ‘Persist (ReaderT r m)’
      (Use UndecidableInstances to permit this)
    • In the instance declaration for ‘Persist (ReaderT r m)’
-}

{-# LANGUAGE
DerivingStrategies,
RecordWildCards,
MultiParamTypeClasses,
DeriveGeneric,
OverloadedStrings
#-}

{-# LANGUAGE FlexibleInstances #-}
{-  • Illegal instance declaration for ‘Persist' m’
        (All instance types must be of the form (T a1 ... an)
         where a1 ... an are *distinct type variables*,
         and each type variable appears at most once in the instance head.
         Use FlexibleInstances if you want to disable this.)
    • In the instance declaration for ‘Persist' m’
-}
{-# LANGUAGE RecordWildCards #-}
{-      Illegal `..' in record pattern
47 |     (\UserRequest{..} -> Right (UserName username, Password password))
   |       ^^^^^^^^^^^^^^^
-}

module Develop where

-- Third party lib imports
import Prelude
-- import Control.Lens ((^.))
-- import Control.Monad.Reader (runReaderT)
import Data.Aeson (eitherDecode)
-- import Data.Functor ((<&>))
import Data.Has (Has(..))
import Data.Text (pack, unpack)
import Data.Text.Lazy (fromStrict)
import Data.Text.Lazy.Encoding (encodeUtf8)
import GHC.Generics (Generic)
import Data.Generics.Labels ()
import Control.Monad.Reader (ReaderT, asks, lift, MonadReader, ask)

-- Imports from our simplified web framework
-- import Web.SimpleHttp (matches, run)
-- import Web.SimpleHttp.Types (Request(..), Response(..), MethodAndPath(..))
import Web.SimpleHttp.Types (RequestBody, Error(..))
-- import Web.SimpleHttp.Class (toResponseFrom)

-- Domain types and interfaces:
import Types (User(..), UserName(..), UserId(..), Password(..), Deletion(..))
import Types (UserRequest(..))
import Persist (Persist(..), Persistence(..))
import Log (Log(..), Logger(..), Loggable(..))




-- | Create New User, without using the Persist typeclass, specialized to the IO monad
-- The first argument is a function (partially applied already with a database)
createNewUser'' :: (UserName -> Password -> IO UserId)
                 -> RequestBody
                 -> IO (Either Error User)
createNewUser'' persistUser body =
  case bodyToUser body of
    Left err -> pure . Left $ err
    Right (user, pass) -> do
      -- Persist user:
      userId <- persistUser user pass
      -- Create a response from the persisted argument:
      pure . Right $ User { userName = user, userId = userId }


-- Nest Persistence Handle in Application Handle
data HandlePersistence'' = HandlePersistence''
  { doPersistUser'' :: UserName -> Password -> IO UserId
  , doGetUser'' :: UserId -> IO (Maybe User)
  }
data Application'' = Application''
  { persistence'' :: HandlePersistence''
  , callIntoThirdPartyService'' :: UserName -> IO Bool
  -- , logLn'' :: Loggable a => a -> IO ()
  }


-- Get rid of IO by abstracting to general monad:
data HandlePersistence' m = HandlePersistence'
  { doPersistUser' :: UserName -> Password -> m UserId
  , doGetUser' :: UserId -> m (Maybe User)
  }
--
data HandleApplication m = HandleApplication
  { persistence' :: HandlePersistence' m
  , callIntoThirdPartyService :: UserName -> m Bool
  -- , logLn :: Loggable a => a -> m ()
  }

getPersistUser' :: HandleApplication m -> (UserName -> Password -> m UserId)
getPersistUser' app = undefined -- app & persistence & persistUser

-- rewrite in terms of Reader monad:
-- getPersistUser :: MonadReader (HandleApplication m) => m (UserName -> Password -> m UserId)
-- getPersistUser = do
--   app <- ask
--   pure $ app -- & persistence & persistUser





-- | Type class for bundling things that fall under the same effect.
-- Part of the "Handle" pattern.
class Monad m => Persist' m where
  persistUser' :: UserName -> Password -> m UserId
  getUser :: UserId -> m (Maybe User)


-- | Create New User, Using the Persist typeclass
createNewUser' :: Persist' m => RequestBody -> m (Either Error User)
createNewUser' body =
  case bodyToUser body of
    Left err -> pure . Left $ err
    Right (user, pass) -> do
      userId <- persistUser' user pass
      -- Create a response from the persisted argument:
      pure . Right $ User { userName = user, userId = userId }
-- Don't have to pass the Application Handle to this anymore


--



instance ( MonadReader (HandlePersistence' m) m, Monad m ) =>
         Persist' m where
  persistUser' user pass =
    ask >>= \(HandlePersistence' persist _) -> persist user pass
  getUser userId =
    ask >>= \(HandlePersistence' _ get) -> get userId

--
instance ( Has (HandlePersistence' m) r, Monad m) =>
         Persist' (ReaderT r m) where
  persistUser' user pass =
    asks getter >>= \(HandlePersistence' persist _) -> lift $ persist user pass
  getUser userId =
    asks getter >>= \(HandlePersistence' _ get) -> lift $ get userId




--
-- | Create New User, WITH LOGGING (The signature is different.)
createNewUser :: Persist m => Log m
                           => RequestBody -> m (Either Error User)
createNewUser body =
  case bodyToUser body of
    Left err ->
      Left err <$ logLn ("Couldn't convert " <> body <> "to user and pass")
    Right (user, pass) -> do
      logLn $ "Going to create " <> unUserName user
      userId <- persistUser user pass
      -- Create a response from the persisted argument:
      pure . Right $ User { userName = user, userId = userId }
-- We don't have to pass the Application to this function

bodyToUser :: RequestBody -> Either Error (UserName, Password)
bodyToUser =
  either
    (Left . BadRequest . pack)
    (\UserRequest{..} -> Right (UserName username, Password password))
    . eitherDecode
    . encodeUtf8
    . fromStrict

-- | Data type containing grouping of similar functions, for parametrizing
-- The "Handle" pattern.
data Application m = Application
  { persistence :: Persistence m  -- Persistence is another such grouping
  , logger :: Logger m
  }
  deriving stock (Generic)
-- The IO has been abstracted out to just "m"
-- We can choose which monad to evaluate our program in.  We can then limit the effects of the monad.
-- We could run these as pure functions by using the Identity monad


instance Has (Logger m) (Application m) where
  getter = logger
  modifier f a = a { logger = f . logger $ a }

instance Has (Persistence m) (Application m) where
  getter = persistence
  modifier f a = a { persistence = f . persistence $ a }
