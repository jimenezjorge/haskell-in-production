{-# LANGUAGE LambdaCase #-}
{-  Illegal lambda-case (use -XLambdaCase)
-}
{-# LANGUAGE DerivingStrategies #-}
{-  Illegal deriving strategy: stock
    Use DerivingStrategies to enable this extension
-}
{-# LANGUAGE DeriveGeneric #-}
{-  • Can't make a derived instance of
        ‘Generic UserRequest’ with the stock strategy:
        You need DeriveGeneric to derive an instance for this class
    • In the data declaration for ‘UserRequest’
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-  • Can't make a derived instance of
        ‘IsString UserName’ with the newtype strategy:
        Try GeneralizedNewtypeDeriving for GHC's newtype-deriving extension
    • In the newtype declaration for ‘UserName’

    ‘ToJSON UserName’ with the newtype strategy:
    ‘IsString Password’ with the newtype strategy:
    ‘ToJSON Password’ with the newtype strategy:
    ‘IsString UserId’ with the newtype strategy:
    ‘ToJSON UserId’ with the newtype strategy:
    ‘Generic User’ with the stock strategy:
-}
{-# LANGUAGE FlexibleContexts #-}
{-  • Non type-variable argument
        in the constraint: Has (Persistence m) r
      (Use FlexibleContexts to permit this)
    • In the instance declaration for ‘Persist (ReaderT r m)’
-}

module Types where

import Prelude

import Data.Aeson (FromJSON, ToJSON, encode)
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.String (IsString)
import GHC.Generics (Generic)
import Network.HTTP.Types.Status (status201, status204, status404)

import Web.SimpleHttp ()
import Web.SimpleHttp.Types (Response(..))
import Web.SimpleHttp.Class (ToResponse(..))

data UserRequest = UserRequest
  { username :: Text
  , password :: Text
  }
  deriving stock (Generic)

instance FromJSON UserRequest

newtype UserName =
  UserName { unUserName :: Text }
  deriving newtype (IsString, ToJSON)

newtype Password =
  Password { unPassword :: Text }
  deriving newtype (IsString, ToJSON)

newtype UserId =
  UserId { unUserId :: Text }
  deriving newtype (IsString, ToJSON)

data Deletion
  = Deleted
  | NotDeleted

instance ToResponse Deletion where
  toResponseFrom = \case
    NotDeleted ->
      Response status404 mempty
    Deleted ->
      Response status204 mempty

data User = User
  { userId :: UserId
  , userName :: UserName
  }
  deriving stock (Generic)

instance ToJSON User

instance ToResponse User where
  toResponseFrom =
    Response status201 . toStrict . decodeUtf8 . encode
