{-# LANGUAGE
OverloadedLabels,
ViewPatterns,
OverloadedStrings
#-}

module API where

import Prelude

import Control.Lens ((^.))
import Data.Functor ((<&>))

import Develop

-- Imports from our simplified web framework
import Web.SimpleHttp (matches, run)
import Web.SimpleHttp.Types (Request(..), Response(..), MethodAndPath(..))
-- import Web.SimpleHttp.Types (RequestBody, Error(..))
import Web.SimpleHttp.Class (toResponseFrom)

-- Domain types and interfaces:
import Types (User(..), UserName(..), UserId(..), Password(..), Deletion(..))
-- import Types (UserRequest(..))
import Persist (Persist(..), Persistence(..))
import Log (Log(..), Logger(..))


-- | The API function, parametrized with the interfaces we defined
api ::
     Log m
  => Persist m
  => Request -> m Response
api request =
  case methodAndPath request of
    POST (matches "/user" -> Just []) ->
      createNewUser (request ^. #body) <&> toResponseFrom
    DELETE (matches "/user/:userId" -> Just [userId]) ->
      deleteUser (UserId userId) <&> toResponseFrom
    _ ->
      pure NoResponse
