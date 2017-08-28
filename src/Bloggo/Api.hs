{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards, TemplateHaskell #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TypeOperators #-}
module Bloggo.Api where

import Control.Monad.Except
import Bloggo.Item
import           Bloggo.Types
import           Data.Aeson
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.ByteString.Lazy as LBS
import           GHC.Generics
import           Servant
import           Servant.Auth
import           Servant.Auth.Server
import Control.Monad.IO.Class
import Bloggo.UserData
import Control.Monad.Logger
import Database.Persist
import qualified Data.ByteString.Char8 as BS
import Data.Monoid ((<>))

data User = User { name :: Text, email :: Text }
  deriving (Eq, Show, Generic)

instance FromJSON User
instance ToJSON User
instance FromJWT User
instance ToJWT User

type BloggoApi auths = LoginApi :<|> (Auth auths User :> SecureApi)

server :: BlogServer (BloggoApi auths)
server = loginServer :<|> protected

protected :: AuthResult User -> BlogServer SecureApi
protected (Authenticated user) = itemServer user :<|> authorServer user
protected _ = throwAll err401

type LoginApi =
  "gimme-token" :> Get '[PlainText] RawJWT

loginServer :: BlogServer LoginApi
loginServer = createTokenForFree

createTokenForFree :: Bloggo RawJWT
createTokenForFree = do
  createJWT (User "Jon Snow" "jon@thewall.north")

type SecureApi = ItemApi :<|> AuthorApi

type ItemApi =
  "item" :> Get '[JSON] [Item] :<|>
  "item" :> Capture "itemId" DbItemId :> Get '[JSON] Item :<|>
  "item" :> ReqBody '[JSON] DbItem :> Post '[JSON] DbItemId

itemServer
  :: User
  -> BlogServer ItemApi
itemServer user =
  getItems :<|>
  getItemById :<|>
  postItem user

getItems :: Bloggo [Item]
getItems = loadItems

getItemById
  :: DbItemId
  -> Bloggo Item
getItemById = loadSpecificItem

postItem
  :: User
  -> DbItem
  -> Bloggo b
postItem user dbItem = do
  $logDebug "Creating a new item"
  key <- createItem dbItem
  redirect $ "/item/" <> BS.pack (show key)

redirect
  :: BS.ByteString
  -> Bloggo a
redirect to =
  throwError err301 { errHeaders = [("Location", to)]}

type AuthorApi =
  "author" :> QueryParam "id" Int :> Get '[JSON] UserData

authorServer
  :: User
  -> BlogServer AuthorApi
authorServer User{..} =
  getAuthorById

getAuthorById :: Maybe Int -> Bloggo UserData
getAuthorById Nothing = throwError err404
getAuthorById (Just id) = loadUserDataFor id
