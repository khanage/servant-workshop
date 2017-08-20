{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TypeOperators #-}
module Bloggo.Api where

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

data User = User { name :: Text, email :: Text }
  deriving (Eq, Show, Generic)

instance FromJSON User
instance ToJSON User
instance FromJWT User
instance ToJWT User

type BloggoApi auths = LoginApi :<|> (Auth auths User :> ItemApi)

server :: BlogServer (BloggoApi auths)
server = loginServer :<|> protected

-- | 'Protected' will be protected by 'auths', which we still have to specify.
protected :: AuthResult User -> BlogServer ItemApi
-- If we get an "Authenticated v", we can trust the information in v, since
-- it was signed by a key we trust.
protected (Authenticated user) = itemServer user
-- Otherwise, we return a 401.
protected _ = throwAll err401

type LoginApi =
  "gimme-token" :> Get '[JSON] RawJWT

loginServer :: BlogServer LoginApi
loginServer = createTokenForFree

createTokenForFree :: Bloggo RawJWT
createTokenForFree = do
  createJWT (User "Jon Snow" "jon@thewall.north")

type ItemApi =
  "item" :> Get '[JSON] [Item] :<|>
  "item" :> Capture "itemId" Integer :> Get '[JSON] Item

itemServer
  :: User
  -> BlogServer ItemApi
itemServer user =
  getItems :<|>
  getItemById

getItems :: Bloggo [Item]
getItems = pure [exampleItem]

getItemById
  :: Integer
  -> Bloggo Item
getItemById = \case
  0 -> pure exampleItem
  _ -> throwError err404

exampleItem :: Item
exampleItem = Item 0 "example item"

-- * item

data Item
  = Item {
    itemId :: Integer,
    itemText :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON Item
instance FromJSON Item
