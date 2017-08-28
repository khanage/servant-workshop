{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Bloggo.Item where

import Control.Monad.IO.Class
import Database.Persist.Sql (SqlPersistT, runSqlPool)
import Bloggo.Types
import Data.Aeson
import Database.Persist
import Database.Persist.TH
import GHC.Generics        (Generic)
import Servant
import Control.Monad.Reader (asks)
import Control.Monad.Logger

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
DbItem json
  text String
  deriving Show
|]

data Item = Item
  { itemId :: DbItemId
  , itemText :: String
  } deriving (Eq, Show, Generic)

instance ToJSON Item where
instance FromJSON Item where

instance Convertable (Entity DbItem) Item where
  convert entity =
    let itemText = dbItemText $ entityVal entity
        itemId = entityKey entity
    in Item{..}

class (Monad m) => MonadItemData m where
  loadItems :: m [Item]
  loadSpecificItem :: DbItemId -> m Item

instance MonadItemData Bloggo where
  loadItems = do
    $logInfo $ "Getting all items"
    items :: [Entity DbItem] <- runDB $ selectList [] []
    pure $ convert <$> items

  loadSpecificItem key = do
    maybeItem <- runDB $ getEntity key
    maybe (throwError err404) (pure . convert) maybeItem

exampleItem :: Item
exampleItem = Item undefined "example item"

runDB :: SqlPersistT IO a -> Bloggo a
runDB query = asks itemDbPool >>= liftIO . runSqlPool query

createItem :: DbItem -> Bloggo DbItemId
createItem = runDB . insert

createExampleItem :: Bloggo DbItemId
createExampleItem =
  runDB $ insert $ DbItem "example item"
