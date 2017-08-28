{-# LANGUAGE DeriveGeneric #-}
module Bloggo.UserData where

import           Bloggo.Types
import           Control.Lens
import           Control.Monad.Except (throwError)
import           Control.Monad.Reader
import           Data.Aeson
import qualified Data.Text            as T
import           GHC.Generics         (Generic)
import           Network.Wreq
import           Servant              (err404)

data UserData = UserData { id :: Int, name :: T.Text }
  deriving (Eq, Show, Generic)

instance FromJSON UserData where
instance ToJSON UserData where

class (Monad m) => MonadLoadUserData m where
  loadUserDataFor :: Int -> m UserData

instance MonadLoadUserData Bloggo where
  loadUserDataFor userId = do
    endpoint <- asks userEndpoint <&> T.unpack

    let resource = endpoint ++ "/hello"
        textId = T.pack $ show $ userId
        options = defaults & param "id" .~ [textId]

    r <- liftIO $ getWith options resource

    maybe (throwError err404) pure $ r ^? responseBody >>= decode
