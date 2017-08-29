{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Main where

import           Data.Aeson
import           Data.Text                (Text)
import qualified Data.Text                as T
import           GHC.Generics             (Generic)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Server
import           System.IO                (hPutStrLn, stderr)

data User = User { id :: Int, name :: Text } deriving (Eq, Show, Generic)
instance ToJSON User where

type TrivialApi = "user" :> Capture "userId" Int :> Get '[JSON] User

trivialApi :: Proxy TrivialApi
trivialApi = Proxy

main :: IO ()
main = do
  let port = 5000
      settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("Listening on " ++ show port)
                        *> hPutStrLn stderr (T.unpack $ layout trivialApi)) $
        defaultSettings
  runSettings settings =<< pure (serve trivialApi server)

server :: Server TrivialApi
server = getUserById

getUserById userId = pure $ User userId "Darth Vader"

