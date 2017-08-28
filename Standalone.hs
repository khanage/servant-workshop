{-# language TypeOperators, DeriveGeneric, DataKinds #-}
module Main where

import Servant
import Data.Aeson
import GHC.Generics
import System.IO
import Network.Wai
import Network.Wai.Handler.Warp

data ServiceResult = Result { id :: Int, name :: String } deriving (Eq, Show, Generic)

instance ToJSON ServiceResult where

type TrivialApi = "hello" :> Get '[JSON] ServiceResult

trivialApi :: Proxy TrivialApi
trivialApi = Proxy

main :: IO ()
main = do
  let port = 5000
      settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("Listening on " ++ show port)) $
        defaultSettings
  runSettings settings =<< pure (serve trivialApi server)

server :: Server TrivialApi
server = pure $ Result 1 "Darth Vader"
