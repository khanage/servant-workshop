{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Bloggo.Machinery where

import           Bloggo.Api
import           Bloggo.Types
import           Control.Lens               ((&))
import           Control.Monad.Trans.Reader
import qualified Data.Text.IO               as T
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import qualified Servant.Auth               as Auth
import qualified Servant.Auth.Server        as Auth
import           System.IO
import Data.Time

runAppOn
  :: Port
  -> Maybe JwtExpirer
  -> IO ()
runAppOn port expiryMod = do
  jwtSigningKey <- Auth.generateKey

  let
    jwtConfig = Auth.defaultJWTSettings jwtSigningKey

    serverContext = Auth.defaultCookieSettings :. jwtConfig :. EmptyContext

    api = Proxy :: Proxy (BloggoApi '[Auth.JWT])

  config <- loadConfig jwtConfig expiryMod

  let
    startupMessage = do
      hPutStrLn stderr ("listening on port " ++ show port)
      T.hPutStrLn stderr (layoutWithContext api serverContext)

    settings = setPort port
             $ setBeforeMainLoop startupMessage
             $ defaultSettings

  bloggoToHandler config
    & flip enter server
    & serveWithContext api serverContext
    & runSettings settings

bloggoToHandler
  :: BloggoConfig
  -> Bloggo :~> Handler
bloggoToHandler config =
  NT $ Handler . flip runReaderT config . unBloggo

loadConfig
  :: Auth.JWTSettings
  -> Maybe JwtExpirer
  -> IO BloggoConfig
loadConfig jwtSettings timeMod =
  pure $ BloggoConfig jwtSettings timeMod
