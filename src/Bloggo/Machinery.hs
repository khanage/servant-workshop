{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Bloggo.Machinery where

import           Bloggo.Api
import           Bloggo.Item
import           Bloggo.Types
import           Control.Lens               ((&))
import           Control.Monad.Logger       (runStdoutLoggingT, runNoLoggingT)
import           Control.Monad.Trans.Reader
import qualified Data.Text.IO               as T
import           Data.Time
import           Database.Persist
import           Database.Persist.Sqlite
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import qualified Servant.Auth               as Auth
import qualified Servant.Auth.Server        as Auth
import           System.IO
import Crypto.JOSE.JWK (JWK)

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

  pool <- runStdoutLoggingT $ createSqlitePool "bloggo.db" 4
  flip runSqlPool pool $ runMigration migrateAll

  let
    config = BloggoConfig jwtConfig expiryMod "http://localhost:5000" pool

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
