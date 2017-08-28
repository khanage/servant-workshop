{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
module Bloggo.Types where

import qualified Data.Text as T
import           Control.Lens               ((&))
import           Control.Monad.Except
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Trans.Except
import           Crypto.JWT                 (ClaimsSet)
import           Data.Aeson                 (ToJSON (..), FromJSON(..), Value (..))
import qualified Data.ByteString.Lazy       as LBS (ByteString)
import qualified Data.Text.Lazy             as LT
import qualified Data.Text.Lazy.Encoding    as LT
import           Data.Time
import           GHC.Generics
import           Servant.API.ContentTypes   (MimeRender (..), PlainText)
import           Servant.Auth.Server        (JWTSettings, ToJWT, makeJWT)
import           Servant.Server             (Handler(..), (:~>)(..), ServantErr, ServantErr (..),
                                             ServerT, err500)
import Database.Persist.Sqlite
import Control.Monad.Logger

type JwtExpirer = (UTCTime -> UTCTime)

data BloggoConfig = BloggoConfig { jwtSettings :: JWTSettings
                                 , jwtExpiry :: Maybe JwtExpirer
                                 , userEndpoint :: T.Text
                                 , itemDbPool :: ConnectionPool
                                 }

newtype Bloggo a = Bloggo { unBloggo :: LoggingT (ReaderT BloggoConfig (ExceptT ServantErr IO)) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadError ServantErr, MonadReader BloggoConfig, MonadLogger)

bloggoToHandler
  :: BloggoConfig
  -> Bloggo :~> Handler
bloggoToHandler config =
  NT $ Handler . flip runReaderT config . runStdoutLoggingT . unBloggo

type BlogServer api = ServerT api Bloggo

newtype RawJWT = RawJWT {unRawJWT :: LBS.ByteString}
  deriving (Eq, Show)

instance ToJSON RawJWT where
  toJSON (RawJWT bs) = String $ LT.toStrict $ LT.decodeUtf8 bs

instance MimeRender PlainText RawJWT where
  mimeRender _ = unRawJWT

class Convertable a b where
  convert :: a -> b

class (Monad m) => MonadMakeJWT m where
  createJWT :: ToJWT a => a -> m RawJWT

instance MonadMakeJWT Bloggo where
  createJWT a = do
    jwtSettings <- asks jwtSettings
    timeMod <- asks jwtExpiry
    currentTime <- liftIO $ getCurrentTime

    let expiry = (&) currentTime <$> timeMod

    mJwt <- liftIO $ makeJWT a jwtSettings expiry
    either
      (\e -> throwError $ err500 {errBody = "Failed to encode jwt"})
      (pure . RawJWT)
      mJwt
