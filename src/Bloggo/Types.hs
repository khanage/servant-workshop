{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Bloggo.Types where

import           Control.Monad.Except
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Trans.Except
import           Crypto.JWT                 (ClaimsSet)
import qualified Data.ByteString.Lazy       as LBS (ByteString)
import           Servant.Auth.Server        (JWTSettings, ToJWT, makeJWT)
import           Servant.Server             (ServantErr, ServantErr (..),
                                             ServerT, err500)
import Data.Aeson (ToJSON(..), Value(..))
import GHC.Generics
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.Text.Lazy as LT
import Data.Time
import Control.Lens ((&))

type JwtExpirer = (UTCTime -> UTCTime)

data BloggoConfig = BloggoConfig { jwtSettings :: JWTSettings
                                 , jwtExpiry :: Maybe JwtExpirer
                                 }

newtype Bloggo a = Bloggo { unBloggo :: ReaderT BloggoConfig (ExceptT ServantErr IO) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadError ServantErr, MonadReader BloggoConfig)

type BlogServer api = ServerT api Bloggo

newtype RawJWT = RawJWT {unRawJWT :: LBS.ByteString}
  deriving (Eq, Show)

instance ToJSON RawJWT where
  toJSON (RawJWT bs) = String $ LT.toStrict $ LT.decodeUtf8 bs

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
