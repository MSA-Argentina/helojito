{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Web.Helojito.Client
-- Stability   : experimental
-- Portability : POSIX
module Web.Helojito.Client
       ( runHelojito
       , buildHJRequest
       , Helojito
       , HelojitoError (..)
       , ConnConf      (..)
       ) where

import           Data.Aeson                 hiding (Result)
import           Data.ByteString            (ByteString)
import           Data.Word                  (Word16)
import           Control.Monad.Trans.Either
import           Control.Exception          (catch, SomeException)
import           Control.Lens

import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Reader (ReaderT, asks, runReaderT)
import           Data.Monoid                ((<>))
import           Data.Text                  (Text, unpack)
import           Network.Wreq
import           Network.HTTP.Client        (HttpException(..))
import Debug.Trace

------------------------------------------------------------------------------
-- | Core Type
type Helojito a = EitherT HelojitoError (ReaderT (String, ByteString) IO) a

------------------------------------------------------------------------------
-- | Error Types
data HelojitoError =
    ConnectionError
  | ParseError
  | NotFound
  | RequestError
  deriving (Show, Eq)

data ConnConf = ConnConf { token :: ByteString
                         , apiurl :: String }

------------------------------------------------------------------------------
-- | Helojito API request method
runHelojito :: FromJSON a => Helojito a -> ConnConf -> IO (Either HelojitoError a)
runHelojito requests (ConnConf t u) = do
   result <- flip runReaderT (u, t) $ runEitherT requests
   return result

------------------------------------------------------------------------------
-- | Request Builder for API
buildHJRequest :: FromJSON a => Text -> Helojito a
buildHJRequest url = do
    base' <- lift . asks $ fst
    token' <- lift . asks $ snd

    let opts = defaults & header "Authorization" .~ ["Token " <> token']

    r <- liftIO $ getWith opts $ base' ++ unpack url

    case eitherDecode (r ^. responseBody) of
        Left _ -> left ParseError
        Right o -> right o

handleHttpError e = case e of
    FailedConnectionException2 _ _ _ _ -> left ConnectionError
