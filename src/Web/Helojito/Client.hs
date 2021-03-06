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
import           Control.Monad.Trans.Either
import           Control.Exception          (try)
import           Control.Lens

import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Reader (ReaderT, asks, runReaderT)
import           Data.Monoid                ((<>))
import           Data.Text                  (Text, unpack)
import           Network.HTTP.Client        (HttpException(..))
import           Network.Wreq
import           Web.Helojito.Concurrent


------------------------------------------------------------------------------
-- | Core Type
type Helojito a = ConcurrentT (EitherT HelojitoError (ReaderT (String, ByteString) IO)) a

------------------------------------------------------------------------------
-- | Error Types
data HelojitoError =
    ConnectionError String
  | ParseError String
  | NotFound
  deriving (Show)

data ConnConf = ConnConf { token :: ByteString
                         , apiurl :: String }

------------------------------------------------------------------------------
-- | Helojito API request method
runHelojito :: Helojito a -> ConnConf -> IO (Either HelojitoError a)
runHelojito requests (ConnConf t u) = runReaderT (runEitherT $ runConcurrentT requests) (u, t)

------------------------------------------------------------------------------
-- | Request Builder for API
buildHJRequest :: FromJSON a => Bool -> Maybe Value -> Text -> Helojito a
buildHJRequest put' mjson_data url = do
    base' <- lift . lift . asks $ fst
    token' <- lift . lift . asks $ snd

    let action = base' ++ unpack url
    let opts = defaults & header "Authorization" .~ ["Token " <> token']

    res <- safeIO $ case mjson_data of
        Nothing -> getWith opts action
        Just json_data -> case put' of
                            True -> putWith opts action json_data
                            False -> postWith opts action json_data

    lift $ case res of
           Left da -> left $ ConnectionError (show da)
           Right r -> case eitherDecode (r ^. responseBody) of
                          Left e -> left $ ParseError e
                          Right o -> right o

safeIO :: IO a -> Helojito (Either HttpException a)
safeIO io = liftIO $ try io
