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
import           Control.Exception          (try, SomeException)
import           Control.Lens

import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Reader (ReaderT, asks, runReaderT)
import           Data.Monoid                ((<>))
import           Data.Text                  (Text, unpack)
import           Network.Wreq


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
  deriving (Show, Eq)

data ConnConf = ConnConf { token :: ByteString
                         , apiurl :: String }

------------------------------------------------------------------------------
-- | Helojito API request method
runHelojito :: FromJSON a => Helojito a -> ConnConf -> IO (Either HelojitoError a)
runHelojito requests (ConnConf t u) = flip runReaderT (u, t) $ runEitherT requests

------------------------------------------------------------------------------
-- | Request Builder for API
buildHJRequest :: FromJSON a => Text -> Helojito a
buildHJRequest url = do
    base' <- lift . asks $ fst
    token' <- lift . asks $ snd

    let opts = defaults & header "Authorization" .~ ["Token " <> token']

    er <- safeIO $ getWith opts $ base' ++ unpack url

    case er of
        Left da -> trace (show da) $ left ConnectionError
        Right r -> case eitherDecode (r ^. responseBody) of
                       Left _ -> left ParseError
                       Right o -> right o

safeIO :: IO a -> Helojito (Either SomeException a)
safeIO action = liftIO $ try action
