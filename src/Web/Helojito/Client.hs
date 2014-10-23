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
       ) where

import           Data.Aeson                 hiding (Result)
import           Data.Aeson.Parser          (value)
import           Data.Attoparsec.ByteString (parseOnly)
import           Data.Either                (rights)
import           Control.Monad.Trans.Either
import           Control.Exception          (try, SomeException)

import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import           Data.Monoid                ((<>))
import qualified Data.Text.Encoding         as T
import           Data.Text                  (Text)
import           Network.Http.Client
import qualified System.IO.Streams          as Streams
import Debug.Trace

------------------------------------------------------------------------------
-- | Core Type
type Helojito a = EitherT HelojitoError (ReaderT Connection IO) a

------------------------------------------------------------------------------
-- | Error Types
data HelojitoError =
    ConnectionError
  | ParseError
  | NotFound
  | RequestError
  deriving (Show, Eq)

------------------------------------------------------------------------------
-- | Helojito API request method
runHelojito :: FromJSON a => Helojito a -> IO (Either HelojitoError a)
runHelojito requests = do
    con <- try (openConnection "localhost" 8090) :: IO (Either SomeException Connection)
    case con of
     Left e -> trace (show e) $ return $ Left ConnectionError
     Right conn -> do
       result <- flip runReaderT conn $ runEitherT requests
       closeConnection conn
       return result

------------------------------------------------------------------------------
-- | Request Builder for API
buildHJRequest :: FromJSON a => Text -> Helojito a
buildHJRequest url = do
    con <- lift ask
    bytes <- liftIO $ do
      req <- buildRequest $ do
        http GET $ "/v0/" <> T.encodeUtf8 url <> ".json"
        setHeader "Connection" "Keep-Alive"
        setAccept "application/json"
      sendRequest con req emptyBody
      receiveResponse con $ const Streams.read
    case bytes of
      Nothing -> left RequestError
      Just bs -> do
        liftIO $ print bs
        let xs = rights [parseOnly value bs, parseOnly json bs]
        case xs of
          []    -> left ParseError
          x : _ ->
            case fromJSON x of
             Success jsonBody -> right jsonBody
             _                -> left NotFound
