module Main where

import           Web.Helojito

import           Data.Text          (unpack)
import           Data.Text.Encoding (encodeUtf8)
import           Control.Monad

import           Helojito.Options
import qualified Helojito.Config as C

main :: IO ()
main = do
    conf <- C.readConf
    opts <- getOptions

    let conconf = ConnConf (encodeUtf8 $ C.token conf)
                           (unpack $ C.apiurl conf)

    print =<< runHelojito one conconf
  where
    one = getTasks
