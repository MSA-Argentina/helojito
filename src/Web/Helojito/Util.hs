-- |
-- Module      : Web.Helojito.Util
-- Stability   : experimental
-- Portability : POSIX
module Web.Helojito.Util where

import           Data.Text (Text, pack)

------------------------------------------------------------------------------
-- | Convert `Show` constrained a to `Text`
toText :: Show a => a -> Text
toText = pack . show
