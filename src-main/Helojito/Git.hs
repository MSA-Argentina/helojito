{-# LANGUAGE OverloadedStrings #-}
module Helojito.Git
       ( gitFromHash
       , GitInfo (GitInfo)
       ) where

import Git
import Git.Libgit2
import Data.Text as T
import Data.Time
import Shelly


data GitInfo = GitInfo { _msg :: Text
                       , _when :: Text } deriving (Show)

gitFromHash :: Text -> IO GitInfo
gitFromHash hash = do
    gitdir <- unpack . T.init <$> (shelly . silently $ run "git" ["rev-parse", "--git-dir"])
    withRepository lgFactory gitdir $ do
        rcoid <- parseObjOid hash
        c <- lookupCommit rcoid

        let msg = T.init $ commitLog c
        let sig = commitAuthor c

        let z = signatureWhen sig
        let day = localDay $ zonedTimeToLocalTime z

        return $ GitInfo msg (pack . show $ day)
