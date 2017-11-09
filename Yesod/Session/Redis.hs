{-# LANGUAGE OverloadedStrings #-}
module Yesod.Session.Redis
  ( getSessionKey
  , redisSessionBackend
  ) where

import           Control.Arrow             ((&&&))
import           Data.ByteString           (ByteString)
import qualified Data.ByteString.Char8     as BC
import qualified Data.Map                  as M
import           Data.Maybe
import qualified Data.Text                 as T
import           Data.Time                 (NominalDiffTime, addUTCTime, getCurrentTime)
import           Database.Redis
import qualified Network.Wai               as W
import           Web.Cookie
import qualified Web.RedisSession          as R
import           Yesod.Core
import           Yesod.Core.Types


getSessionKey :: ByteString -- ^ Name of the session (used for cookies and redis key)
              -> W.Request  -- ^ incoming Wai Request
              -> IO (ByteString, Maybe ByteString)
getSessionKey sessionName req = do
  let val = do
        raw <- lookup "Cookie" $ W.requestHeaders req
        lookup sessionName $ parseCookies raw
  key <- case val of
           Nothing ->
             R.newKey
           Just k ->
             return k
  return (key, val)

loadRedisSession :: Connection
                 -> ByteString -- ^ Name of the session (used for cookies and redis key)
                 -> Maybe ByteString -- ^ domain for the cookie
                 -> NominalDiffTime -- ^ How long until the session expires
                 -> W.Request -- ^ incoming Wai Request
                 -> IO (SessionMap, SaveSession)
loadRedisSession conn sessionName mSessionDomain timeout req = do
  (key, val) <- getSessionKey sessionName req
  sess <- case val of
            Nothing ->
              return M.empty
            Just sessionKey -> do
              result <- R.getSession conn sessionKey
              return $ M.fromList $ map ((T.pack . BC.unpack . fst) &&& snd) $ fromJust result
  let saveSession = saveRedisSession conn sessionName mSessionDomain timeout key
  return (sess, saveSession)

saveRedisSession :: Connection -- ^ Redis connection pool
                 -> ByteString -- ^ Name of the session (used for cookies and redis key)
                 -> Maybe ByteString -- ^ domain for the cookie
                 -> NominalDiffTime -- ^ How long until the session expires
                 -> ByteString -- ^ session key (value of cookie)
                 -> SessionMap -- ^ session information
                 -> IO [Header]
saveRedisSession conn sessionName mSessionDomain timeout key sess = do
  now <- getCurrentTime
  let expires = timeout `addUTCTime` now
  R.setSessionExpiring conn key ( map
                                  ((BC.pack . T.unpack . fst) &&& snd)
                                  (M.toList sess)
                                ) (truncate timeout)
  return [ AddCookie def { setCookieName     = sessionName
                         , setCookieValue    = key
                         , setCookiePath     = Just "/"
                         , setCookieExpires  = Just expires
                         , setCookieDomain   = mSessionDomain
                         , setCookieHttpOnly = True
                         }
         ]

redisSessionBackend :: Maybe Connection -- ^ If Nothing then a default connection is created via 'defaultConnectInfo'
                    -> ByteString -- ^ Name of the session (used for cookies and redis key)
                    -> Maybe ByteString -- ^ domain for the cookie (Nothing does current domain)
                    -> NominalDiffTime -- ^ How long until the session expires (converted to seconds when used)
                    -> IO SessionBackend
redisSessionBackend mConnection sessionName mSessionDomain timeout = do
  conn <- case mConnection of
            Nothing ->
              connect defaultConnectInfo
            Just connection ->
              pure connection
  return SessionBackend {
    sbLoadSession = loadRedisSession conn sessionName mSessionDomain timeout
  }
