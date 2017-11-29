{-# LANGUAGE OverloadedStrings #-}
module Yesod.Session.Redis
  ( redisSessionBackend
  , redisSessionBackend'
  , simpleRedisSessionBackend
  ) where

import           Control.Arrow             ((&&&))
import           Data.ByteString           (ByteString)
import qualified Data.ByteString.Char8     as BC
import qualified Data.Map                  as M
import           Data.Maybe
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as TE
import           Data.Time                 (NominalDiffTime, addUTCTime, getCurrentTime)
import           Database.Redis
import qualified Network.Wai               as W
import           Web.Cookie
import qualified Web.RedisSession          as R
import           Yesod.Core
import           Yesod.Core.Types

loadRedisSession :: Connection
                 -> ByteString -- ^ Name of the session (used for cookies and redis key)
                 -> Maybe ByteString -- ^ domain for the cookie
                 -> NominalDiffTime -- ^ How long until the session expires
                 -> (W.Request -> IO (Either () (Maybe ByteString)))
                 -- ^ A function to determine if the session should be forced to be new or forces to use a specific key
                 --   Left will cause the normal session lookup to occur
                 --   Right Nothing will force a new session with a new key
                 --   Right Just will force the session to use the specified key
                 -> (W.Request -> Bool) -- ^ A function to determine if the session should be saved
                 -> (W.Request -> Maybe T.Text) -- ^ Just with sessionKeyName will put the session key in the session
                                                -- ^ Nothing will not put the session key in the session
                 -> W.Request -- ^ incoming Wai Request
                 -> IO (SessionMap, SaveSession)
loadRedisSession conn sessionName mSessionDomain timeout overrideSession shouldSave shouldAddSessionKey req = do
  shouldOverride <- overrideSession req
  let val = case shouldOverride of
              Left _ -> do
                raw <- lookup "Cookie" $ W.requestHeaders req
                lookup sessionName $ parseCookies raw
              Right mToken ->
                mToken
  key <- case val of
           Nothing ->
             R.newKey
           Just k ->
             pure k
  sess <- case val of
            Nothing ->
              pure $ case shouldAddSessionKey req of
                       Nothing ->
                         M.empty
                       Just sessionKeyName ->
                         M.singleton sessionKeyName key
            Just sessionKey -> do
              result <- R.getSession conn sessionKey
              let session = M.fromList $ map ((TE.decodeUtf8 . fst) &&& snd) $ fromJust result
              pure $ case shouldAddSessionKey req of
                       Nothing ->
                         session
                       Just sessionKeyName ->
                         M.insert sessionKeyName key session
  let saveSession = if shouldSave req then
                      saveRedisSession conn sessionName mSessionDomain timeout key
                    else
                      const $ pure []
  pure (sess, saveSession)

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
  pure [ AddCookie def { setCookieName     = sessionName
                       , setCookieValue    = key
                       , setCookiePath     = Just "/"
                       , setCookieExpires  = Just expires
                       , setCookieDomain   = mSessionDomain
                       , setCookieHttpOnly = True
                       }
       ]

-- | Uses a default redis connection and the domain of the request
simpleRedisSessionBackend :: ByteString -- ^ Name of the session (used for cookies and redis key)
                          -> NominalDiffTime -- ^ How long until the session expires (converted to seconds when used)
                          -> IO SessionBackend
simpleRedisSessionBackend sessionName timeout =
  redisSessionBackend' Nothing sessionName Nothing timeout (const (pure $ Left ())) (const True) (const Nothing)


redisSessionBackend :: Maybe Connection -- ^ If Nothing then a default connection is created via 'defaultConnectInfo'
                    -> ByteString -- ^ Name of the session (used for cookies and redis key)
                    -> Maybe ByteString -- ^ domain for the cookie (Nothing does current domain)
                    -> NominalDiffTime -- ^ How long until the session expires (converted to seconds when used)
                    -> IO SessionBackend
redisSessionBackend mConnection sessionName mSessionDomain timeout =
  redisSessionBackend' mConnection sessionName mSessionDomain timeout (const (pure $ Left ())) (const True) (const Nothing)


-- | Use this if you'd like to specify whether the session key is put in the session and if the session should be saved
--   or loaded based off the Wai Request
-- A Function you can use to get the Yesod Route from the Wai Request:
-- @
-- routeFromRequest :: ParseRoute a => W.Request -> Maybe (Route a)
-- routeFromRequest req =
--   let
--     path = W.pathInfo req
--     convert sofar (t, mT) = sofar ++ [ (t, fromMaybe "" mT) ]
--     query = foldl' convert [] (queryToQueryText $ W.queryString req) :: [ (Text, Text) ]
--   in
--     parseRoute (path, query)
-- @
redisSessionBackend' :: Maybe Connection -- ^ If Nothing then a default connection is created via 'defaultConnectInfo'
                     -> ByteString -- ^ Name of the session (used for cookies and redis key)
                     -> Maybe ByteString -- ^ domain for the cookie (Nothing does current domain)
                     -> NominalDiffTime -- ^ How long until the session expires (converted to seconds when used)
                     -> (W.Request -> IO (Either () (Maybe ByteString)))
                     -- ^ A function to determine if the session should be forced to be new or forces to use a specific key
                     --   Left will cause the normal session lookup to occur
                     --   Right Nothing will force a new session with a new key
                     --   Right Just will force the session to use the specified key
                     -> (W.Request -> Bool) -- ^ A function to determine if the session should be saved
                     -> (W.Request -> Maybe T.Text)
                     -- ^ Just with sessionKeyName will put the session key in the session
                     --   Nothing will not put the session key in the session
                     -> IO SessionBackend
redisSessionBackend' mConnection sessionName mSessionDomain timeout overrideSession shouldSave shouldAddSessionKey = do
  conn <- case mConnection of
            Nothing ->
              connect defaultConnectInfo
            Just connection ->
              pure connection
  pure SessionBackend {
    sbLoadSession = loadRedisSession conn sessionName mSessionDomain timeout overrideSession shouldSave shouldAddSessionKey
  }
