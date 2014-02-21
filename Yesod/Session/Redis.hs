{-# LANGUAGE OverloadedStrings #-}
module Yesod.Session.Redis (
        redisSessionBackend
	) where

import qualified Web.RedisSession as R
import Yesod.Core
import Yesod.Core.Types
import qualified Network.Wai as W
import Web.Cookie
import Control.Monad.Trans (liftIO)
import Data.Time (UTCTime, addUTCTime, getCurrentTime)
import qualified Data.Text as T
import Data.ByteString (ByteString)
import Database.Redis
import Network.Socket.Internal (PortNumber)
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as BC
import Data.Maybe

sessionName = "yesodSession"
timeout = 10

loadRedisSession :: Connection -> Integer -> W.Request -> IO (SessionMap, SaveSession)
loadRedisSession conn timeout req = do
        let val = do
            raw <- lookup "Cookie" $ W.requestHeaders req
            lookup sessionName $ parseCookies raw
        key <- case val of
            Nothing -> R.newKey
            Just k -> return k
        sess <- case val of
                        Nothing -> return M.empty
                        Just s -> do 
                                     result <- R.getSession conn s
                                     return $ M.fromList $ map (\x -> ((T.pack . BC.unpack . fst) x, snd x)) $ fromJust result
        let save = saveRedisSession conn timeout key
        return (sess, save)

saveRedisSession :: Connection -> Integer -> ByteString -> SessionMap -> IO [Header]
saveRedisSession conn timeout key sess = do
    now <- getCurrentTime
    let expires = fromIntegral (timeout * 60) `addUTCTime` now
    R.setSessionExpiring conn key (map (\x -> ((BC.pack . T.unpack . fst) x, snd x)) (M.toList sess)) timeout
    return [AddCookie def {
            setCookieName = sessionName,
            setCookieValue = key,
            setCookiePath = Just "/",
            setCookieExpires = Just expires,
            setCookieDomain = Nothing,
            setCookieHttpOnly = True
        }]


redisSessionBackend :: IO SessionBackend
redisSessionBackend  = do
	conn <- connect defaultConnectInfo
	return SessionBackend {
		sbLoadSession = loadRedisSession conn 20
	}
