module Web.RedisSession (
		setSession, setSessionExpiring,
		getSession,
		newKey,
		Redis
	) where

import Database.Redis
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Network.Socket.Internal (PortNumber)
import System.Random 
import Control.Monad
import Control.Monad.Trans (liftIO)

setSessionExpiring :: Connection -> ByteString -> [(ByteString, ByteString)] -> Integer ->  IO ()
setSessionExpiring conn key values timeout = runRedis conn $ do
    del [key]
    forM_ values (\x -> hset key (fst x) (snd x))
    expire key timeout
    return ()

setSession :: Connection -> ByteString -> [(ByteString, ByteString)] ->  IO ()
setSession conn key values = runRedis conn $ do 
    oldsess <- liftIO $ getSession conn key
    let todelete = case oldsess of
                       Just o -> o
                       Nothing -> []
    hdel key $ map fst todelete --- ISNT PROPERLY DELETING KEYS PLS FIX PLS! ;_; trying to remove all the hkeys without refreshing the expiry in redis ---
    forM_ values (\x -> hset key (fst x) (snd x))
    return ()

getSession :: Connection -> ByteString -> IO (Maybe [(ByteString, ByteString)])
getSession conn key = runRedis conn $ do
    result <- hgetall key
    let output = case result of
                     (Right b) -> Just b
                     _ -> Nothing
    return output

newKey = do
         let n = 30
         gen <- newStdGen 
         let chars = ['0'..'9']++['a'..'z']++['A'..'B']
         let numbers = randomRs (0, (length chars - 1)) gen
         return $ BC.pack $ take n $ map (chars!!) numbers
