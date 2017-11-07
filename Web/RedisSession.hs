module Web.RedisSession
  ( setSession
  , setSessionExpiring
  , getSession
  , newKey
  , Redis
  ) where

import           Control.Monad             (void)
import           Control.Monad.Trans       (liftIO)
import           Data.ByteString           (ByteString)
import qualified Data.ByteString.Char8     as BC
import qualified Data.List                 as L
import           Database.Redis
import           System.Random

setSessionExpiring :: Connection -> ByteString -> [(ByteString, ByteString)] -> Integer -> IO ()
setSessionExpiring conn key values timeout = runRedis conn $ do
    void $ del [key]
    void $ hmset key values
    void $ expire key timeout
    return ()

setSession :: Connection
           -> ByteString
           -> [(ByteString, ByteString)] -- ^ if this is ever empty then the expiration is removed
           -> IO ()
setSession conn key values = runRedis conn $ do
    oldsess <- liftIO $ getSession conn key
    let todelete =
          case oldsess of
            Just old ->
              let
                oldfields = fst <$> old
                newfields = fst <$> values
              in
                oldfields L.\\ newfields -- only delete fields that aren't in the new values
            Nothing ->
              []
    void $ hmset key values -- overwrites keys that exist already
    void $ hdel key todelete
    return ()

getSession :: Connection -> ByteString -> IO (Maybe [(ByteString, ByteString)])
getSession conn key = runRedis conn $ do
    result <- hgetall key
    return $ case result of
               Right b ->
                 Just b
               Left _ ->
                 Nothing

newKey :: IO ByteString
newKey = do
  gen <- newStdGen
  let n = 30
      chars = ['0'..'9']++['a'..'z']++['A'..'B']
      numbers = randomRs (0, length chars - 1) gen
  return $ BC.pack $ take n $ map (chars!!) numbers
