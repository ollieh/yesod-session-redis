To use add the following to your Foundation.hs:

```haskell
instance Yesod App where
     -- Store session data in redis with a key in a cookie clientside
     makeSessionBackend _ = do
      let conn = Nothing -- ^ just use the default connection
          name = "SESSION_NAME"
          domain = Nothing -- ^ just use whatever domain the request comes in on
          expireTime = fromIntegral $ (60 * 120 :: Int) -- ^ for 120 minutes
      backend <- redisSessionBackend conn name domain expireTime
      return $ Just backend
```

## Using your own Redis Connection

to have a redis connection available in other places you could put it in your App by adding it to the App definition in Foundation.hs then update the makeSessionBackend definition to use it

### Foundation.hs

```haskell
import qualified Database.Redis as R

...

data App = App
    { appSettings        :: !AppSettings
    , appRedisConn       :: !R.Connection -- ^ this is actually a pool of connections
    }

...

instance Yesod App where
     -- Store session data in redis with a key in a cookie clientside
     makeSessionBackend app = do
      let conn = Just $ appRedisConn app -- ^ use this connection for sessions
          name = "[SESSION_NAME]" -- ^ change the session name here
          domain = Nothing -- ^ just use whatever domain the request comes in on
          expireTime = fromIntegral $ (60 * 120 :: Int) -- ^ for 120 minutes
      backend <- redisSessionBackend conn name domain expireTime
      return $ Just backend
```

### Application.hs

```haskell
import qualified Database.Redis as R

...

makeFoundation :: AppSettings -> IO App
makeFoundation appSettings = do

    ...

    -- Setup Redis Connection Pool
    -- NOTE: this doesn't attempt to connect until the first use of the connection, any exceptions will occur there.
    appRedisConn <- R.connect $ R.defaultConnectInfo { R.connectHost = "[REDIS IP ADDRESS]" } -- ^ change the IP address here

    let mkFoundation appConnPool = App {..}

    ...
```
