To use add the following to your Foundation.hs:

```haskell
instance Yesod App where
     -- Store session data in redis with a key in a cookie clientside
     makeSessionBackend _ = do
         backend <- redisSessionBackend
         return $ Just backend  
```
