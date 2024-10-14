-- | A file cache to avoid reading and parsing the same file many times
module GF.Server.Cache (Cache,newCache,flushCache,expireCache,listCache,readCache,readCache') where

import Control.Concurrent.MVar
import Data.Map (Map)
import qualified Data.Map as Map 
import Data.Foldable as T(mapM_)
import Data.Maybe(mapMaybe)
import System.Directory (getModificationTime, canonicalizePath)
import System.FilePath (makeRelative)
import System.Mem(performGC)
import Data.Time (UTCTime,getCurrentTime,diffUTCTime)
--import Data.Time.Compat (toUTCTime)

data Cache a = Cache {
      cacheRoot :: FilePath,
      cacheLoad :: Maybe a -> FilePath -> IO a,
      cacheObjects :: MVar (Map FilePath (MVar (Maybe (FileInfo a))))
    }

type FileInfo a = (UTCTime,UTCTime,a) -- modification time, access time, contents

-- | Create a new cache that uses the given function to read and parse files
newCache :: FilePath -> (Maybe a -> FilePath -> IO a) -> IO (Cache a)
newCache root load = do
  objs <- newMVar Map.empty
  return (Cache { cacheRoot = root
                , cacheLoad = load
                , cacheObjects = objs
                })

-- | Forget all cached objects
flushCache :: Cache a -> IO ()
flushCache c = do modifyMVar_ (cacheObjects c) (const (return Map.empty))
                  performGC

-- | Forget cached objects that have been unused for longer than the given time
expireCache age c =
  do now <- getCurrentTime
     let expire object@(Just (_,t,_)) | diffUTCTime now t>age = return Nothing
         expire object = return object
     withMVar (cacheObjects c) (T.mapM_ (flip modifyMVar_ expire))
     performGC

-- | List currently cached files
listCache :: Cache a -> IO [(FilePath,UTCTime)]
listCache c =
  fmap (mapMaybe id) . mapM check . Map.toList =<< readMVar (cacheObjects c)
  where
    check (path,v) = maybe Nothing (Just . (,) path . fst3) `fmap` readMVar v
    fst3 (x,y,z) = x

-- | Lookup a cached object (or read the file if it is not in the cache or if
-- it has been modified)
readCache :: Cache a -> FilePath -> IO a
readCache c file = snd `fmap` readCache' c file

-- | Like 'readCache', but also return the last modification time of the file
readCache' :: Cache a -> FilePath -> IO (UTCTime,a)
readCache' c file = do
  file <- canonicalizePath file
  v <- modifyMVar (cacheObjects c)
                  (findEntry (makeRelative (cacheRoot c) file))
  modifyMVar v (readObject file)
  where
    -- Find the cache entry, inserting a new one if neccessary.
    findEntry file objs = case Map.lookup file objs of
                            Just v  -> return (objs,v)
                            Nothing -> do v <- newMVar Nothing
                                          return (Map.insert file v objs, v)

    -- Check time stamp, and reload if different than the cache entry
    readObject file m = do
      t'  <- getModificationTime file
      now <- getCurrentTime
      x' <- case m of
              Just (t,_,x) | t' == t   -> return x
                           | otherwise -> cacheLoad c (Just x) file
              _                        -> cacheLoad c Nothing  file
      return (Just (t',now,x'), (t',x'))