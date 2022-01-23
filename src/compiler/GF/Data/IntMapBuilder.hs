-- | In order to build an IntMap in one pass, we need a map data structure with
-- fast lookup in both keys and values.
-- This is achieved by keeping a separate reversed map of values to keys during building.
module GF.Data.IntMapBuilder where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Tuple (swap)
import Prelude hiding (lookup)

data IMB a = IMB {
  intMap :: IntMap a,
  valMap :: HashMap a Int
}

-- | An empty IMB
empty :: (Eq a, Hashable a) => IMB a
empty = IMB {
  intMap = IntMap.empty,
  valMap = HashMap.empty
}

-- | An empty IntMap
emptyIntMap :: IntMap a
emptyIntMap = IntMap.empty

-- | Lookup a value
lookup :: (Eq a, Hashable a) => a -> IMB a -> Maybe Int
lookup a IMB { valMap = vm } = HashMap.lookup a vm

-- | Insert without any lookup
insert :: (Eq a, Hashable a) => a -> IMB a -> (Int, IMB a)
insert a IMB { intMap = im, valMap = vm } =
  let
    ix = IntMap.size im
    im' = IntMap.insert ix a im
    vm' = HashMap.insert a ix vm
    imb' = IMB { intMap = im', valMap = vm' }
  in
    (ix, imb')

-- | Insert only when lookup fails
insert' :: (Eq a, Hashable a) => a -> IMB a -> (Int, IMB a)
insert' a imb =
  case lookup a imb of
    Just ix -> (ix, imb)
    Nothing -> insert a imb

-- | Build IMB from existing IntMap
fromIntMap :: (Eq a, Hashable a) => IntMap a -> IMB a
fromIntMap im = IMB {
  intMap = im,
  valMap = HashMap.fromList (map swap (IntMap.toList im))
}

-- | Get IntMap from IMB
toIntMap :: (Eq a, Hashable a) => IMB a -> IntMap a
toIntMap = intMap
