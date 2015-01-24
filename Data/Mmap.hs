module Data.Mmap
(
	fromList
)where


import qualified Data.Map as M
import Data.List


newtype MMap k v = MMap (M.Map k [v]) deriving Show


fromList :: (Eq k, Ord v, Ord k) => [(k,v)] -> MMap k v
fromList value = MMap $ M.fromList result
	where result = map(\x -> ((fst . head)  x, map(\y -> snd y) x)) (groupBy (\x y -> fst x == fst y) $ sort value)

getMap:: MMap k v => M.Map k [v]
getMap (MMap value) = value


keys:: MMap k v -> [k]
keys (MMap value) = M.keys value

lookup:: Ord k => MMap k v -> k -> Maybe [v]
lookup (MMap value) item = M.lookup item value
