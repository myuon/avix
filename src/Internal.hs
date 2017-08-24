{-# LANGUAGE UndecidableInstances #-}
module Internal where

import Bookkeeper
import Bookkeeper.Internal (Book'(..))
import Control.Lens hiding (Empty)
import Data.Type.Map
import GHC.TypeLits (Symbol)
import Data.Kind (Type)

type family ShowBook (xs :: [Mapping Symbol Type]) where
  ShowBook xs = Book (ShowBookList xs)

type family ShowBookList (xs :: [Mapping Symbol Type]) where
  ShowBookList '[] = '[]
  ShowBookList ((x :-> v) : kvs) = (x :-> (v -> String)) : ShowBookList kvs


newtype Booked xs = Booked { getBooked :: (Book xs, ShowBook xs) }
makeWrapped ''Booked

_book :: Lens' (Booked xs) (Book xs)
_book = _Wrapped . _1

_strbook :: Lens' (Booked xs) (ShowBook xs)
_strbook = _Wrapped . _2

class UnzipMap xs ys zs | xs -> ys, xs -> zs where
  unzipMap :: Map xs -> (Map ys, Map zs)

instance UnzipMap '[] '[] '[] where
  unzipMap _ = (Empty, Empty)

instance UnzipMap kvs kvs1 kvs2 => UnzipMap ((k :=> (v1,v2)) : kvs) ((k :=> v1) : kvs1) ((k :=> v2) : kvs2) where
  unzipMap (Ext k (v1,v2) m) = let (x,y) = unzipMap m in (Ext k v1 x, Ext k v2 y)

unzipBook :: UnzipMap xs ys zs => Book' xs -> (Book' ys, Book' zs)
unzipBook (Book b) = let (x,y) = unzipMap b in (Book x, Book y)

