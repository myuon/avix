{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
-- from: https://gist.github.com/lotz84/5a5fd1aba842bbf111a7f5c548f40cfa
module Bookkeeper.JSON where

import Bookkeeper
import Bookkeeper.Internal (Book'(..))
import Data.Aeson (ToJSON(..), object, (.=))
import Data.Aeson.Types (Pair)
import qualified Data.Text as Text
import Data.Type.Map (Map(..), Mapping(..))
import GHC.TypeLits (KnownSymbol, Symbol)
import Data.Kind (Type)

instance ToPairs m => ToJSON (Book' m) where
  toJSON (Book m) = object $ toPairs m

class ToPairs (m :: [Mapping Symbol Type]) where
  toPairs :: Map m -> [Pair]

instance ToPairs '[] where
  toPairs _ = []

instance (KnownSymbol k, ToJSON v, ToPairs m) => ToPairs ((k :-> v) ': m) where
  toPairs (Ext k v m) = (Text.pack (show k) .= toJSON v) : toPairs m

class ToStrPairs (m :: [Mapping Symbol Type]) where
  toStrPairs :: Map m -> [(String, String)]

instance ToStrPairs '[] where
  toStrPairs _ = []

instance (KnownSymbol k, Show v, ToStrPairs m) => ToStrPairs ((k :-> v) : m) where
  toStrPairs (Ext k v m) = (show k, show v) : toStrPairs m

bookPairs :: ToStrPairs m => Book' m -> [(String, String)]
bookPairs (Book m) = toStrPairs m

