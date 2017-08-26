module Data.Exo.ExoFormat where

import Control.Lens
import Data.Extensible
import Data.Proxy
import GHC.TypeLits
import qualified Data.Text.Lazy as T


toPairs :: Forall (KeyValue KnownSymbol ((~) T.Text)) xs => Record xs -> [(T.Text, T.Text)]
toPairs r = henumerateFor (Proxy @(KeyValue KnownSymbol ((~) T.Text))) r (\mem -> (:) (T.pack $ symbolVal (proxyAssocKey mem) , r ^. itemAt mem)) []

unlinePairs :: [(T.Text, T.Text)] -> T.Text
unlinePairs = T.unlines . fmap (\(k,v) -> T.concat [stripUS k, "=", v]) where
  stripUS (T.uncons -> Just ('_', k)) = k
  stripUS k = k

-- |
-- > showt = pack . show
showt :: (Show a) => a -> T.Text
showt = T.pack . show

-- |
-- > showBin True = "1"
-- > showBin False = "0"
showBin :: Bool -> T.Text
showBin True = "1"
showBin False = "0"

-- |
-- > showOpt True = "1"
-- > showOpt False = ""
showOpt :: Bool -> T.Text
showOpt True = "1"
showOpt False = ""

class ExoFormat t where
  -- |
  -- Takes object index, data and returns formatted text.
  eformat :: Int -> t -> T.Text

  def :: t


