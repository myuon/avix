module Main where

import Data.Exo
import Data.Extensible
import qualified Data.Text.Lazy.IO as T
import Control.Lens hiding ((...))

printExo :: Exedit -> IO ()
printExo ex = do
  T.putStrLn $ eformat 0 $ ex & _Wrapped . #length .~ 10
  T.putStrLn $ eformat 0 $ (def @TLObject) & _Wrapped . #object .~ embed (#figure @= def @Figure)

main :: IO ()
main = do
  printExo def

