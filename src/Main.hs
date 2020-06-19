module Main where

import qualified Data.ByteString.Lazy as BS
import Data.Digest.DrunkenBishop

main :: IO ()
main = do
  f <- BS.getContents
  putStrLn "+-----------------+"
  mapM_
    putStrLn
    [ "|" ++ ln ++ "|"
      | ln <- lines (drunkenBishop f)
    ]
  putStrLn "+-----------------+"
