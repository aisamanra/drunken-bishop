module Main (main) where

import Crypto.Hash.SHA256 (hashlazy)
import qualified Data.ByteString.Lazy as BS
import Data.Digest.DrunkenBishop
import System.Console.GetOpt (ArgDescr (..), ArgOrder (..), OptDescr (..), getOpt)
import System.Environment (getArgs)

readSize :: String -> (Int, Int)
readSize str = case reads str of
  [(x, 'x' : rest)] -> case reads rest of
    [(y, "")] -> (x, y)
    _ -> error msg
  _ -> error msg
  where
    msg = "Unable to parse " ++ str ++ " as an [Int]x[Int] pair"

options :: [OptDescr (DrunkenBishopOptions BS.ByteString -> DrunkenBishopOptions BS.ByteString)]
options =
  [ Option
      ['s']
      ["size"]
      (ReqArg (\o opts -> opts {drunkenBishopBoardSize = readSize o}) "[WIDTH]x[HEIGHT]")
      "The size of the art fingerprint",
    Option
      ['p']
      ["position"]
      (ReqArg (\o opts -> opts {drunkenBishopInitialPosition = Just (readSize o)}) "[X]x[Y]")
      "The initial position of the bishop",
    Option
      []
      ["sha256"]
      (NoArg (\opts -> opts {drunkenBishopHash = hashlazy}))
      "Use SHA256 instead of MD5 for the source hash"
  ]

main :: IO ()
main = do
  argv <- getArgs
  let (opts, files) = case getOpt Permute options argv of
        (fs, args, []) -> (foldr ($) drunkenBishopDefaultOptions fs, args)
        (_, _, errs) -> error (unlines errs)
  f <- case files of
    [] -> BS.getContents
    _ -> mconcat (map BS.readFile files)
  putStrLn "+-----------------+"
  mapM_
    putStrLn
    [ "|" ++ ln ++ "|"
      | ln <- lines (drunkenBishopWithOptions opts f)
    ]
  putStrLn "+-----------------+"
