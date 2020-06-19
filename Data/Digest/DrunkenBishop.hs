{-# LANGUAGE BinaryLiterals #-}

module Data.Digest.DrunkenBishop (drunkenBishop) where

import Data.Array
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Digest.Pure.MD5
import Data.Word (Word8)

type Board = Array (Int, Int) Int

initialPosition :: (Int, Int)
initialPosition = (8, 4)

mkBoard :: Board
mkBoard = array bound [(i, 0) | i <- range bound]
  where
    bound = ((0, 0), (16, 8))

toDirections :: BS.ByteString -> [Dir]
toDirections bs = case BS.uncons bs of
  Just (x, xs) ->
    toDir (x `shift` (-6))
      : toDir (x `shift` (-4))
      : toDir (x `shift` (-2))
      : toDir x
      : toDirections xs
  Nothing -> []

data Dir = UL | UR | DL | DR deriving (Eq, Show)

toDir :: Word8 -> Dir
toDir x = go (x .&. 0b11)
  where
    go 0b00 = UL
    go 0b01 = UR
    go 0b10 = DL
    go 0b11 = DR
    go _ = error "unreachable"

move :: Dir -> (Int, Int) -> (Int, Int)
move d (a, b) = snap (go d (a, b))
  where
    go UL (x, y) = (x -1, y -1)
    go UR (x, y) = (x + 1, y -1)
    go DL (x, y) = (x -1, y + 1)
    go DR (x, y) = (x + 1, y + 1)
    snap (x, y) = (clamp x 0 16, clamp y 0 8)

clamp :: Ord a => a -> a -> a -> a
clamp n low high
  | n < low = low
  | n > high = high
  | otherwise = n

toChar :: Int -> Char
toChar n = case n of
  00 -> ' '
  01 -> '.'
  02 -> 'o'
  03 -> '+'
  04 -> '='
  05 -> '*'
  06 -> 'B'
  07 -> 'O'
  08 -> 'X'
  09 -> '@'
  10 -> '%'
  11 -> '&'
  12 -> '#'
  13 -> '/'
  14 -> '^'
  15 -> 'S'
  16 -> 'E'
  _ -> '?'

runSteps :: (Int, Int) -> [Dir] -> Board -> Board
runSteps pos [] b = b // [(pos, 16)]
runSteps pos (d : ds) b =
  let newPos = move d pos
   in if b ! pos == 15
        then runSteps newPos ds b
        else runSteps newPos ds (b // [(newPos, clamp ((b ! newPos) + 1) 0 14)])

drunkenBishop :: BSL.ByteString -> String
drunkenBishop bs = render (runSteps initialPosition (toDirections h) mkBoard // [((8, 4), 15)])
  where
    render b =
      unlines
        [ foldr (:) "" [toChar (b ! (x, y)) | x <- [0 .. 16]]
          | y <- [0 .. 8]
        ]
    h = md5DigestBytes (md5 bs)
