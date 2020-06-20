{-# LANGUAGE BinaryLiterals #-}

module Data.Digest.DrunkenBishop
  ( -- * Drunken Bishop
    drunkenBishop,
    -- * Configurable Drunken Bishop
    DrunkenBishopOptions(..),
    drunkenBishopDefaultOptions,
    drunkenBishopWithOptions
  ) where

import Data.Array
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Digest.Pure.MD5
import Data.Word (Word8)

type Board = Array (Int, Int) Word8

mkBoard :: (Int, Int) -> Board
mkBoard (width, height) = array bound [(i, 0) | i <- range bound]
  where
    bound = ((0, 0), (width -1, height -1))

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

move :: (Int, Int) -> Dir -> (Int, Int) -> (Int, Int)
move (width, height) d (a, b) = snap (go d (a, b))
  where
    go UL (x, y) = (x - 1, y - 1)
    go UR (x, y) = (x + 1, y - 1)
    go DL (x, y) = (x - 1, y + 1)
    go DR (x, y) = (x + 1, y + 1)
    snap (x, y) = (clamp x 0 (width - 1), clamp y 0 (height - 1))

clamp :: Ord a => a -> a -> a -> a
clamp n low high
  | n < low = low
  | n > high = high
  | otherwise = n

-- | The default mapping from byte to ASCII character used by
-- OpenSSH. The intention of this mapping was to have characters of
-- increasing "noise" as the byte value goes up, with 15 and 16
-- instead mapping to special "begin" and "end" characters.
drunkenBishopOpenSSHCharMap :: Word8 -> Char
drunkenBishopOpenSSHCharMap n = case n of
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

-- | Once we've converted a hash into a sequence of two-bit 'Dir'
-- values, we can walk it, incrementing the values of board locations
-- as we pass them
runSteps :: (Int, Int) -> (Int, Int) -> [Dir] -> Board -> Board
runSteps _ pos [] b = b // [(pos, 16)]
runSteps size pos (d : ds) b
  | b ! pos == 15 = runSteps size newPos ds b
  | otherwise = runSteps size newPos ds (b // [(newPos, clamp ((b ! newPos) + 1) 0 14)])
  where
    newPos = move size d pos

-- | Convert a finalized board into a string
renderBoard :: DrunkenBishopOptions input -> Board -> String
renderBoard opts board =
  unlines
    [ foldr (:) "" [drunkenBishopCharMap opts (board ! (x, y)) | x <- [0 .. width -1]]
      | y <- [0 .. height -1]
    ]
  where
    (width, height) = drunkenBishopBoardSize opts

-- | A set of configuration options for specializing the Drunken
-- Bishop algorithm.
data DrunkenBishopOptions input = DrunkenBishopOptions
  { drunkenBishopHash :: input -> BS.ByteString,
    -- ^ The hashing function to use in order to convert an input
    -- value into a hash usable for a Drunken Bishop run.
    drunkenBishopBoardSize :: (Int, Int),
    -- ^ The board size used for a Drunken Bishop run.
    drunkenBishopInitialPosition :: Maybe (Int, Int),
    -- ^ The initial position of the bishop on the board. If this is
    -- 'Nothing', then the initial position will be @(width `div` 2,
    -- height `div` 2)@.
    drunkenBishopCharMap :: Word8 -> Char
    -- ^ The mapping from bytes to characters used for visualizing a
    -- Drunken Bishop run. The values @0x0@ through @0xE@ correspond
    -- to how many times the bishop has visited the cell in the course
    -- of a walk, while the values @0xE@ and @0xF@ are 'special' in
    -- that they represent the starting and ending position of the
    -- bishop, respectively.
  }

-- | The options used by the OpenSSH implementation of Drunken
-- Bishop. This uses the MD5 hash algorithm, starting the bishop at
-- the center of a a 17x9 grid, and uses [the character table
-- described here](http://www.dirk-loss.de/sshvis/drunken_bishop.pdf).
drunkenBishopDefaultOptions :: DrunkenBishopOptions BSL.ByteString
drunkenBishopDefaultOptions =
  DrunkenBishopOptions
    { drunkenBishopHash = md5DigestBytes . md5,
      drunkenBishopBoardSize = (17, 9),
      drunkenBishopInitialPosition = Nothing,
      drunkenBishopCharMap = drunkenBishopOpenSSHCharMap
    }

-- | Run the Drunken Bishop algorithm with the options chosen by
-- OpenSSH. See the documentation on 'drunkenBishopDefaultOptions' for
-- specifics on what those are.
drunkenBishop :: BSL.ByteString -> String
drunkenBishop = drunkenBishopWithOptions drunkenBishopDefaultOptions

-- | Run the Drunken Bishop algorithm with the provided set of options.
drunkenBishopWithOptions :: DrunkenBishopOptions input -> input -> String
drunkenBishopWithOptions opts bs = renderBoard opts finalBoard
  where
    (width, height) = drunkenBishopBoardSize opts
    initialBoard = mkBoard (width, height) // [(initialPosition, 15)]
    bishopPath = toDirections (drunkenBishopHash opts bs)
    finalBoard = runSteps (width, height) initialPosition bishopPath initialBoard
    initialPosition = case drunkenBishopInitialPosition opts of
      Nothing -> (width `div` 2, height `div` 2)
      Just pos -> pos
