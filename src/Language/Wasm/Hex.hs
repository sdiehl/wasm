module Language.Wasm.Hex (
  simpleHex
) where

import qualified Data.ByteString as BS
import Data.List hiding (group)

import Numeric (showHex)

byteWidth    = 2
numWordBytes = 4

paddedShowHex :: (Show a, Integral a) => Int -> a -> String
paddedShowHex w n = pad ++ str
    where
     str = showHex n ""
     pad = replicate (w - length str) '0'

group :: Int -> [a] -> [[a]]
group n
 | n <= 0    = (:[])
 | otherwise = unfoldr go
  where
    go [] = Nothing
    go xs = Just (splitAt n xs)

simpleHex :: BS.ByteString -> String
simpleHex = intercalate "  "
          . map unwords . group numWordBytes
          . map (paddedShowHex byteWidth)
          . BS.unpack
