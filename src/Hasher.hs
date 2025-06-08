module Hasher (rollingHash) where

import qualified Data.ByteString as BS
import Data.Bits (xor, shiftL)

rollingHash :: BS.ByteString -> Integer
rollingHash = BS.foldl' (\h b -> (h `xor` fromIntegral b) `shiftL` 1) 0
