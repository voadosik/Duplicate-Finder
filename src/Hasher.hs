module Hasher (rollingHash) where

import qualified Data.ByteString as BS
import Data.Bits (xor, shiftL)

-- Compute a rolling hash of a file's contents
-- XOR current byte with accumulated hash, shifts left
-- Not strong, but fast
rollingHash :: BS.ByteString -> Integer
rollingHash = BS.foldl' (\h b -> (h `xor` fromIntegral b) `shiftL` 1) 0
