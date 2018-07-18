module Data.IO.Bytestrings
( testFromChunk
) where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as S

-- creates lazy bytestring from a list of strict ones
testFromChunk :: B.ByteString
testFromChunk = B.fromChunks [S.pack [40, 41, 42], S.pack [43, 44, 45], S.pack [46, 47, 48]]