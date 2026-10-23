module Text.Megaparsec.Safetensors.Header where

import Data.ByteString as BS
import Data.ByteString.Lazy.UTF8 as BSL
import Data.Word
import Text.Megaparsec
import Text.Megaparsec.Byte.Binary
import Text.Megaparsec.JSON
import Text.Megaparsec.Safetensors.Types

safetHeaderSize :: SafetParser Int
safetHeaderSize = do
    i <- word64le 
    return (fromIntegral i :: Int)

safetHeader :: Int -> SafetParser JSONObj
safetHeader headerSize = do
    json <- takeP (Just "json header")  headerSize
    sp <- getSourcePos
    let fname = sourceName sp
    let ret = runParser jsonDoc fname (BSL.toString json)
    case ret of 
        Left err -> fail (errorBundlePretty err)
        Right json' -> return json'

