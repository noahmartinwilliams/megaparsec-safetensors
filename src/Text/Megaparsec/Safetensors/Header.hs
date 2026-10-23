module Text.Megaparsec.Safetensors.Header where

import Data.ByteString as BS
import Data.ByteString.Lazy.UTF8 as BSL
import Data.List.NonEmpty
import Data.Map
import Data.Set as Set
import Data.Word
import Text.Megaparsec
import Text.Megaparsec.Byte.Binary
import Text.Megaparsec.JSON
import Text.Megaparsec.Safetensors.Types

data DType = F64 | F32 | F16 | BF16 | I64 | I32 | I16 | I8 | U8 | DTBOOL deriving(Show, Eq)

data STTensorMeta = TensorMeta { sttmName :: String, sttmDType :: DType, sttmShape :: (Int, Int), sttmDataOffsets :: (Int, Int) }

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

{-
getTensorsMeta :: SafetParser [STTensorMeta]
getTensorsMeta = do
    shs <- safetHeaderSize
    json@(JSONObj json') <- safetHeader shs
    let names = keys json'
    getTensorsMeta' names json'
-}

--getTensorsMeta' :: [String] -> Map String JSONObj -> SafetParser [STTensorMeta]
--getTensorsMeta' [] _ = return []
--getTensorsMeta' (h : t) m = do
    --dt <- getDType h ((lookup h m) >>= (\(JSONObj x) -> lookup "dtype" x))

getDType :: String -> Maybe JSONObj -> SafetParser DType
getDType s Nothing = failure Nothing (Set.fromList [Label ('M' :| "issing key dtype in " ++ s ++ ".")])
getDType _ (Just (JSONString "F64")) = return F64
getDType _ (Just (JSONString "F32")) = return F32
getDType _ (Just (JSONString "F16")) = return F16
getDType _ (Just (JSONString "BF16")) = return BF16
getDType _ (Just (JSONString "I64")) = return I64
getDType _ (Just (JSONString "I32")) = return I32
getDType _ (Just (JSONString "I16")) = return I16
getDType _ (Just (JSONString "I8")) = return I8
getDType _ (Just (JSONString "U8")) = return U8
getDType _ (Just (JSONString "BOOL")) = return DTBOOL
getDType str j = failure Nothing (Set.fromList [Label ('I' :| "nvalid data type: " ++ (show j) ++ " for object: \"" ++ str ++ "\".")])
    

--getShape :: String -> MaybeJSONObj -> SafetParser (Int, Int)
