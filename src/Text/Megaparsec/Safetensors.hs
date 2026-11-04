{-# LANGUAGE FlexibleContexts #-}
module Text.Megaparsec.Safetensors(
    module Text.Megaparsec.Safetensors.Header,
    module Text.Megaparsec.Safetensors.Tensors,
    module Text.Megaparsec.Safetensors.Types,
    module Data.ByteString.Lazy.UTF8,
    STTensors(..),
    safetensors
) where

import Data.Array.Accelerate as A
import Data.Array.Accelerate.Matrix
import Data.ByteString.Lazy.UTF8
import Data.List
import Data.Map
import Text.Megaparsec.Safetensors.Header
import Text.Megaparsec.Safetensors.Tensors
import Text.Megaparsec.Safetensors.Types
import Prelude as P

data STTensors f a b = STTensors { sttensorMeta :: [STTensorMeta], sttensorMats :: Map String (AccMat f a b)}

instance P.Ord STTensorMeta where
    compare (STTensorMeta { sttmDataOffsets = (a, _)}) (STTensorMeta { sttmDataOffsets = (b, _)}) | a P.> b = GT
    compare (STTensorMeta { sttmDataOffsets = (a, _)}) (STTensorMeta { sttmDataOffsets = (b, _)}) | a P.< b = LT
    compare (STTensorMeta { sttmDataOffsets = (a, _)}) (STTensorMeta { sttmDataOffsets = (b, _)}) | a P.== b = EQ

safetensors :: (P.RealFloat f, A.Floating f) => a -> b -> SafetParser (STTensors f a b)
safetensors a b = do
    meta <- tensorsMeta
    let sorted = sort meta
    tensors <- intern a b sorted
    let names = P.map sttmName sorted
    return (STTensors { sttensorMeta = sorted, sttensorMats = Data.Map.fromList (P.zip names tensors)}) where
        intern :: (P.RealFloat f, A.Floating f) => a -> b -> [STTensorMeta] -> SafetParser [AccMat f a b]
        intern _ _ [] = return []
        intern a b (h : t) = do
            tensor <- safeTensorMat h a b
            rest <- intern a b t
            return (tensor : rest)


