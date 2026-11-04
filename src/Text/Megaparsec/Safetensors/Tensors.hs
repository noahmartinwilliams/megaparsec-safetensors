{-# LANGUAGE RankNTypes, FlexibleContexts, ScopedTypeVariables #-}
module Text.Megaparsec.Safetensors.Tensors where

import Data.Array.Accelerate as A
import Data.Array.Accelerate.Matrix
import Data.Binary.IEEE754 as B
import Text.Megaparsec
import Text.Megaparsec.Byte
import Text.Megaparsec.Byte.Binary
import Text.Megaparsec.Byte.Lexer
import Text.Megaparsec.Safetensors.Header
import Text.Megaparsec.Safetensors.Types
import Prelude as P

safeTensorMatF64 :: forall f a b. (P.RealFloat f, A.Elt f, A.Floating f) => Int -> Int -> Int -> Int -> a -> b -> SafetParser (AccMat f a b) 
safeTensorMatF64 offset end m n a b = do
    list <- intern offset end
    return (AccMat (use (A.fromList (Z:.m:.n) list )) a b) where
        
        intern :: (P.Fractional f, A.Floating f) => Int -> Int -> SafetParser [f]
        intern o e | o P.== e = return []
        intern o e = do
            fl <- word64le
            fll <- intern (o + 8) e
            return ((P.realToFrac (B.wordToDouble fl)) : fll)

safeTensorMatF32 :: forall f a b. (P.RealFloat f, A.Elt f, A.Floating f) => Int -> Int -> Int -> Int -> a -> b -> SafetParser (AccMat f a b)
safeTensorMatF32 offset end m n a b = do
    l <- intern offset end
    return (AccMat (use (A.fromList (Z:.m:.n) l)) a b) where

        intern :: (P.Fractional f, A.Floating f) => Int -> Int -> SafetParser [f]
        intern o e | o P.== e = return []
        intern o e = do
            fl <- word32le
            fll <- intern (o + 8) e
            return ((P.realToFrac (B.wordToFloat fl)) : fll)

safeTensorMat :: forall f a b. (P.RealFloat f, A.Floating f, P.Fractional f ) => STTensorMeta -> a -> b -> SafetParser (AccMat f a b)
safeTensorMat (STTensorMeta { sttmShape = (m, n), sttmDataOffsets = (offset, end), sttmDType = F64}) a b = safeTensorMatF64 offset end m n a b
safeTensorMat (STTensorMeta { sttmShape = (m, n), sttmDataOffsets = (o, e), sttmDType = F32}) a b = safeTensorMatF32 o e m n a b
