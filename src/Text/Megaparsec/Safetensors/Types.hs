module Text.Megaparsec.Safetensors.Types where

import Data.ByteString.Lazy.UTF8 as BSL
import Data.Void
import Text.Megaparsec

type SafetParser = Parsec Void BSL.ByteString
