module BinHelpers
where

import Data.Binary
import Data.Binary.Put
import qualified Data.ByteString.Char8 as C
import Control.Applicative
import Data.Char

getWord8Int :: Get Int
getWord8Int = fromIntegral <$> getWord8

putWord8Int :: Int -> Put
putWord8Int = putWord8 . fromIntegral

getName :: C.ByteString -> String
getName = correctName . C.unpack . C.takeWhile (/= '\0')

putName :: Int -> String -> Put
putName n s = putBytes n (C.pack s)

putBytes :: Int -> C.ByteString -> Put
putBytes n s =
  let s'  = C.take n s
      pad = C.replicate (n - C.length s') '\0'
  in putByteString $ s' `C.append` pad

capitalize :: String -> String
capitalize []     = []
capitalize (h:hs) = toUpper h : hs

deCaps :: String -> String
deCaps = map toLower

correctName :: String -> String
correctName = unwords . map capitalize . words . deCaps

caps :: String -> String
caps = map toUpper


