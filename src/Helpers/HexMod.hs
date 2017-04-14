module Helpers.HexMod( readHex ) where

import Data.Char

readHex :: String -> Integer
readHex [] = 0
readHex (x:xs) = readHexChar x * (16 ^ length xs) + readHex xs

readHexChar :: Char -> Integer
readHexChar a
    | isDigit a = read [a]
    | a >= 'a' && a <= 'f'
        = case a of
            'a' -> 10
            'b' -> 11
            'c' -> 12
            'd' -> 13
            'e' -> 14
            'f' -> 15
    | a >= 'A' && a <= 'F' = readHexChar . toLower $ a
readHexChar _ = error "invalid hex String"
