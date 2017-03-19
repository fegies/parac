module HexMod( readHex ) where

readHex :: String -> Integer
readHex [] = 0
readHex (x:xs) = (readHexChar x)*(16 ^ (length xs)) + readHex xs

readHexChar :: Char -> Integer
readHexChar a
    | a >= '0' && a <= '9' = read [a]
    | a >= 'a' && a <= 'f'
        = case a of
            'a' -> 10
            'b' -> 11
            'c' -> 12
            'd' -> 13
            'e' -> 14
            'f' -> 15
readHexChar a = error "invalid hex String"

toHex :: Integer -> String
toHex a
    | a <= 15 = [toHexChar a]
toHex a = toHex (a `div` 16) ++ [toHexChar (a `mod` 16)]

toHexChar :: Integer -> Char
toHexChar a
    = case a of
        0 -> '0'
        1 -> '1'
        2 -> '2'
        3 -> '3'
        4 -> '4'
        5 -> '5'
        6 -> '6'
        7 -> '7'
        8 -> '8'
        9 -> '9'
        10 -> 'a'
        11 -> 'b'
        12 -> 'c'
        13 -> 'd'
        14 -> 'e'
        15 -> 'f'
