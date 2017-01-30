module ToBytecode(toByecode) where

import Instructions
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Put
import Control.Monad

toBs :: [Instruction] -> Put
toBs [] = putWord8 0
toBs (x:xs) = do
    toB x
    toBs xs

pint :: Int -> Put
pint a = putInt32be . fromIntegral $ a

toB :: Instruction -> Put
toB (InstrJump a)
    = putWord8 1 >> pint a
toB (InstrConditionalJump a)
    = putWord8 2 >> pint a
toB (InstrFunctionDecl name args len)
    = do
        putWord8 3
        puts name
        putstrs args
        pint len
toB (InstrClassDecl name mem)
    = putWord8 4 >> puts name >> putstrs mem
toB (InstrVarLookup name)
    = putWord8 6 >> puts name
toB (InstrGlobalLookup name)
    = putWord8 7 >> puts name
toB (InstrPushConstStr str)
    = putWord8 8 >> puts str
toB (InstrPushConstInt int)
    = putWord8 9 >> pint int
toB (InstrObjNew name)
    = putWord8 12 >> puts name
toB a = putWord8 $
        case a of
            InstrReturn          -> 5
            InstrFunctionCall    -> 10
            InstrArrayAccess     -> 11
            InstrObjMemberAccess -> 13
            InstrAssign          -> 14
            InstrCompareEq       -> 15
            InstrCompareLt       -> 16
            InstrCompareGt       -> 17
            InstrCompareLeq      -> 18
            InstrCompareGeq      -> 19
            InstrArithPlus       -> 20
            InstrArithMinus      -> 21
            InstrArithMul        -> 22
            InstrArithDiv        -> 23
            InstrArithMod        -> 24
            InstrArithInc        -> 25
            InstrArithDec        -> 26
            InstrLogicNot        -> 27
            InstrLogicAnd        -> 28
            InstrLogicOr         -> 29
            InstrBlockEnter      -> 30
            InstrBlockLeave      -> 31
            InstrStackPop        -> 32

putstrs :: [String] -> Put
putstrs [] = putWord8 0
putstrs (x:xs) = puts x >> putstrs xs

puts :: String -> Put
puts a = putStringUtf8 a >> putWord8 0

toByecode :: [Instruction] -> BL.ByteString
toByecode a = runPut $ toBs a

main = BL.putStr . runPut . toB $ (InstrFunctionDecl "name" ["arg1","arg2","arg2"] 5)
