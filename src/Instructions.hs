module Instructions where

data Instruction
    = InstrJump Int --jumps give the number of the node where execution should resume
    | InstrConditionalJump Int --jump if false
    | InstrFunctionDecl String [String] Int --number of instructions in the function block
    | InstrClassDecl String [String]
    | InstrReturn
    | InstrVarLookup String
    | InstrGlobalLookup String
    | InstrPushConstStr String
    | InstrPushConstInt Int
    | InstrFunctionCall
    | InstrArrayAccess
    | InstrObjNew String
    | InstrObjMemberAccess
    | InstrAssign

    | InstrCompareEq
    | InstrCompareLt
    | InstrCompareGt
    | InstrCompareLeq
    | InstrCompareGeq

    | InstrArithPlus
    | InstrArithMinus
    | InstrArithMul
    | InstrArithDiv
    | InstrArithMod
    | InstrArithInc
    | InstrArithDec

    | InstrLogicNot
    | InstrLogicAnd
    | InstrLogicOr

    | InstrBlockEnter   --these are used to implement scoping
    | InstrBlockLeave

    | InstrStackPop --used to discard the to element of the stack.

    | InstrLiteral Integer --used by the inline assembler, just puts the value of the Instruction in there.

    | InstrLoad String --used to load other files
    deriving (Show)



