module AstTransform(normaliseAst,transformToInstructions) where

import Ast
import Instructions

normaliseAst :: Program -> Program
normaliseAst = checkReturnStatement . normaliseStatements . checkTree

--throw errors if an error in the grammar is found
--returns the exact same AST if no errors were found
checkTree :: Program -> Program
checkTree (p @(StatementForTo (ExpressionAssign _ _) _ _) : xs) = p : checkTree xs
checkTree (p @(StatementForTo _ _ _) : xs) = error "Deine For-Schleife ist behindert. Streng dich mehr an."
checkTree (p @(StatementForDownto (ExpressionAssign _ _) _ _) : xs) = p : checkTree xs
checkTree (p @(StatementForDownto _ _ _) : xs) = error "Deine For-Schleife ist behindert. Streng dich mehr an."
checkTree (p @(StatementExpression e):xs) = (StatementExpression $ checkExpression e) : checkTree xs
checkTree a = a

--thows errors if a bad expression is found
--returns the same expression otherwise
checkExpression :: Expression -> Expression
checkExpression a = a


normaliseStatements :: [Statement] -> [Statement]
normaliseStatements [] = []
normaliseStatements (x:xs) = normaliseStatement x ++ normaliseStatements xs


--turn all loops into while loops
normaliseStatement :: Statement -> [Statement]
normaliseStatement (StatementForTo (ExpressionAssign to from) expc block)
    = StatementExpression (ExpressionAssign to from)
    : StatementWhile (ExpressionCompareLt to expc) (block++[StatementExpression $ ExpressionArithInc to])
    : []
normaliseStatement (StatementForDownto (ExpressionAssign to from) expc block)
    = StatementExpression (ExpressionAssign to from)
    : StatementWhile (ExpressionCompareGt to expc) (block++[StatementExpression $ ExpressionArithDec to])
    : []
normaliseStatement (StatementExpression exp)
    = (StatementExpression $ normaliseExpression exp) : []
normaliseStatement (StatementFunctionDeclaration name args block)
    = StatementFunctionDeclaration name args 
        (checkReturnStatement . normaliseStatements $ block) : []
normaliseStatement (StatementIf exp th el)
    = StatementIf (normaliseExpression exp) (normaliseStatements th) (normaliseStatements el) : []
normaliseStatement a = [a]

--checks if a block ends with a return statement
--if it does not, adds one to the end.
checkReturnStatement :: [Statement] -> [Statement]
checkReturnStatement [] = []
checkReturnStatement (r @(StatementReturn _) : [] ) = r : []
checkReturnStatement (x:[]) = x : (StatementReturn EmptyExpression) : []
checkReturnStatement (x:xs) = x : checkReturnStatement xs

normaliseExpression :: Expression -> Expression
normaliseExpression (ExpressionCompareNeq e1 e2) = ExpressionLogicNot( ExpressionCompareEq e1 e2 )
normaliseExpression a = a

transformToInstructions :: Block -> [Instruction]
transformToInstructions [] = []
transformToInstructions (x:xs) = (serializeStatement x)++transformToInstructions xs

serializeStatement :: Statement -> [Instruction]
serializeStatement (StatementIf expc bthen belse )
    = let ci = serializeExpression expc
          ei = InstrBlockEnter : transformToInstructions belse ++ [InstrBlockLeave]
          ti = InstrBlockEnter : transformToInstructions bthen ++ 
            [InstrBlockLeave,InstrJump (length ei)]
      in ci ++ [InstrConditionalJump (length ti)] ++ ti ++ ei
serializeStatement (StatementWhile exp block)
    = let ci = serializeExpression exp
          bi = InstrBlockEnter : transformToInstructions block ++ [InstrBlockLeave]
          jf = length bi + 1 -- +1 because of the jump statement on the end
          jb = - (jf + length ci + 1)
      in ci ++ [InstrConditionalJump jf] ++ bi ++ [InstrJump jb]
serializeStatement (StatementRepeat block expr)
    = let bi = InstrBlockEnter : transformToInstructions block ++ [InstrBlockLeave]
          ei = serializeExpression expr
          jb = -(length bi + length ei)
      in bi ++ ei ++ [InstrConditionalJump jb]
serializeStatement (StatementFunctionDeclaration name args block)
    = let ins = transformToInstructions block
          len = length ins
      in (InstrFunctionDecl name args len) : ins
serializeStatement (StatementClassDeclaration name args)
    = InstrClassDecl name args : []
serializeStatement (StatementReturn exp)
    = serializeExpression exp ++ [InstrReturn]
serializeStatement (StatementExpression exp)
    = serializeExpression exp ++ [InstrStackPop]

argstolist :: Expression -> Expression -> [Instruction]
argstolist a b = serializeExpression b ++ serializeExpression a

serializeExpression :: Expression -> [Instruction]
serializeExpression EmptyExpression = []
serializeExpression (ExpressionVar name) = InstrVarLookup name : []
serializeExpression (ExpressionConstant const)
    = case const of
        ConstantString s -> InstrPushConstStr s
        ConstantInt i -> InstrPushConstInt $ fromIntegral i
      : []
serializeExpression (ExpressionFunctionCall exp args)
    = let l = case exp of
                (ExpressionVar s) -> [InstrGlobalLookup s]
                a -> serializeExpression a
          r = concat $ map serializeExpression args
      in r ++ l ++ [InstrFunctionCall]
serializeExpression (ExpressionObjectNew cla)
    = InstrObjNew cla : []
serializeExpression (ExpressionObjectMembAccess obj member)
    = serializeExpression obj ++ [InstrPushConstStr member] ++ [InstrObjMemberAccess]
serializeExpression (ExpressionArrayAccess exp pos)
    = argstolist exp pos ++ [InstrArrayAccess]
serializeExpression (ExpressionAssign to what)
    = argstolist what to ++ [InstrAssign]

serializeExpression (ExpressionCompareEq l r )
    = argstolist l r ++ [InstrCompareEq]
serializeExpression (ExpressionCompareLt l r )
    = argstolist l r ++ [InstrCompareLt]
serializeExpression (ExpressionCompareLeq l r )
    = argstolist l r ++ [InstrCompareLeq ]
serializeExpression (ExpressionCompareGt l r )
    = argstolist l r ++ [InstrCompareGt]
serializeExpression (ExpressionCompareGeq l r )
    = argstolist l r ++ [InstrCompareGeq]

serializeExpression (ExpressionArithPlus l r )
    = argstolist l r ++ [InstrArithPlus]
serializeExpression (ExpressionArithMinus l r)
    = argstolist l r ++ [InstrArithMinus]
serializeExpression (ExpressionArithMul l r)
    = argstolist l r ++ [InstrArithMul]
serializeExpression (ExpressionArithDiv l r)
    = argstolist l r ++ [InstrArithDiv]
serializeExpression (ExpressionArithMod l r)
    = argstolist l r ++ [InstrArithMod]
serializeExpression (ExpressionArithInc e)
    = serializeExpression e ++ [InstrArithInc]
serializeExpression (ExpressionArithDec e)
    = serializeExpression e ++ [InstrArithDec]

serializeExpression (ExpressionLogicAnd l r)
    = argstolist l r ++ [InstrLogicAnd]
serializeExpression (ExpressionLogicOr l r)
    = argstolist l r ++ [InstrLogicOr]
serializeExpression (ExpressionLogicNot e)
    = serializeExpression e ++ [InstrLogicNot]
