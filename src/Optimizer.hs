module Optimizer where

import AbsLatte
import Control.Monad.Identity

type Eval a = IO a

-- Utils --

isEmpty :: Stmt -> Eval Bool
isEmpty (BStmt (Block [])) = return True
isEmpty (BStmt (Block stmts)) =
    foldM (\b s -> isEmpty s >>= \v -> return (v && b)) True stmts
isEmpty Empty = return True
isEmpty stmt = return False

-- Optimization --

optimizeIntIntExp :: Expr -> Expr -> Expr -> (Integer -> Integer -> Integer) -> Eval Expr
optimizeIntIntExp _ (ELitInt n1) (ELitInt n2) f = return $ ELitInt $ f n1 n2
optimizeIntIntExp exp _ _ _ = return exp

optimizeIntIntBoolExp :: Expr -> Expr -> Expr -> (Integer -> Integer -> Bool) -> Eval Expr
optimizeIntIntBoolExp _ (ELitInt n1) (ELitInt n2) f =
    if f n1 n2 then return ELitTrue else return ELitFalse
optimizeIntIntBoolExp exp _ _ _ = return exp

optimizeExp :: Expr -> Eval Expr 
optimizeExp (EApp id exps) = do
    newExps <- (mapM optimizeExp exps)
    return (EApp id newExps)
optimizeExp (Neg exp) = do
    newExp <- optimizeExp exp
    case newExp of
        (ELitInt n) -> return $ ELitInt (-n)
        _ -> return $ (Neg newExp)
optimizeExp (Not exp) = do
    newExp <- optimizeExp exp
    case newExp of
        ELitTrue -> return ELitFalse
        ELitFalse -> return ELitTrue
        _ -> return $ Not newExp
optimizeExp (EMul exp1 mulop exp2) = do
    newExp1 <- optimizeExp exp1
    newExp2 <- optimizeExp exp2
    let exp = (EMul newExp1 mulop newExp2) in
        case mulop of
            Times -> optimizeIntIntExp exp newExp1 newExp2 (*)
            Div -> optimizeIntIntExp exp newExp1 newExp2 (div)
            Mod -> optimizeIntIntExp exp newExp1 newExp2 (mod)
optimizeExp (EAdd exp1 Plus exp2) = do
    newExp1 <- optimizeExp exp1
    newExp2 <- optimizeExp exp2
    let exp = (EAdd newExp1 Plus newExp2) in
        case newExp1 of
            (ELitInt _) -> optimizeIntIntExp exp newExp1 newExp2 (+)
            (EString s1) -> do
                case newExp2 of
                    (EString s2) -> return $ EString $ s1 ++ s2
                    _ -> return $ exp
            _ -> return exp
optimizeExp (EAdd exp1 Minus exp2) = do
    newExp1 <- optimizeExp exp1
    newExp2 <- optimizeExp exp2
    let exp = (EAdd newExp1 Minus newExp2) in
        optimizeIntIntExp exp newExp1 newExp2 (-)
optimizeExp (ERel exp1 relop exp2) = do
    newExp1 <- optimizeExp exp1
    newExp2 <- optimizeExp exp2
    let exp = (ERel newExp1 relop newExp2) in
        case relop of
            LTH -> optimizeIntIntBoolExp exp newExp1 newExp2 (<=)
            LE -> optimizeIntIntBoolExp exp newExp1 newExp2 (<)
            GTH -> optimizeIntIntBoolExp exp newExp1 newExp2 (>)
            GE -> optimizeIntIntBoolExp exp newExp1 newExp2 (>=)
            EQU -> case newExp1 of
                (ELitInt _) -> optimizeIntIntBoolExp exp newExp1 newExp2 (==)
                (ELitTrue) -> do
                    if newExp2 == ELitTrue then return ELitTrue
                    else if newExp2 == ELitFalse then return ELitFalse
                    else return exp
                (ELitFalse) -> do
                    if newExp2 == ELitFalse then return ELitTrue
                    else if newExp2 == ELitTrue then return ELitFalse
                    else return exp
                _ -> return exp
            NE -> case newExp1 of
                (ELitInt _) -> optimizeIntIntBoolExp exp newExp1 newExp2 (/=)
                (ELitTrue) -> do
                    if newExp2 == ELitTrue then return ELitFalse
                    else if newExp2 == ELitFalse then return ELitTrue
                    else return exp
                (ELitFalse) -> do
                    if newExp2 == ELitFalse then return ELitFalse
                    else if newExp2 == ELitTrue then return ELitTrue
                    else return exp
                _ -> return exp
optimizeExp (EAnd exp1 exp2) = do
    newExp1 <- optimizeExp exp1
    newExp2 <- optimizeExp exp2
    case newExp1 of
        ELitTrue -> do
            case newExp2 of
                ELitTrue -> return ELitTrue
                ELitFalse -> return ELitFalse
                _ -> return newExp2
        ELitFalse -> return ELitFalse
        _ -> return $ EAnd newExp1 newExp2
optimizeExp (EOr exp1 exp2) = do
    newExp1 <- optimizeExp exp1
    newExp2 <- optimizeExp exp2
    case newExp1 of
        ELitTrue -> return ELitTrue
        ELitFalse ->
            case newExp2 of
                ELitTrue -> return ELitTrue
                ELitFalse -> return ELitFalse
                _ -> return newExp2
        _ -> return $ EOr newExp1 newExp2
optimizeExp exp = return exp

optimizeDeclaration :: Item -> Eval Item
optimizeDeclaration (Init id exp) = do
    newExp <- optimizeExp exp
    return $ Init id newExp
optimizeDeclaration item = return item

optimizeStmts :: [Stmt] -> [Stmt] -> Eval [Stmt]
optimizeStmts (Empty:stmts) acc = optimizeStmts stmts acc
optimizeStmts (s@(BStmt block):stmts) acc = do
    empty <- isEmpty s
    if empty then
        optimizeStmts stmts acc
    else do
        newBlock <- optimizeBlock block
        optimizeStmts stmts $ acc ++ [(BStmt newBlock)]
optimizeStmts ((Decl typ items):stmts) acc = do
    newItems <- mapM optimizeDeclaration items
    optimizeStmts stmts $ acc ++ [(Decl typ newItems)] 
optimizeStmts ((Ass ident exp):stmts) acc = do
    newExp <- optimizeExp exp
    optimizeStmts stmts $ acc ++ [(Ass ident newExp)]
optimizeStmts ((Ret exp):stmts) acc = do
    newExp <- optimizeExp exp
    optimizeStmts stmts $ acc ++ [(Ret newExp)]
optimizeStmts ((Cond exp stmt):stmts) acc = do
    empty <- isEmpty stmt
    if empty then
        optimizeStmts stmts acc
    else do
        newExp <- optimizeExp exp
        s@[newStmt] <- optimizeStmts [stmt] []
        case newExp of
            ELitTrue -> do
                optimizeStmts stmts $ acc ++ s
            ELitFalse ->
                optimizeStmts stmts acc
            _ -> optimizeStmts stmts $ acc ++ [(Cond newExp newStmt)]
optimizeStmts ((CondElse exp stmt1 stmt2):stmts) acc = do
    empty1 <- isEmpty stmt1
    empty2 <- isEmpty stmt2
    if empty1 then
        optimizeStmts ((Cond (Not exp) stmt2):stmts) acc
    else if empty2 then
        optimizeStmts ((Cond exp stmt1):stmts) acc
    else do
        newExp <- optimizeExp exp
        s1@[newStmt1] <- optimizeStmts [stmt1] []
        s2@[newStmt2] <- optimizeStmts [stmt2] []
        case newExp of
            ELitTrue -> optimizeStmts stmts $ acc ++ s1
            ELitFalse -> optimizeStmts stmts $ acc ++ s2
            _ -> optimizeStmts stmts $ acc ++ [(CondElse newExp newStmt1 newStmt2)]
optimizeStmts ((While exp stmt):stmts) acc = do
    empty <- isEmpty stmt
    if empty then
        optimizeStmts stmts acc
    else do
        newExp <- optimizeExp exp
        [newStmt] <- optimizeStmts [stmt] []
        case newExp of
            ELitFalse -> optimizeStmts stmts acc
            _ -> optimizeStmts stmts $ acc ++ [(While newExp newStmt)]
optimizeStmts ((SExp exp):stmts) acc = do
    newExp <- optimizeExp exp
    case newExp of
        (ELitInt _) -> optimizeStmts stmts acc
        (ELitTrue) -> optimizeStmts stmts acc
        (ELitFalse) -> optimizeStmts stmts acc
        (EString str) -> optimizeStmts stmts acc
        _ -> optimizeStmts stmts $ acc ++ [(SExp newExp)]
optimizeStmts (stmt:stmts) acc = optimizeStmts stmts $ acc ++ [stmt]
optimizeStmts [] acc = return acc

removeEmptyBlocks :: [Stmt] -> [Stmt] -> Eval [Stmt]
removeEmptyBlocks ((BStmt (Block [])):stmts) acc = removeEmptyBlocks stmts acc
removeEmptyBlocks ((BStmt (Block ((BStmt block):s))):stmts) acc = 
    removeEmptyBlocks ([(BStmt block)] ++ s ++ stmts) acc
removeEmptyBlocks (stmt:stmts) acc = removeEmptyBlocks stmts $ acc ++ [stmt]
removeEmptyBlocks [] acc = return acc

optimizeBlock :: Block -> Eval Block
optimizeBlock (Block stmts) = do
    newStmts <- optimizeStmts stmts []
    resultStmts <- removeEmptyBlocks newStmts []
    return $ (Block resultStmts)

optimizeDefinition :: TopDef -> Eval TopDef
optimizeDefinition (FnDef typ ident args block) = do
    newBlock <- optimizeBlock block
    return $ FnDef typ ident args newBlock
optimizeDefinition (ClsDef id fields funcs) = do
    newFuncs <- mapM optimizeDefinition funcs
    return $ ClsDef id fields newFuncs
optimizeDefinition (ClsExtDef id baseCls fields funcs) = do
    newFuncs <- mapM optimizeDefinition funcs
    return $ ClsExtDef id baseCls fields funcs

optimizeProgram :: Program -> Eval Program
optimizeProgram (Program defs) = do
    newDefs <- mapM optimizeDefinition defs
    return $ Program newDefs

optimizeLatteProgram :: Program -> IO Program
optimizeLatteProgram prog = optimizeProgram prog
