module SemanticAnalyzer where

import AbsLatte
import PrintLatte
import qualified Data.Map as Map
import Control.Monad.Reader
import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.State
import Utils (showStr, thisId)
import Data.Maybe (fromJust)
import SymbolicTable

data VarInfo = VarInfo Type Bool
data ProcInfo = FuncInfo Ident Type Bool | StmtInfo Stmt | ClassInfo Ident | EmptyInfo
data Env = Env (Map.Map Ident VarInfo) (ProcInfo, ProcInfo, ProcInfo)
type Eval a = ReaderT Env (ErrorT String (StateT SymTable IO)) a

-- Utils --

emptyEnv :: Env
emptyEnv = Env Map.empty (EmptyInfo, EmptyInfo, EmptyInfo)

insertVar :: Ident -> Type -> Eval Env
insertVar id t = do
    (Env vars i) <- ask
    case t of
        Void -> throwCtxErr invalidDeclErr
        (Array Void) -> throwCtxErr invalidDeclErr
        (Array (Array _)) -> throwCtxErr invalidDeclErr
        _ -> do
            classExists t
            var <- findVar id
            case var of
                (Just (VarInfo _ shadowed)) -> 
                    if shadowed then
                        throwCtxErr dupVarErr
                    else
                        return $ Env (Map.insert id (VarInfo t True) vars) i
                (Nothing) -> return $ Env (Map.insert id (VarInfo t True) vars) i
    where
        invalidDeclErr = "Invalid variable declaration type: " ++ showStr t ++ " " ++ showStr id
        dupVarErr = "Duplicate variable name: " ++ showStr id

findVar :: Ident -> Eval (Maybe VarInfo)
findVar id = do
    (Env vars _) <- ask 
    return $ Map.lookup id vars

setFunction :: Ident -> Type -> Eval Env
setFunction id typ = do
    (Env v (_, s, c)) <- ask
    return $ Env v ((FuncInfo id typ False), s, c)

setStatement :: Stmt -> Eval Env
setStatement stmt = ask >>= \(Env v (i,_, c)) -> return $ Env v (i, (StmtInfo stmt), c)

setClass :: Ident -> Eval Env
setClass id = ask >>= \(Env v (i, s, _)) -> return $ Env v (i, s, (ClassInfo id))

setReturnFound :: Type -> Eval Env
setReturnFound t = do
    tab <- get
    env@(Env v ((FuncInfo id rt b), s, c)) <- ask
    when b $ throwCtxErr tooManyRetErr
    unless (compareTypes tab t rt) $ throwCtxErr invalidRetType
    return $ Env v ((FuncInfo id rt True), s, c)
    where
        tooManyRetErr = "Too many return statements"
        invalidRetType = "Invalid return type: " ++ showStr t

setReturn :: Bool -> Eval Env
setReturn ret = do
    (Env v ((FuncInfo id t _), s, c)) <- ask
    return $ Env v ((FuncInfo id t ret), s, c)

getReturn :: Eval Bool
getReturn = do
    (Env _ ((FuncInfo _ _ b), _, _)) <- ask
    return b

clearVars :: Eval Env
clearVars = do
    (Env vars info) <- ask
    return $ Env (Map.map (transform) (vars)) info
    where
        transform = \(VarInfo typ _) -> (VarInfo typ False)

checkForMain :: Eval ()
checkForMain = do
    tab <- get
    case findFunction (Ident "main") tab of
        (Just (Fun Int [])) -> return ()
        (Just _) -> throwError $ "Invalid main definition"
        (Nothing) -> throwError $ "Main method not defined"

classExists :: Type -> Eval ()
classExists (Class id) = do
    tab <- get
    case findClass id tab of
        (Just _) -> return ()
        (Nothing) -> throwCtxErr $ "Class " ++ (showStr id) ++ " not found"
classExists _ = return ()

checkForReturn :: Eval ()
checkForReturn = do
    env@(Env _ ((FuncInfo _ t b), _, _)) <- ask
    when (not b && t /= Void) $ throwCtxErr "Missing return statement"

checkType :: [Type] -> Expr -> Eval Type
checkType types exp = do
    tab <- get
    t <- analyzeExp exp
    let invalidTypesErr = "Invalid type, actual: " ++ showStr t ++ " expected: " ++ showStr types in do
        case filter (compareTypes tab t) types of
            [] -> throwCtxErr invalidTypesErr 
            [tt] -> return t

checkTypes :: [Type] -> Expr -> Expr -> Eval Type
checkTypes types exp1 exp2 = do
    tab <- get
    t1 <- analyzeExp exp1
    t2 <- analyzeExp exp2
    if compareTypes tab t1 t2 then do
        case filter (compareTypes tab t1) types of
            [] -> throwCtxErr $ "Invalid type in expression" ++ showStr t1
            [t] -> return t
    else
        throwCtxErr $ "Incompatibile types: " ++ showStr t1 ++ ", " ++ showStr t2

getContextClassInfo :: Env -> Eval String
getContextClassInfo (Env _ (_,_,c)) =
    case c of
        (ClassInfo id) -> return $ "In class: " ++ showStr id ++ "\n"
        _ -> return ""

getContextFunInfo :: Env -> Eval String
getContextFunInfo (Env _ (f,_,_)) =
    case f of
        (FuncInfo id _ _) -> return $ "In function: " ++ showStr id ++ "\n"
        _ -> return ""

getContextStmtInfo :: Env -> Eval String
getContextStmtInfo (Env _ (_,s,_)) =
    case s of
        (StmtInfo s) -> return $ "In statement: " ++ printTree s
        _ -> return ""

throwCtxErr :: String -> Eval a
throwCtxErr err = do
    env <- ask
    clsInfo <- getContextClassInfo env
    funInfo <- getContextFunInfo env
    stmtInfo <- getContextStmtInfo env
    throwError $ clsInfo ++ funInfo ++ stmtInfo ++ "Error: " ++ err

-- Analysis --

analyzeFunctionArgTypes :: Type -> [Expr] -> Eval Type
analyzeFunctionArgTypes (Fun rt types) exps = do
    tab <- get
    expTypes <- mapM analyzeExp exps
    unless ((length expTypes) == (length types)) $ throwCtxErr $ "Invalid number of arguments passed to function"  
    if all (uncurry (compareTypes tab)) (zip expTypes types) then
        return rt
    else
        throwCtxErr $ "Invalid function argument types, actual: " ++ showStr expTypes ++ " expected: " ++ showStr types

analyzeExp :: Expr -> Eval Type
analyzeExp (EVar id) = do
    var <- findVar id
    case var of
        (Just (VarInfo t _)) -> return t
        (Nothing) -> throwCtxErr notDeclaredErr
    where
        notDeclaredErr = "Variable not declared " ++ showStr id
analyzeExp (ENew id) = do
    classExists cls
    return cls
    where cls = Class id
analyzeExp (ENewArr t exp) = checkType [Int] exp >> return (Array t)
analyzeExp (EArrLen id) = do
    checkType [Array Int, Array Str, Array Bool, Array (Class defBaseClass)] (EVar id)
    return Int
analyzeExp (EArrElem id exp) = do
    checkType [Int] exp
    (Array t) <- checkType [Array Int, Array Str, Array Bool, Array (Class defBaseClass)] (EVar id)
    return t
analyzeExp (ENull c@(Class id)) = return c
analyzeExp (ENull _) = throwCtxErr $ "Casting is only allowed for objects"
analyzeExp (ELitInt int) = return Int
analyzeExp ELitTrue = return Bool
analyzeExp ELitFalse = return Bool
analyzeExp (EApp id exps) = do
    tab <- get
    case findFunction id tab of
        (Just t) -> analyzeFunctionArgTypes t exps
        (Nothing) -> throwCtxErr funcNotDeclaredErr
    where
        funcNotDeclaredErr = "Function not declared: " ++ showStr id
analyzeExp (EMthApp obj id exps) = do
    (Class c) <- checkType [Class defBaseClass] (EVar obj)
    tab <- get
    case findVirtualMethod c id tab of
        (Just (_, t)) -> analyzeFunctionArgTypes t newExps
        (Nothing) -> throwCtxErr methodNotFoundErr
    where
        methodNotFoundErr = "Method " ++ showStr id ++ " not found"
        newExps = [(EVar obj)] ++ exps
analyzeExp (EString str) = return Str
analyzeExp (Neg exp) = checkType [Int] exp
analyzeExp (Not exp) = checkType [Bool] exp
analyzeExp (EMul exp1 mulop exp2) = checkTypes [Int] exp1 exp2
analyzeExp (EAdd exp1 Plus exp2) = checkTypes [Int,Str] exp1 exp2
analyzeExp (EAdd exp1 Minus exp2) = checkTypes [Int] exp1 exp2
analyzeExp (ERel exp1 EQU exp2) = checkTypes [Class defBaseClass, Int, Bool] exp1 exp2 >> return Bool
analyzeExp (ERel exp1 NE exp2) = checkTypes [Class defBaseClass, Int, Bool] exp1 exp2 >> return Bool
analyzeExp (ERel exp1 relop exp2) = checkTypes [Int] exp1 exp2 >> return Bool
analyzeExp (EAnd exp1 exp2) = checkTypes [Bool] exp1 exp2
analyzeExp (EOr exp1 exp2) = checkTypes [Bool] exp1 exp2

analyzeDeclarations :: Type -> [Item] -> Eval Env
analyzeDeclarations t ((NoInit id):ds) = do
    env <- insertVar id t
    local (const env) $ analyzeDeclarations t ds
analyzeDeclarations t ((Init id exp):ds) = do
    env <- insertVar id t
    local (const env) $ analyzeStmts [(Ass id exp)] >> analyzeDeclarations t ds
analyzeDeclarations t [] = ask

analyzeStmts :: [Stmt] -> Eval Env
analyzeStmts (Empty:stmts) = analyzeStmts stmts
analyzeStmts ((BStmt block):stmts) = do
    blockEnv <- analyzeBlock block
    b <- local (const blockEnv) getReturn
    env <- setReturn b
    local (const env) $ analyzeStmts stmts
analyzeStmts (s@(Decl t items):stmts) = do
    env1 <- setStatement s 
    env2 <- local (const env1) (analyzeDeclarations t items)
    local (const env2) $ analyzeStmts stmts
analyzeStmts (s@(Ass id exp):stmts) = do
    env <- setStatement s
    t <- analyzeExp $ EVar id
    local (const env) $ checkType [t] exp
    analyzeStmts stmts
analyzeStmts (s@(ArrAss id exp1 exp2):stmts) = do
    env <- setStatement s
    (Array t) <- local (const env) $ checkType [Array Int, Array Str, Array Bool, Array (Class defBaseClass)] (EVar id)
    checkType [Int] exp1
    checkType [t] exp2
    analyzeStmts stmts
analyzeStmts (s@(Incr id):stmts) = do
    env <- setStatement s 
    local (const env) $ checkType [Int] (EVar id)
    analyzeStmts stmts
analyzeStmts (s@(Decr id):stmts) = do
    env <- setStatement s
    local (const env) $ checkType [Int] (EVar id)
    analyzeStmts stmts
analyzeStmts (s@(Ret exp):stmts) = do
    env1 <- setStatement s
    t <- local (const env1) (analyzeExp exp)
    env2 <- setReturnFound t
    local (const env2) $ analyzeStmts stmts
analyzeStmts (s@VRet:stmts) = do
    env1 <- setStatement s
    env2 <- (local (const env1) (setReturnFound Void)) 
    local (const env2) $ analyzeStmts stmts
analyzeStmts ((Cond exp stmt):stmts) = do
    env <- setStatement (Cond exp Empty)
    local (const env) $ checkType [Bool] exp
    analyzeBlock (Block [stmt])
    analyzeStmts stmts
analyzeStmts ((CondElse exp stmt1 stmt2):stmts) = do
    env <- setStatement $ Cond exp Empty
    local (const env) $ checkType [Bool] exp
    env1 <- analyzeBlock $ Block [stmt1] 
    ifRet <-(local (const env1) getReturn)
    env2 <- analyzeBlock $ Block [stmt2]
    elseRet <- (local (const env2) $ getReturn)
    env3 <- setReturn (ifRet && elseRet)
    local (const env3) $ analyzeStmts stmts
analyzeStmts ((While exp stmt):stmts) = do
    env <- setStatement (While exp Empty)
    local (const env) $ checkType [Bool] exp 
    analyzeBlock (Block [stmt])
    analyzeStmts stmts
analyzeStmts ((For t id tab stmt):stmts) = do
    env <- setStatement (For t id tab Empty)
    env2 <- local (const env) $ insertVar id t
    local (const env2) $ checkType [Array t] (EVar tab) >> analyzeBlock (Block [stmt])
    analyzeStmts stmts
analyzeStmts (s@(SExp exp):stmts) = do
    env <- setStatement s    
    local (const env) $ checkType [Int,Str,Bool,Void,Class defBaseClass] exp 
    analyzeStmts stmts
analyzeStmts [] = ask

analyzeBlock :: Block -> Eval Env
analyzeBlock (Block stmts) = do
    env <- clearVars
    local (const env) (analyzeStmts stmts)

analyzeFunctionArgs :: [Arg] -> Eval Env
analyzeFunctionArgs ((Arg t id):args) = do
    env <- insertVar id t
    local (const env) $ analyzeFunctionArgs args
analyzeFunctionArgs [] = ask

analyzeClassFields :: [Field] -> Eval Env
analyzeClassFields ((Field t id):fs) = do
    env <- insertVar id t
    local (const env) $ analyzeClassFields fs
analyzeClassFields [] = ask

analyzeDefinition :: TopDef -> Eval ()
analyzeDefinition (FnDef t id args block) = do
    env1 <- setFunction id t
    env2 <- local (const env1)  $ analyzeFunctionArgs args
    env3 <- local (const env2) $ analyzeBlock block 
    local (const env3) checkForReturn
analyzeDefinition (ClsExtDef id baseCls fields funcs) = do
    env1 <- setClass id
    env2 <- local (const env1) $ analyzeClassFields fields
    local (const env2) $ forM_ newFuncs analyzeDefinition
    where
        thisArg = (Arg (Class id) thisId)
        newFuncs = map (\(FnDef t id args block) -> (FnDef t id (thisArg:args) block)) funcs
analyzeDefinition (ClsDef id fields funcs) =
    analyzeDefinition $ ClsExtDef id defBaseClass fields funcs

collectDefinition :: TopDef -> Eval ()
collectDefinition f@(FnDef t id args block) = do
    tab <- get
    case addFunction f tab of
        (Right newTab) -> put newTab
        (Left err) -> throwCtxErr $ err
collectDefinition cls = do
    tab <- get
    case addClass cls tab of
        (Right newTab) -> put newTab
        (Left err) -> throwCtxErr $ err

analyzeProgram :: Program -> Eval ()
analyzeProgram (Program defs) = do
    forM_ defs collectDefinition
    checkForMain
    forM_ defs analyzeDefinition

analyzeLatteProgram :: Program -> IO (Either String (), SymTable)
analyzeLatteProgram prog = runStateT (runErrorT (runReaderT (analyzeProgram prog) emptyEnv)) emptySymTable
