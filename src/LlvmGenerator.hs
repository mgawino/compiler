module LlvmGenerator where

import AbsLatte
import Utils
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.List
import Control.Monad.Reader
import Control.Monad.State (StateT, modify, get, put, runStateT)
import SymbolicTable

data StmtInfo = StmtInfo Bool Bool
type Env = (Map.Map Ident RegValue,StmtInfo)
data State = State ([String], Integer, Label) SymTable
type Eval a = ReaderT Env (StateT State IO) a

-- Utils Store --

emptyStore :: SymTable -> State
emptyStore tab = State ([], 0, Label "") tab

emit :: Print a => a -> Eval ()
emit s = modify $ \(State (code, r, l) tab) -> State (code ++ [convert s], r, l) tab

emitGlobal :: Print a => a -> Eval ()
emitGlobal s = modify $ \(State (code, r, l) tab) -> State ([convert s] ++ code, r, l) tab

createRegister :: Type -> Eval RegValue
createRegister t = do
    (State (code, reg, l) tab) <- get
    put $ State (code, reg+1, l) tab
    return $ RegVal (Local ("_" ++ show reg)) t

createGlobalRegister :: Type -> Eval RegValue
createGlobalRegister t = do
    (State (code, reg, l) tab) <- get
    put $ State (code, reg+1, l) tab
    return $ RegVal (Global ("_" ++ show reg)) t

createLabel :: Eval Label
createLabel = do
    (State (code, reg, l) tab) <- get
    put $ State (code, reg+1, l) tab
    return $ Label ("_L" ++ show reg)

setLabel :: Label -> Eval ()
setLabel l = do
    (State (code, reg, _) tab) <- get
    put $ State (code, reg, l) tab

getLabel :: Eval Label
getLabel = get >>= \(State (_, _, l) _) -> return l

getSymTable :: Eval SymTable
getSymTable = get >>= \(State _ tab) -> return tab

-- Utils Env --

emptyEnv :: Env
emptyEnv = (Map.empty,StmtInfo True False)

insertVar :: Ident -> Type -> Eval (Env, RegValue)
insertVar id t = do
    r <- createRegister $ Ptr t
    emit $ Alloc r 
    (map,b) <- ask
    return ((Map.insert id r map, b), r)

getClassField :: Env -> Ident -> Eval RegValue
getClassField (map, _) ident = do
    tab <- getSymTable
    thisReg <- createRegister $ Class cls
    emit $ Load thisReg this
    let ((Field t _), off) = findClassField cls ident tab in do
       fieldPtr <- createRegister $ Ptr Byte
       emit $ GetElemPtr fieldPtr thisReg $ IntVal off
       res <- createRegister $ Ptr t
       emit $ BitCast res fieldPtr
       return res
    where
        this = map Map.! thisId
        (Ptr (Class cls)) = getRegisterType this

getRegister :: Ident -> Eval RegValue
getRegister ident = do
    env@(map, _) <- ask
    case Map.lookup ident map of
        (Just r) -> return r
        (Nothing) -> getClassField env ident

setLastStatement :: Bool -> Eval Env
setLastStatement b = ask >>= \(map,(StmtInfo _ r)) -> return (map,(StmtInfo b r))

isLastStatement :: Eval Bool
isLastStatement = ask >>= \(_, (StmtInfo s _)) -> return s

setReturn :: Bool -> Eval Env
setReturn b = ask >>= \(map, (StmtInfo s _)) -> return (map,(StmtInfo s b))

getReturn :: Eval Bool
getReturn = ask >>= \(map, (StmtInfo _ r)) -> return r

getDefaultValue :: Type -> Eval RegValue
getDefaultValue Int = return $ IntVal 0
getDefaultValue Bool = return $ BoolVal False
getDefaultValue Str = generateExp $ EString ""
getDefaultValue (Class _) = return Null
getDefaultValue (Array _) = return Null

-- Generation --

generateFieldInit :: Ident -> RegValue -> (Field, Integer) -> Eval ()
generateFieldInit cls reg ((Field t id), 0) = do
    ptr <- createRegister $ Ptr Byte
    emit $ GetElemPtr ptr reg $ IntVal 0
    vtbPtr <- createRegister $ Ptr $ Ptr structVtableType
    emit $ BitCast vtbPtr ptr
    emit $ Store (RegVal (Global vtableData) (Ptr structVtableType)) vtbPtr
    where
        structVtableType = Struct $ makeVtableTypeIdent cls
        (Ident vtableData) =  makeVtableDataIdent cls  
generateFieldInit cls reg ((Field t id), off) = do
    ptr <- createRegister $ Ptr Byte
    emit $ GetElemPtr ptr reg $ IntVal off
    valPtr <- createRegister $ Ptr t
    emit $ BitCast valPtr ptr
    val <- getDefaultValue t
    emit $ Store val valPtr

generateExp' :: Operation -> Expr -> Expr -> Eval RegValue
generateExp' op exp1 exp2 = do
    r1 <- generateExp exp1
    r2 <- generateExp exp2
    r <- createRegister $ getRegisterType r1
    emit $ Opr r r1 op r2
    return r
    
generateExp :: Expr -> Eval RegValue
generateExp (EVar id) = do
    r@(RegVal i (Ptr t)) <- getRegister id
    var <- createRegister t
    emit $ Load var r
    return var
generateExp (ENew id) = do
    tab <- getSymTable
    let (_, _, fields, _) = fromJust $ findClass id tab
        ((Field t _), off) = last fields in do
        r <- createRegister cls
        emit $ Malloc r $ IntVal ((toInteger off) + (sizeOf t))
        forM_ fields $ generateFieldInit id r
        return r
    where cls = (Class id)
generateExp (ENewArr t exp) = do
    r <- createRegister $ Ptr Byte
    emit $ Malloc r $ IntVal $ sizeOf $ Array t
    structPtr <- createRegister $ Array t
    emit $ BitCast structPtr r
    lenPtr <- createRegister $ Ptr Int
    emit $ GetStructElemPtr lenPtr structPtr 0
    size <- generateExp exp
    emit $ Store size lenPtr
    arrPtr <- createRegister $ Ptr $ Ptr t
    emit $ GetStructElemPtr arrPtr structPtr 1
    newArrPtr <- createRegister $ Ptr Byte
    arrSize <- generateExp $ EMul (ELitInt (sizeOf t)) Times exp
    emit $ Malloc newArrPtr $ arrSize
    newArr <- createRegister $ Ptr t
    emit $ BitCast newArr newArrPtr
    emit $ Store newArr arrPtr
    return structPtr
generateExp (EArrLen id) = do
    structPtr <- generateExp $ EVar id
    lenPtr <- createRegister $ Ptr Int
    emit $ GetStructElemPtr lenPtr structPtr 0
    len <- createRegister Int
    emit $ Load len lenPtr
    return len
generateExp (EArrElem id exp) = do
    structPtr <- generateExp $ EVar id
    index <- generateExp exp
    let (Array t) = getRegisterType structPtr in do
        arrPtr <- createRegister $ Ptr $ Ptr t
        emit $ GetStructElemPtr arrPtr structPtr 1
        arr <- createRegister $ Ptr t
        emit $ Load arr arrPtr
        elemPtr <- createRegister $ Ptr t
        emit $ GetElemPtr elemPtr arr index
        elem <- createRegister t
        emit $ Load elem elemPtr
        return elem
generateExp (ENull _) = return Null
generateExp (ELitInt int) = return $ IntVal int
generateExp ELitTrue = return $ BoolVal True 
generateExp ELitFalse = return $ BoolVal False 
generateExp (EApp id@(Ident fun) exps) = do
    tab <- getSymTable
    let (Fun t _) = fromJust $ findFunction id tab in do
        regs <- mapM generateExp exps
        r <- createRegister t
        emit $ Call t (Global fun) regs r
        return r
generateExp (EMthApp ob fid exps) = do
    objPtr <- getRegister ob
    tab <- getSymTable
    let (Ptr c@(Class cls)) = getRegisterType objPtr
        (_, ft@(Fun rt types)) = fromJust $ findVirtualMethod cls fid tab
        struct = Struct $ makeVtableTypeIdent cls
        ix = findVirtualMethodIndex cls fid tab in do
        obj <- createRegister c
        emit $ Load obj objPtr 
        firstFieldPtr <- createRegister $ Ptr Byte
        emit $ GetElemPtr firstFieldPtr obj $ IntVal 0
        vtbPtr <- createRegister $ Ptr $ Ptr struct
        emit $ BitCast vtbPtr firstFieldPtr
        vtb <- createRegister $ Ptr struct
        emit $ Load vtb vtbPtr
        funcTabPtr <- createRegister $ Ptr $ Ptr ft
        emit $ GetStructElemPtr funcTabPtr vtb ix
        funPtr@(RegVal fr _) <- createRegister $ Ptr ft
        emit $ Load funPtr funcTabPtr
        regs <- mapM generateExp $ (EVar ob) : exps
        ret <- createRegister rt
        emit $ Call rt fr regs ret
        return ret
generateExp (EString str) = do
    r <- createGlobalRegister Str
    emitGlobal $ GlobalStr r str
    let len = (length str) + 1 in do
        res <- createRegister Str
        emit $ StrBitcast res r len
        return res
generateExp (Neg exp) = generateExp' (MOp Times) exp (ELitInt (-1))
generateExp (Not exp) = generateExp' XorOp exp ELitTrue
generateExp (EMul exp1 mulop exp2) = generateExp' (MOp mulop) exp1 exp2
generateExp (EAdd exp1 Plus exp2) = do
    r1 <- generateExp exp1
    r2 <- generateExp exp2
    r <- createRegister $ getRegisterType r1
    when ((getRegisterType r1) == Int) $ emit $ Opr r r1 (AOp Plus) r2
    when ((getRegisterType r1) == Str) $ emit $ StrConcat r r1 r2
    return r
generateExp (EAdd exp1 Minus exp2) = generateExp' (AOp Minus) exp1 exp2
generateExp (ERel exp1 relop exp2) = do
    (RegVal r t) <- generateExp' (ROp relop) exp1 exp2
    return $ (RegVal r Bool)
generateExp (EAnd exp1 exp2) = do
    r1 <- generateExp exp1
    lastLabel <- getLabel
    expLabel <- createLabel
    endLabel <- createLabel
    emit $ GotoCond r1 expLabel endLabel
    emit expLabel >> setLabel expLabel
    r2 <- generateExp exp2
    emit $ Goto endLabel
    emit endLabel >> setLabel endLabel
    r <- createRegister Bool
    emit $ Phi r (BoolVal False) lastLabel r2 expLabel
    return r
generateExp (EOr exp1 exp2) = do
    r1 <- generateExp exp1
    lastLabel <- getLabel
    expLabel <- createLabel
    endLabel <- createLabel
    emit $ GotoCond r1 endLabel expLabel
    emit expLabel >> setLabel expLabel
    r2 <- generateExp exp2
    emit $ Goto endLabel
    emit endLabel >> setLabel endLabel
    r <- createRegister Bool
    emit $ Phi r (BoolVal True) lastLabel r2 expLabel
    return r
generateDeclarations :: Type -> [Item] -> Eval Env
generateDeclarations t ((NoInit id):ds) = do
    (newEnv, r) <- insertVar id t
    val <- getDefaultValue t
    emit $ Store val r
    local (const newEnv) $ generateDeclarations t ds
generateDeclarations t ((Init id exp):ds) = do
    (newEnv, r) <- insertVar id t
    val <- generateExp exp
    emit $ Store val r
    local (const newEnv) $ generateDeclarations t ds
generateDeclarations _ [] = ask

generateStmts :: [Stmt] -> Eval Env
generateStmts (Empty:stmts) = generateStmts stmts
generateStmts ((BStmt (Block bstmts)):stmts) = do
    env1 <- setLastStatement $ stmts == []
    env2 <- local (const env1) $ generateStmts bstmts
    r <- local (const env2) getReturn
    if not r then
        generateStmts stmts
    else
        setReturn r
generateStmts ((Decl typ items):stmts) = do 
    env <- generateDeclarations typ items
    local (const env) (generateStmts stmts)
generateStmts ((Ass ident exp):stmts) = do
    val <- generateExp exp
    r <- getRegister ident
    emit $ Store val r
    generateStmts stmts
generateStmts ((ArrAss id exp1 exp2):stmts) = do
    structPtr <- generateExp $ EVar id
    index <- generateExp exp1
    val <- generateExp exp2
    arrPtr <- createRegister $ Ptr $ Ptr $ getRegisterType val
    emit $ GetStructElemPtr arrPtr structPtr 1
    arr <- createRegister $ Ptr $ getRegisterType val
    emit $ Load arr arrPtr
    elemPtr <- createRegister $ Ptr $ getRegisterType val
    emit $ GetElemPtr elemPtr arr index
    emit $ Store val elemPtr
    generateStmts stmts
generateStmts ((Incr ident):stmts) = generateStmts ((Ass ident exp):stmts)
    where exp = EAdd (EVar ident) Plus (ELitInt 1)
generateStmts ((Decr ident):stmts) = generateStmts ((Ass ident exp):stmts)
    where exp = EAdd (EVar ident) Minus (ELitInt 1)
generateStmts ((Ret exp):stmts) = do
    r <- generateExp exp
    emit $ Return r
    setReturn True
generateStmts (VRet:stmts) = emit VReturn >> setReturn True
generateStmts ((Cond exp stmt):stmts) = do
    val <- generateExp exp
    tLabel <- createLabel
    endLabel <- createLabel
    emit $ GotoCond val tLabel endLabel
    emit tLabel >> setLabel tLabel
    generateStmts [stmt]
    emit $ Goto endLabel
    emit endLabel >> setLabel endLabel
    generateStmts stmts
generateStmts ((CondElse exp stmt1 stmt2):stmts) = do
    val <- generateExp exp
    tLabel <- createLabel
    fLabel <- createLabel
    endLabel <- createLabel
    s <- isLastStatement
    emit $ GotoCond val tLabel fLabel
    emit tLabel >> setLabel tLabel
    env1 <- generateStmts [stmt1]
    r1 <- local (const env1) getReturn
    unless (s && stmts == []) $ emit (Goto endLabel) >> setLabel endLabel
    emit fLabel >> setLabel fLabel
    env2 <- generateStmts [stmt2]
    r2 <- local (const env2) getReturn
    unless (s && stmts == []) $ emit (Goto endLabel) >> emit endLabel >> setLabel endLabel
    env3 <- setReturn $ r1 && r2
    local (const env3) $ generateStmts stmts
generateStmts ((While exp stmt):stmts) = do
    condLabel <- createLabel
    loopLabel <- createLabel
    endLabel <- createLabel
    emit $ Goto condLabel
    emit condLabel >> setLabel condLabel
    val <- generateExp exp
    emit $ GotoCond val loopLabel endLabel
    emit loopLabel >> setLabel loopLabel
    generateStmts [stmt]
    emit $ Goto condLabel
    emit endLabel >> setLabel endLabel
    generateStmts stmts
generateStmts ((For t var tab stmt):stmts) =
    generateStmts $ [(Decl Int [NoInit id]), (Decl t [NoInit var]), (While exp newStmt)] ++ stmts
    where
        id = Ident "_i"
        exp = (ERel (EVar id) LTH (EArrLen tab))
        newStmt = BStmt $ Block $ [(Ass var (EArrElem tab (EVar id)))] ++ [stmt] ++ [(Incr id)]
generateStmts ((SExp exp):stmts) = generateExp exp >> generateStmts stmts
generateStmts [] = ask

generateBlock :: Block -> Eval Env
generateBlock (Block stmts) = do
    env <- generateStmts stmts
    b <- local (const env) getReturn
    unless b $ emit $ VReturn
    return env

storeFunArgs :: [Arg] -> Eval Env
storeFunArgs ((Arg t ident@(Ident id)):as) = do
    (env, r) <- insertVar ident t
    emit $ Store (RegVal (Local id) t) r
    local (const env) (storeFunArgs as)
storeFunArgs [] = ask

generateClassFunction :: Ident -> TopDef -> Eval ()
generateClassFunction cls (FnDef typ id args block) = do
    let method = makeClsMethodName cls id in
        generateDefinition (FnDef typ method newArgs block)
    where
        newArgs = [(Arg (Class cls) thisId)] ++ args

generateVTable :: Ident -> Eval ()
generateVTable id = do
    tab <- getSymTable
    let vt = getClassVirtualTable id tab in do
        emitGlobal $ VTableData id vt
        emitGlobal $ VTableType id vt

generateDefinition :: TopDef -> Eval ()
generateDefinition f@(FnDef typ id args block) = do
    emit f
    emit OpBrace
    entry <- createLabel
    emit entry >> setLabel entry
    env <- storeFunArgs args
    local (const env) $ generateBlock block
    emit ClBrace
generateDefinition (ClsExtDef id baseCls fields funcs) = do
    forM_ funcs $ generateClassFunction id
    tab <- getSymTable
    generateVTable id
generateDefinition (ClsDef id fields funcs) = do
    forM_ funcs $ generateClassFunction id
    generateVTable id

generateProgram :: Program -> Eval ()
generateProgram (Program defs) = do
    emitGlobal $ ArrayTypes
    forM_ preDefinedFunctions $ \(ident,t) -> emit $ Declare ident t
    forM_ defs generateDefinition

generateLlvmProgram :: Program -> SymTable -> IO [String]
generateLlvmProgram prog tab = do
    ((), (State (code,_,_) _)) <- runStateT (runReaderT (generateProgram prog) emptyEnv) (emptyStore tab)
    return code
