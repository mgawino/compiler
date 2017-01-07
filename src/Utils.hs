module Utils where

import AbsLatte
import Data.List (intercalate)

data IdentDec = Global String | Local String
data RegValue = RegVal IdentDec Type | IntVal Integer | BoolVal Bool | Null
data Operation = MOp MulOp | AOp AddOp | ROp RelOp | AndOp | OrOp | XorOp
data Statement = Call Type IdentDec [RegValue] RegValue |
                 Load RegValue RegValue |
                 Store RegValue RegValue |
                 Opr RegValue RegValue Operation RegValue |
                 Goto Label | 
                 GotoCond RegValue Label Label |
                 Return RegValue | 
                 VReturn | 
                 Alloc RegValue |
                 StrBitcast RegValue RegValue Int |
                 StrConcat RegValue RegValue RegValue |
                 Malloc RegValue RegValue |
                 Phi RegValue RegValue Label RegValue Label |
                 BitCast RegValue RegValue |
                 GetElemPtr RegValue RegValue RegValue |
                 GetStructElemPtr RegValue RegValue Int
data GlobalDecls = Declare Ident Type | 
                   GlobalStr RegValue String |
                   VTableType Ident [(Ident,Type)] |
                   VTableData Ident [(Ident,Type)] |
                   ArrayTypes
data Braces = OpBrace | ClBrace
data Label = Label String

thisId = Ident "self"
vtableDataSuffix = "_vtable_data"
vtableTypeSuffix = "_vtable_type"
vrtTablePtr = Ident "_vptr"

makeVtableDataIdent :: Ident -> Ident
makeVtableDataIdent (Ident cls) = Ident $ cls ++ vtableDataSuffix

makeVtableTypeIdent :: Ident -> Ident
makeVtableTypeIdent (Ident cls) = Ident $ cls ++ vtableTypeSuffix

getVPtrField :: Ident -> Field
getVPtrField id = Field (Ptr struct) vrtTablePtr
    where
        struct = Struct $ makeVtableTypeIdent id

getRegisterType :: RegValue -> Type
getRegisterType (RegVal _ t) = t
getRegisterType (IntVal _) = Int
getRegisterType (BoolVal _) = Bool
getRegisterType Null = Ptr $ Byte

convertFunArgs :: Print a => [a] -> String
convertFunArgs args = 
    "(" ++ (intercalate ", " (map convert args)) ++ ")"

convertFunCallArgs :: [RegValue] -> String
convertFunCallArgs args = 
    "(" ++ (intercalate ", " (map fun args)) ++ ")"
    where
        fun = (\r -> (convert $ getRegisterType r) ++ " " ++ (convert r))

convertVTableData :: [(Ident,Type)] -> String
convertVTableData funcs =
    "{\n" ++ (intercalate ",\n" (map fun funcs)) ++ "\n}"
    where
        fun = (\(id,t) -> convert t ++ "* @" ++ showStr id)

convertVTableType :: [(Ident,Type)] -> String
convertVTableType funcs =
        "{\n" ++ (intercalate ",\n" (map fun funcs)) ++ "\n}"
    where
        fun = (\(_, t) -> convert t ++ "*")

convertArrayTypes :: String
convertArrayTypes =
    concat $ map (\a@(Array t) -> (convertArray a) ++ " = type {i32, " ++ (convert (Ptr t)) ++ "}\n") types
    where
        types = [Array Int, Array Str, Array Bool, Array (Class (Ident ""))]

convertArray :: Type -> String
convertArray (Array Int) = "%struct.ArrInt"
convertArray (Array Str) = "%struct.ArrStr"
convertArray (Array Bool) = "%struct.ArrBool"
convertArray (Array (Class _)) = "%struct.ArrObj"

class ShowStr a where
    showStr :: a -> String

instance ShowStr Type where
    showStr Int = "Integer"
    showStr Str = "String"
    showStr Bool = "Boolean"
    showStr Void = "Void"
    showStr (Array t) = "Array of " ++ showStr t
    showStr (Class (Ident id)) = "Class " ++ id 
    showStr a = show a

instance ShowStr a => ShowStr [a] where
    showStr list = "[" ++ (intercalate "," (map showStr list)) ++ "]"

instance ShowStr Ident where
    showStr (Ident id) = id

class Print a where
    convert :: a -> String

class PrintType a where
    convertType :: a -> String

instance Print TopDef where
    convert (FnDef typ (Ident ident) args block) = 
        unwords ["define", convert typ, "@" ++ ident, convertFunArgs args]

instance Print Type where
    convert Int = "i32"
    convert Str = "i8*"
    convert Bool = "i1"
    convert Void = "void"
    convert a@(Array t) = (convertArray a) ++ "*" 
    convert (Fun rt types) = convert rt ++ (convertFunArgs types)
    convert (Class _) = "i8*"
    convert (Ptr t) = (convert t) ++ "*"
    convert (Struct id) = "%" ++ showStr id
    convert (Byte) = "i8"

instance Print Ident where
    convert (Ident id) = show id

instance Print Arg where
    convert (Arg typ (Ident ident)) = convert typ ++ " %" ++ ident

instance Print IdentDec where
    convert (Global id) = "@" ++ id
    convert (Local id) = "%" ++ id

instance PrintType RegValue where
    convertType r = convert $ getRegisterType r

instance Print RegValue where
    convert (RegVal id _) = convert id
    convert (IntVal i) = show i
    convert (BoolVal True) = "true"
    convert (BoolVal False) = "false"
    convert (Null) = "null"

instance Print Operation where
    convert (MOp Times) = "mul"
    convert (MOp Div) = "sdiv"
    convert (MOp Mod) = "srem"
    convert (AOp Plus) = "add"
    convert (AOp Minus) = "sub"
    convert (ROp LTH) = "icmp slt"
    convert (ROp LE) = "icmp sle"
    convert (ROp GTH) = "icmp sgt"
    convert (ROp GE) = "icmp sge"
    convert (ROp EQU) = "icmp eq"
    convert (ROp NE) = "icmp ne"
    convert (AndOp) = "and"
    convert (OrOp) = "or"
    convert (XorOp) = "xor"

instance Print Statement where
    convert (Call t id args optReg) = 
        let pref = if t == Void then "" else (convert optReg) ++ " = " in 
            pref ++ unwords ["call", convert t, convert id, convertFunCallArgs args]
    convert (Load r1 r2) = unwords [convert r1, "=", "load", convertType r2, convert r2]
    convert (Store val r) = unwords ["store", convertType val, convert val, ",", convertType r, convert r]
    convert (Opr r r1 op r2) = unwords [convert r, "=", convert op, convertType r, convert r1, ",", convert r2]
    convert (Goto (Label l)) = unwords ["br", "label", "%" ++ l]
    convert (GotoCond r (Label l1) (Label l2)) = 
        unwords ["br","i1", convert r, ",", "label", "%" ++ l1, ",", "label", "%" ++ l2] 
    convert (Return r) = unwords ["ret", convert (getRegisterType r), convert r]
    convert (VReturn) = "ret void"
    convert (Alloc (RegVal id (Ptr t))) = unwords [convert id, "=", "alloca", convert t]
    convert (StrConcat r r1 r2) = unwords [convert r, "=", "call i8* @_concat(i8*", convert r1, ", i8*", (convert r2) ++ ")"]
    convert (StrBitcast r r2 len) = unwords [convert r, "=", "bitcast", "[" ++ (show len), "x i8]*", convert r2, "to i8*"]
    convert (Malloc r size) = unwords [convert r, "=", "call i8* @malloc(i32", (convert size) ++ ")"]
    convert (Phi r r1 (Label l1) r2 (Label l2)) = 
        unwords [convert r, "=", "phi i1", "[", convert r1, ",", "%" ++ l1, "]", ", [", convert r2, ",", "%" ++ l2, "]"]
    convert (BitCast r1 r2) = unwords [convert r1, "=", "bitcast", convertType r2, convert r2, "to", convertType r1]
    convert (GetElemPtr ptr r n) = 
        unwords [convert ptr, "=", "getelementptr", convertType r, convert r, ", i32", convert n]
    convert (GetStructElemPtr ptr r n) = 
        unwords [convert ptr, "=", "getelementptr", convertType r, convert r, ", i32 0, i32", show n]
instance Print GlobalDecls where
    convert (Declare (Ident id) (Fun t args)) = unwords ["declare", convert t, "@" ++ id, convertFunArgs args]
    convert (GlobalStr r str) = 
        unwords [convert r, "=", "internal", "constant", "[" ++ show ((length str) + 1), "x", "i8]", "c\"" ++ str ++ "\\00\""] 
    convert (VTableType id funcs) = 
        unwords ["%" ++ showStr id ++ vtableTypeSuffix, "=", "type", convertVTableType funcs]
    convert (VTableData id funcs) = 
        unwords ["@" ++ showStr id ++ vtableDataSuffix, "=", "global", "%" ++ showStr id ++ vtableTypeSuffix, convertVTableData funcs]
    convert ArrayTypes = convertArrayTypes

instance Print Label where
    convert (Label l) = l ++ ":"

instance Print Braces where
    convert OpBrace = "{"
    convert ClBrace = "}"
