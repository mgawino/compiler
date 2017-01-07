module SymbolicTable where

import qualified Data.Map as Map
import qualified Data.List as List
import AbsLatte
import Utils
import Data.Maybe (fromJust)

type FuncInfo = Map.Map Ident Type
type VTable = [(Ident, (Ident,Type))]
type ClassInfo = Map.Map Ident (Ident, [Ident], [(Field, Integer)], VTable)

data SymTable = SymTable FuncInfo ClassInfo

preDefinedFunctions = [(Ident "printInt", Fun Void [Int]), (Ident "printString", Fun Void [Str]), 
                       (Ident "error", Fun Void []), (Ident "readInt", Fun Int []), 
                       (Ident "readString", Fun Str []), (Ident "_concat", Fun Str [Str,Str]),
                       (Ident "malloc", Fun Str [Int])]

defBaseClass = Ident "_Object"

-- Utils --

isSuperclass :: SymTable -> Ident -> Ident -> Bool
isSuperclass tab c1 c2 = do
    if c1 == c2 then True
    else case findClass c1 tab of
        (Just (baseCls, _, _, _)) -> isSuperclass tab baseCls c2
        (Nothing) -> False

compareTypes :: SymTable -> Type -> Type -> Bool
compareTypes tab (Class c1) (Class c2) = isSuperclass tab c1 c2
compareTypes tab (Array (Class c1)) (Array (Class c2)) = isSuperclass tab c2 c2
compareTypes tab (Fun rt1 types1) (Fun rt2 types2) = 
    (rt1 == rt2) && ((length types1) == (length types2)) && all (uncurry (compareTypes tab)) (zip types1 types2)
compareTypes _ t1 t2 = t1 == t2

emptySymTable :: SymTable
emptySymTable = collectPreDefinedFunctions preDefinedFunctions emptySymTable'

emptySymTable' :: SymTable
emptySymTable' = SymTable Map.empty (Map.insert defBaseClass (Ident "", [], [(Field (Ptr Int) (Ident ""),0)], []) Map.empty)

makeClsMethodName :: Ident -> Ident -> Ident
makeClsMethodName cls funcIdent = Ident $ showStr cls ++ "$" ++ showStr funcIdent

find :: Ident -> VTable -> Maybe (Ident,Type)
find id ((ident, res):fs) = if id == ident then (Just res) else find id fs
find _ [] = Nothing

add :: VTable -> (Ident, (Ident,Type)) -> VTable -> VTable
add acc e@(fid, ft) (r@(id,t):fs) = 
    if fid == id then (acc ++ [e] ++ fs) 
    else add (acc ++ [r]) e fs
add acc e [] = acc ++ [e]   

collectPreDefinedFunctions :: [(Ident, Type)] -> SymTable -> SymTable
collectPreDefinedFunctions ((id, (Fun rt types)):fs) tab =
    case addFunction fun tab of
        (Right newTab) -> collectPreDefinedFunctions fs newTab
    where
        args = map (\t -> (Arg t (Ident "_"))) types
        fun = (FnDef rt id args (Block []))
collectPreDefinedFunctions [] tab = tab

computeOffsets :: Integer -> [(Field, Integer)] -> [Field] -> [(Field, Integer)]
computeOffsets off res (f@(Field t id):fs) = computeOffsets (off + (sizeOf t)) (res ++ [(f,off)]) fs
computeOffsets _ res [] = res

sizeOf :: Type -> Integer
sizeOf Bool = 1
sizeOf (Ptr _) = 8
sizeOf (Class _) = 8
sizeOf (Array _) = 12
sizeOf _ = 4

-- Symbolic Table --

addFunction :: TopDef -> SymTable -> Either String SymTable
addFunction (FnDef rt id args _) tab@(SymTable funcs classes) = 
    case findFunction id tab of
        (Just _) -> Left $ "Function already defined: " ++ showStr id
        (Nothing) -> Right $ SymTable (Map.insert id (Fun rt argTypes) funcs) classes
    where
        argTypes = map (\(Arg t _) -> t) args

findFunction :: Ident -> SymTable -> Maybe Type
findFunction id (SymTable funcs _) = Map.lookup id funcs

addClassMethod :: TopDef -> Ident -> SymTable -> Either String SymTable
addClassMethod (FnDef rt fid args _) cls tab@(SymTable funcs classes) =
    case findFunction method tab of
        (Just _) -> Left $ "Function already defined: " ++ showStr fid
        (Nothing) ->
            let newClss = Map.insert cls (baseCls, supercls, fields, newVTable) classes
                newFuncs = Map.insert method ft funcs
                newSymTable = SymTable newFuncs newClss in do
                case find fid vtable of
                    (Just (_, vt)) ->
                        if compareTypes tab ft vt then
                            Right newSymTable
                        else
                            Left $ "Invalid redeclaration of virtual function: " ++ showStr fid
                    (Nothing) ->
                        Right $ newSymTable
    where
        ft = Fun rt $ [(Class cls)] ++ map (\(Arg t _) -> t) args
        method = makeClsMethodName cls fid
        (baseCls, supercls, fields, vtable) = fromJust $ findClass cls tab
        newVTable = add [] (fid, (cls, ft)) vtable

addClassMethods :: [TopDef] -> Ident -> SymTable -> Either String SymTable
addClassMethods (f:fs) cls  tab = 
    case addClassMethod f cls tab of
        (Right newTab) -> addClassMethods fs cls newTab
        left -> left
addClassMethods [] _ tab = Right $ tab

addClass :: TopDef -> SymTable -> Either String SymTable
addClass (ClsExtDef id baseCls fields methods) tab@(SymTable fs clss) = 
    case findClass id tab of
        (Just _) -> Left $ "Class already defined: " ++ showStr id
        (Nothing) ->
            case findClass baseCls tab of
                (Just (bbcls, supercls, baseFields, vt)) ->
                    let newClss = Map.insert baseCls (bbcls, id:supercls, baseFields, vt) clss
                        s@((Field t fid), off) = last baseFields
                        newFields = computeOffsets ((toInteger off) + (sizeOf t)) [] fields 
                        newMembers = (vptrField, 0) : tail (baseFields ++ newFields)
                        insFields = SymTable fs (Map.insert id (baseCls, [], newMembers, vt) newClss) in do
                        addClassMethods methods id insFields
                (Nothing) -> Left $ "Base class not found: " ++ showStr baseCls
    where
        vptrField = getVPtrField id
addClass (ClsDef id fields methods) tab = 
    addClass (ClsExtDef id defBaseClass fields methods) tab
addClass fun tab = Left $ "Nested classes not allowed"

findClass :: Ident -> SymTable -> Maybe (Ident, [Ident], [(Field, Integer)], VTable)
findClass id (SymTable _ classes) = Map.lookup id classes

findVirtualMethod :: Ident -> Ident -> SymTable -> Maybe (Ident, Type)
findVirtualMethod cls id tab =
    case findClass cls tab of
        (Just (_, _, _, vt)) -> find id vt
        Nothing -> Nothing

findVirtualMethodIndex :: Ident -> Ident -> SymTable -> Int
findVirtualMethodIndex cls id tab = fromJust $ List.findIndex (\(i,res) -> i == id) vt 
    where
        (_, _, _, vt) = fromJust $ findClass cls tab

getClassVirtualTable :: Ident -> SymTable -> [(Ident,Type)]
getClassVirtualTable id tab = 
    map (\(fid,(cls,t)) -> (makeClsMethodName cls fid, t)) $ vt 
    where
        (_, _, _, vt) = fromJust $ findClass id tab

findClassField :: Ident -> Ident -> SymTable -> (Field, Integer)
findClassField cls fid tab =
    case res of
        [x] -> x
        [] -> head $ map (\c -> findClassField c fid tab) supercls 
    where 
        (_, supercls, fields, _) = fromJust $ findClass cls tab
        res = filter (\((Field t id), off) -> id == fid) fields 
        
