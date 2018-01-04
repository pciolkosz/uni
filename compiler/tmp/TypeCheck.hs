module TypeCheck where

import Control.Monad.Except
import Data.Either
import Control.Monad.Reader
import qualified Data.Map as Map
import Data.Maybe
import AbsLatte
import PrintLatte

type TCMonad = ReaderT TCEnv (Except String)

type TCEnv = (Map.Map Ident Type, Map.Map Ident (Map.Map Ident Type, [Ident]))

checkTypes :: Program -> IO (Bool, Map.Map Ident Type)
checkTypes (Program defs) = do 
    case runExcept $ prepareTopEnv defs startEnv of
        Left errMsg -> putStr errMsg >> return (False, Map.empty)
        Right env -> case runExcept $ runReaderT (prepareTCMonad defs) env of
            Left errMsg -> putStr (errMsg ++ "\n")  >> return (False, Map.empty)
            Right _ -> putStr "types OK\n" >> return (True, fst env)

prepareTopEnv :: [TopDef] -> TCEnv -> Except String TCEnv 
prepareTopEnv [] env = return env
prepareTopEnv (def:rest) (vEnv, cEnv) =
    let envMonad = case def of {
        FnDef t ident args _ -> return (Map.insert ident (Fun t (map a2t args)) vEnv, cEnv);
        ClDef ident members -> return (vEnv, Map.insert ident (prepClsTopEnv members, []) cEnv);
        DepClDef ident anc members -> case Map.lookup anc cEnv of
            Just (ancEnv, superClses) -> return (vEnv, Map.insert ident (Map.union (prepClsTopEnv members) ancEnv, (anc:superClses)) cEnv)
            Nothing -> throwError $ "Class " ++ (printTree ident) ++ " can't extend unknown class " ++ (printTree anc) ++ "\n"} 
    in envMonad >>= \newEnv -> prepareTopEnv rest newEnv

prepareTCMonad :: [TopDef] -> TCMonad ()
prepareTCMonad [] = return ()
prepareTCMonad (def:rest) = case def of {
    FnDef t _ args (Block body) ->  checkFunc args body t;
    ClDef ident members -> findClassEnv ident >>= \env -> localF (Map.union env) $ checkMembers members;
    DepClDef ident anc members -> findClassEnv ident >>= \env -> localF (Map.union env) $ checkMembers members
} >> prepareTCMonad rest --TODO last to cases are the same

prepClsTopEnv :: [MemberDef] -> Map.Map Ident Type
prepClsTopEnv [] = Map.empty
prepClsTopEnv (def:rest) = case def of
    MAttr t items -> foldr (flip Map.insert $ t) (prepClsTopEnv rest) $ map i2id items --TODO is foldl faster?
    MMethod t id args _ -> Map.insert id (Fun t $ map a2t args) $ prepClsTopEnv rest

checkMembers :: [MemberDef] -> TCMonad ()
checkMembers [] = return ()
checkMembers (def:rest) = case def of
    MAttr t1 items -> (typeCorrect t1 (printTree def)) >> mapM_ (\item -> case item of
        NoInit _ -> return ()
        Init ident expr -> checkExpr expr >>= \t2 -> sameTypesOrErr t1 t2 $ printTree def
        InitAlloc ident t2 -> sameTypesOrErr t1 t2 $ printTree def
        ArrIAlloc ident t2 expr -> do 
            sameTypesOrErr t1 (Arr t2) $ printTree def
            t3 <- checkExpr expr
            sameTypesOrErr t3 Int $ printTree def)
     items
    MMethod t ident args (Block body) -> checkFunc args body t

checkExpr :: Expr -> TCMonad Type
checkExpr expr = case expr of
    EArrAcc e1 e2 -> do
        t1 <- checkExpr e1
        t2 <- checkExpr e2
        case (t1, t2) of
            (Arr t, Int) -> return t
            _ -> throwError $ "Wrong types in arr acc in " ++ (printTree expr)
    ENull t -> case t of
        Class id -> return t
        _ -> throwError $ "Can't cast null to non-class type in " ++ (printTree expr)
    EMember e memId -> do
        t <- checkExpr e
        case t of
            Class clsId -> do
                env <- findClassEnv clsId
                case (Map.lookup memId env) of
                    Just t -> return t
                    Nothing -> throwError $ "Access to unknown class in " ++ (printTree expr)
            Arr _ -> do
                if memId == Ident "length" then
                    return Int
                else
                    throwError $ "Non-class types does not have attributes in " ++ (printTree expr)
            _ -> throwError $ "Non-class types does not have attributes in " ++ (printTree expr)
    EMethod obj ident params -> do
        t <- checkExpr obj
        case t of
            Class classId -> do
                env <- findClassEnv classId
                case Map.lookup ident env of
                    Just (Fun retT args) -> do
                        checkCall params args $ printTree expr
                        return retT
                    _ -> throwError $ "Class " ++ (printTree classId) ++ " has no method " ++ (printTree ident) ++ " in " ++ (printTree expr)
            _ -> throwError $ "Non-class types does not have attributes in " ++ (printTree expr)
    EVar ident -> do
        varT <- findType ident
        case varT of
            Just t -> return t
            Nothing -> throwError $ "Unknown variable " ++ (printTree ident) ++ " in " ++ (printTree expr)
    ELitInt _ -> return Int
    ELitTrue -> return Bool
    ELitFalse -> return Bool
    EApp ident params -> do
        t <- findType ident
        case t of
            Just (Fun retT args) -> do
                checkCall params args $ printTree expr
                return retT
            _ -> throwError $ "Unknown function " ++ (printTree ident) ++ " in " ++ (printTree expr)
    EString _ -> return Str
    Neg expr -> do
        t <- checkExpr expr
        sameTypesOrErr t Int $ printTree expr
        return Int
    Not expr -> do
        t <- checkExpr expr
        sameTypesOrErr t Bool $ printTree expr
        return Bool
    EMul e1 _ e2 -> checkBinOp e1 e2 [Int] $ printTree expr
    EAdd e1 _ e2 -> checkBinOp e1 e2 [Int, Str] $ printTree expr
    ERel e1 op e2 -> if op == E || op == NE 
        then 
            do
                t1 <- checkExpr e1
                t2 <- checkExpr e2
                sameTypesOrErr t1 t2 $ printTree expr
                return Bool
        else
            (checkBinOp e1 e2 [Int] $ printTree expr) >> return Bool
    EAnd e1 e2 -> checkBinOp e1 e2 [Bool] $ printTree expr
    EOr e1 e2 -> checkBinOp e1 e2 [Bool] $ printTree expr
   
checkCall :: [Expr] -> [Type] -> String -> TCMonad ()
checkCall exprs types errMsg = do 
    args <- mapM checkExpr exprs
    foldl (>>) (return ()) $ zipWith (sameTypesOrErr' errMsg) types args

checkBinOp :: Expr -> Expr -> [Type] -> String -> TCMonad Type
checkBinOp e1 e2 types errMsg = do
    t1 <- checkExpr e1    
    t2 <- checkExpr e2
    if t1 == t2 && (elem t1 types) then
        return t1
    else
        throwError errMsg --TODO better eMsg

checkStmts :: [Stmt] -> Type -> TCMonad ()
checkStmts [] _ = return ()
checkStmts (stmt:rest) retT = case stmt of
    Decl t items -> ask >>= (checkDecls items t . fst) >>= 
        \newEnv -> localF (const newEnv) $ checkStmts rest retT
    _ -> (case stmt of
        Alloc expr t1 -> do
            t2 <- checkExpr expr
            sameTypesOrErr t2 t1 $ printTree stmt
        ArrAlloc e1 t1 e2 -> do
            t2 <- checkExpr e1
            t3 <- checkExpr e2
            sameTypesOrErr t2 (Arr t1) $ printTree stmt
            sameTypesOrErr t3 Int $ printTree stmt
        Empty -> return ()
        BStmt (Block stmts) -> checkStmts stmts retT
        Ass e1 e2 -> do
            t1 <- checkExpr e1
            t2 <- checkExpr e2
            sameTypesOrErr t1 t2 $ printTree stmt
        Incr ident -> do
            t <- findType ident
            case t of
                Just t -> sameTypesOrErr t Int $ printTree stmt
                Nothing -> throwError $ "Unknown variable " ++ (printTree ident) ++ " in " ++ (printTree stmt)
        Decr ident -> do
            t <- findType ident
            case t of
                Just t -> sameTypesOrErr t Int $ printTree stmt
                Nothing -> throwError $ "Unknown variable " ++ (printTree ident) ++ " in " ++ (printTree stmt)
        Ret expr -> do
            t <- checkExpr expr
            sameTypesOrErr retT t $ printTree stmt
        VRet -> sameTypesOrErr retT Void $ printTree stmt
        Cond expr st -> do
            t <- checkExpr expr
            sameTypesOrErr t Bool $ printTree stmt
            checkStmts [st] retT
        CondElse expr st1 st2 -> do
            t <- checkExpr expr
            sameTypesOrErr t Bool $ printTree stmt
            checkStmts [st1] retT
            checkStmts [st2] retT
        While expr st -> do
            t <- checkExpr expr
            sameTypesOrErr t Bool $ printTree stmt
            checkStmts [st] retT
        SExp expr -> checkExpr expr >> return ()
        SFor varT varId arrId st -> do
            arrT <- findType arrId
            case arrT of
                Just (Arr t) -> localF (Map.insert varId varT) $ checkStmts [st] retT
                _ -> throwError $ printTree stmt) --TODO better eMsg
        >> checkStmts rest retT
    

checkDecls :: [Item] -> Type -> Map.Map Ident Type -> TCMonad (Map.Map Ident Type)
checkDecls [] _ map = return map
checkDecls (item:rest) t map = (typeCorrect t (printTree item)) >> (case item of
    NoInit ident -> return $ Map.insert ident t map
    Init ident expr -> do
        exprT <- checkExpr expr
        sameTypesOrErr t exprT $ printTree item
        return $ Map.insert ident t map
    InitAlloc ident objT -> do
        sameTypesOrErr t objT $ printTree item
        return $ Map.insert ident t map
    ArrIAlloc ident arrT expr -> do
        sameTypesOrErr t (Arr arrT) $ printTree item
        exprT <- checkExpr expr
        sameTypesOrErr exprT Int $ printTree item
        return $ Map.insert ident t map)
    >>= checkDecls rest t

typeCorrect :: Type -> String -> TCMonad ()
typeCorrect t eMsg = case t of
    (Class ident) -> classDefined ident eMsg
    (Arr t) -> typeCorrect t eMsg
    (Fun t1 args) -> typeCorrect t1 eMsg >> (mapM_ (flip typeCorrect $ eMsg) args)
    _ -> return ()

classDefined :: Ident -> String -> TCMonad ()
classDefined ident eMsg = do
    env <- asks $ (Map.lookup ident) . snd
    case env of
        Nothing -> throwError $ "Unknown class " ++ (printTree ident) ++ " in " ++ eMsg
        Just _ -> return ()

checkFunc :: [Arg] -> [Stmt] -> Type -> TCMonad ()
checkFunc args body t = localF (foldr (.) id (zipWith Map.insert (map a2id args) (map a2t args))) $ checkStmts body t 

findClassAncs :: Ident -> TCMonad (Maybe [Ident])
findClassAncs ident = asks $ (maybe Nothing (Just . snd)) . (Map.lookup ident) . snd 

findClassEnv :: Ident -> TCMonad (Map.Map Ident Type)
findClassEnv ident = asks $ fst . fromJust . (Map.lookup ident) . snd 

findType :: Ident -> TCMonad (Maybe Type)
findType ident = asks $ (Map.lookup ident) . fst

sameTypesOrErr :: Type -> Type -> String -> TCMonad ()
sameTypesOrErr t1 t2 eMsg = if t1 == t2 then return () else checkAncs t1 t2 $ "can't match types " ++ (printTree t1) ++ (printTree t2) ++ " in " ++ eMsg

sameTypesOrErr' :: String -> Type -> Type -> TCMonad ()
sameTypesOrErr' eMsg t1 t2 = sameTypesOrErr t1 t2 eMsg

checkAncs :: Type -> Type -> String -> TCMonad ()
checkAncs (Class c1) (Class c2) eMsg = findClassAncs c2 >>= \ancs -> case ancs of
    Nothing -> throwError eMsg
    Just ancs -> if elem c1 ancs then return () else throwError $ eMsg
checkAncs (Arr t1) (Arr t2) eMsg = checkAncs t1 t2 eMsg
checkAncs _ _ eMsg = throwError eMsg

startEnv :: TCEnv
startEnv = (Map.fromList [(Ident "printInt", Fun Void [Int]),
    (Ident "printString", Fun Void [Str]), (Ident "error", Fun Void []),
    (Ident "readInt", Fun Int []), (Ident "readString", Fun Str [])], Map.empty)

localF :: (Map.Map Ident Type -> Map.Map Ident Type) -> TCMonad a -> TCMonad a
localF func monad = local (\(funs, clses) -> (func funs, clses)) monad

a2t :: Arg -> Type
a2t (Arg t _) = t

a2id :: Arg -> Ident
a2id (Arg _ ident) = ident

i2id :: Item -> Ident
i2id it = case it of
    NoInit id -> id
    Init id _ -> id
    InitAlloc id _ -> id
