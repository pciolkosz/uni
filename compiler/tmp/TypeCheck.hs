module TypeCheck where

import Control.Monad.Except
import Data.Either
import Control.Monad.Reader
import qualified Data.Map as Map
import Data.Maybe
import AbsLatte


type TCMonad = ReaderT TCEnv (Except String)

type TCEnv = (Map.Map Ident Type, Map.Map Ident (Map.Map Ident Type))

checkTypes :: Program -> IO Bool
checkTypes (Program defs) = do 
    case runExcept $ runReaderT (prepareTCMonad defs) $ prepareTopEnv defs startEnv of
        Left errMsg -> putStr errMsg >> return False
        Right _ -> return True

prepareTopEnv :: [TopDef] -> TCEnv -> TCEnv --TODO tutaj dać samego excepta
prepareTopEnv [] env = env
prepareTopEnv (def:rest) (vEnv, cEnv) = 
    prepareTopEnv rest $ case def of {
        FnDef t ident args _ -> (Map.insert ident (Fun t (map a2t args)) vEnv, cEnv);
        ClDef ident members -> (vEnv, Map.insert ident (prepClsTopEnv members) cEnv);
        DepClDef ident anc members -> case Map.lookup anc cEnv of
            Just ancEnv -> (vEnv, Map.insert ident (Map.union (prepClsTopEnv members) ancEnv) cEnv)
            Nothing -> (Map.empty, Map.empty)} --TODO error handling

prepareTCMonad :: [TopDef] -> TCMonad ()
prepareTCMonad [] = return ()
prepareTCMonad (def:rest) = case def of {
    FnDef t _ args (Block body) ->  checkFunc args body t;
    ClDef ident members -> findClassEnv ident >>= \env -> localF (Map.union env) $ checkMembers members;
    DepClDef ident anc members -> findClassEnv ident >>= \env -> localF (Map.union env) $ checkMembers members
}--TODO last to cases are the same

prepClsTopEnv :: [MemberDef] -> Map.Map Ident Type
prepClsTopEnv [] = Map.empty
prepClsTopEnv (def:rest) = case def of
    MAttr t items -> foldr (flip Map.insert $ t) (prepClsTopEnv rest) $ map i2id items --TODO is foldl faster?
    MMethod t id args _ -> Map.insert id (Fun t $ map a2t args) $ prepClsTopEnv rest

checkMembers :: [MemberDef] -> TCMonad ()
checkMembers [] = return ()
checkMembers (def:rest) = case def of
    MAttr _ items -> mapM_ (\item -> case item of
        NoInit _ -> return ()
        Init ident expr -> checkExpr expr >>= \t1 -> findType ident >>= \(Just t2) -> sameTypesOrErr t1 t2 $ show def
        InitAlloc ident t1 -> findType ident >>= \(Just t2) -> sameTypesOrErr t1 t2 $ show def
        ArrIAlloc ident t1 expr -> checkExpr expr >>= \t2 -> sameTypesOrErr t1 t2 $ show def)
     items
    MMethod t ident args (Block body) -> checkFunc args body t

checkExpr :: Expr -> TCMonad Type
checkExpr expr = case expr of
    EArrAcc e1 e2 -> do
        t1 <- checkExpr e1
        t2 <- checkExpr e2
        case (t1, t2) of
            (Arr t, Int) -> return t
            _ -> throwError $ show expr
    ENull t -> case t of
        Class id -> return t
        _ -> throwError $ show expr
    EMember e memId -> do
        t <- checkExpr e
        case t of
            Class clsId -> do
                env <- findClassEnv clsId
                case (Map.lookup memId env) of
                    Just t -> return t
                    Nothing -> throwError $ show expr
            Arr _ -> do
                if memId == Ident "length" then
                    return Int
                else
                    throwError $ show expr
            _ -> throwError $ show expr
    EMethod obj ident params -> do
        t <- checkExpr obj
        case t of
            Class classId -> do
                env <- findClassEnv classId
                case Map.lookup ident env of
                    Just (Fun retT args) -> do
                        checkCall params args $ show expr
                        return retT
                    _ -> throwError $ show expr
            _ -> throwError $ show expr
    EVar ident -> do
        varT <- findType ident
        case varT of
            Just t -> return t
            Nothing -> throwError $ show expr
    ELitInt _ -> return Int
    ELitTrue -> return Bool
    ELitFalse -> return Bool
    EApp ident params -> do
        t <- findType ident
        case t of
            Just (Fun retT args) -> do
                checkCall params args $ show expr
                return retT
            _ -> throwError $ show expr
    EString _ -> return Str
    Neg expr -> do
        t <- checkExpr expr
        sameTypesOrErr t Int $ show expr
        return Int
    Not expr -> do
        t <- checkExpr expr
        sameTypesOrErr t Bool $ show expr
        return Bool
    EMul e1 _ e2 -> checkBinOp e1 e2 [Int] $ show expr
    EAdd e1 _ e2 -> checkBinOp e1 e2 [Int, Str] $ show expr
    ERel e1 _ e2 -> (checkBinOp e1 e2 [Int] $ show expr) >> return Bool
    EAnd e1 e2 -> checkBinOp e1 e2 [Bool] $ show expr
    EOr e1 e2 -> checkBinOp e1 e2 [Bool] $ show expr
   
checkCall :: [Expr] -> [Type] -> String -> TCMonad ()
checkCall exprs types errMsg = do 
    args <- mapM checkExpr exprs
    if types == args then
        return ()
    else
        throwError errMsg
 
checkBinOp :: Expr -> Expr -> [Type] -> String -> TCMonad Type
checkBinOp e1 e2 types errMsg = do
    t1 <- checkExpr e1    
    t2 <- checkExpr e2
    if t1 == t2 && (elem t1 types) then
        return t1
    else
        throwError errMsg

checkStmts :: [Stmt] -> Type -> TCMonad ()
checkStmts [] _ = return ()
checkStmts (stmt:rest) retT = case stmt of
    Decl t items -> ask >>= (checkDecls items t . fst) >>= 
        \newEnv -> localF (const newEnv) $ checkStmts rest retT
    _ -> (case stmt of
        Alloc expr t1 -> do
            t2 <- checkExpr expr
            sameTypesOrErr t1 t2 $ show stmt
        ArrAlloc e1 t1 e2 -> do
            t2 <- checkExpr e1
            t3 <- checkExpr e2
            sameTypesOrErr t2 (Arr t1) $ show stmt
            sameTypesOrErr t3 Int $ show stmt
        Empty -> return ()
        BStmt (Block stmts) -> checkStmts stmts retT
        Ass e1 e2 -> do
            t1 <- checkExpr e1
            t2 <- checkExpr e2
            sameTypesOrErr t1 t2 $ show stmt
        Incr ident -> do
            t <- findType ident
            case t of
                Just t -> sameTypesOrErr t Int $ show stmt
                Nothing -> throwError $ show stmt
        Decr ident -> do
            t <- findType ident
            case t of
                Just t -> sameTypesOrErr t Int $ show stmt
                Nothing -> throwError $ show stmt
        Ret expr -> do
            t <- checkExpr expr
            sameTypesOrErr t retT $ show stmt
        VRet -> sameTypesOrErr retT Void $ show stmt
        Cond expr st -> do
            t <- checkExpr expr
            sameTypesOrErr t Bool $ show stmt
            checkStmts [st] retT
        CondElse expr st1 st2 -> do
            t <- checkExpr expr
            sameTypesOrErr t Bool $ show stmt
            checkStmts [st1] retT
            checkStmts [st2] retT
        While expr st -> do
            t <- checkExpr expr
            sameTypesOrErr t Bool $ show stmt
            checkStmts [st] retT
        SExp expr -> checkExpr expr >> return ()
        SFor varT varId arrId st -> do
            arrT <- findType arrId
            case arrT of
                Just (Arr t) -> localF (Map.insert varId varT) $ checkStmts [st] retT
                _ -> throwError $ show stmt)
        >> checkStmts rest retT
    

checkDecls :: [Item] -> Type -> Map.Map Ident Type -> TCMonad (Map.Map Ident Type)
checkDecls [] _ map = return map
checkDecls (item:rest) t map = (case item of
    NoInit ident -> return $ Map.insert ident t map
    Init ident expr -> do
        exprT <- checkExpr expr
        sameTypesOrErr t exprT $ show item
        return $ Map.insert ident t map
    InitAlloc ident objT -> do
        sameTypesOrErr t objT $ show item
        return $ Map.insert ident t map
    ArrIAlloc ident arrT expr -> do
        sameTypesOrErr t (Arr arrT) $ show item
        exprT <- checkExpr expr
        sameTypesOrErr exprT Int $ show item
        return $ Map.insert ident t map)
    >>= checkDecls rest t

checkFunc :: [Arg] -> [Stmt] -> Type -> TCMonad ()
checkFunc args body t = localF (foldr (.) id (zipWith Map.insert (map a2id args) (map a2t args))) $ checkStmts body t; 

findClassEnv :: Ident -> TCMonad (Map.Map Ident Type)
findClassEnv ident = asks $ fromJust . (Map.lookup ident) . snd

findType :: Ident -> TCMonad (Maybe Type)
findType ident = asks $ (Map.lookup ident) . fst

sameTypesOrErr :: Type -> Type -> String -> TCMonad ()
sameTypesOrErr t1 t2 eMsg = if t1 == t2 then return () else throwError eMsg

startEnv :: TCEnv
startEnv = (Map.insert (Ident "printInt") (Fun Void [Int]) Map.empty, Map.empty) --TODO dodać printy i ready

--localC :: (TCEnv -> TCEnv) -> TCMonad a -> TCMonad a
--localC func monad = local (\(funs, clses) -> (funs, func clses)) monad

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
