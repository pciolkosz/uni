module TypeCheck where

import Control.Monad.Except
import Data.Either
import Control.Monad.Reader
import qualified Data.Map as Map
import Data.Maybe
import AbsLatte
import PrintLatte
import System.IO (stderr, hPutStr)

type TCMonad = ReaderT TCEnv (Except String)

type TCEnv = (Map.Map Ident Type, Map.Map Ident (Map.Map Ident Type, [Ident]), [Ident])

checkTypes :: Program -> IO (Bool, Map.Map Ident Type)
checkTypes (Program defs) = do 
    case runExcept $ prepareTopEnv defs startEnv of
        Left errMsg -> putStr errMsg >> return (False, Map.empty)
        Right env -> case runExcept $ runReaderT (prepareTCMonad defs) env of
            Left errMsg -> hPutStr stderr ("Error\n" ++ errMsg ++ "\n")  >> return (False, Map.empty)
            Right _ -> return (True, fst3 env)

prepareTopEnv :: [TopDef] -> TCEnv -> Except String TCEnv 
prepareTopEnv [] env = return env
prepareTopEnv (def:rest) (vEnv, cEnv, d) =
    let envMonad = case def of {
        FnDef t ident args _ -> return (Map.insert ident (Fun t (map a2t args)) vEnv, cEnv, d);
        ClDef ident members -> return (vEnv, Map.insert ident (prepClsTopEnv members, []) cEnv, d);
        DepClDef ident anc members -> case Map.lookup anc cEnv of
            Just (ancEnv, superClses) -> return (vEnv, Map.insert ident (Map.union (prepClsTopEnv members) ancEnv, (anc:superClses)) cEnv, d)
            Nothing -> throwError $ "Class " ++ (printTree ident) ++ " can't extend unknown class " ++ (printTree anc) ++ "\n"} 
    in envMonad >>= \newEnv -> prepareTopEnv rest newEnv

prepareTCMonad :: [TopDef] -> TCMonad ()
prepareTCMonad [] = return ()
prepareTCMonad (def:rest) = case def of {
    FnDef t _ args (Block body) -> catchError (checkFunc args body t) (\error -> throwError $ error ++ "\n in function\n" ++ (printTree def));
    ClDef ident members -> findClassEnv ident >>= \env -> localF (Map.union env) [] $ checkMembers members;
    DepClDef ident anc members -> findClassEnv ident >>= \env -> localF (Map.union env) [] $ checkMembers members
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
        Init ident expr -> checkExprErrMsg expr >>= \t2 -> sameTypesOrErr t1 t2
        InitAlloc ident t2 -> sameTypesOrErr t1 t2
        ArrIAlloc ident t2 expr -> do 
            sameTypesOrErr t1 (Arr t2)
            t3 <- checkExprErrMsg expr
            sameTypesOrErr t3 Int)
     items
    MMethod t ident args (Block body) -> checkFunc args body t

checkExpr :: Expr -> TCMonad Type
checkExpr expr = case expr of
    EArrAcc e1 e2 -> do
        t1 <- checkExpr e1
        t2 <- checkExpr e2
        case (t1, t2) of
            (Arr t, Int) -> return t
            (_, Int) -> throwError $ (printTree expr) ++ " is not an array"
            (Arr t, _) -> throwError $ "Array can be indexed only with integer"
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
            Nothing -> throwError $ "Unknown variable " ++ (printTree ident)
    ELitInt _ -> return Int
    ELitTrue -> return Bool
    ELitFalse -> return Bool
    EApp ident params -> do
        t <- findType ident
        case t of
            Just (Fun retT args) -> do
                checkCall params args $ printTree expr
                return retT
            _ -> throwError $ "Unknown function " ++ (printTree ident)
    EString _ -> return Str
    Neg expr -> do
        t <- checkExpr expr
        sameTypesOrErr t Int
        return Int
    Not expr -> do
        t <- checkExpr expr
        sameTypesOrErr t Bool
        return Bool
    EMul e1 _ e2 -> checkBinOp e1 e2 [Int]
    EAdd e1 _ e2 -> checkBinOp e1 e2 [Int, Str]
    ERel e1 op e2 -> if op == E || op == NE 
        then 
            do
                t1 <- checkExpr e1
                t2 <- checkExpr e2
                sameTypesOrErr t1 t2
                return Bool
        else
            (checkBinOp e1 e2 [Int]) >> return Bool
    EAnd e1 e2 -> checkBinOp e1 e2 [Bool]
    EOr e1 e2 -> checkBinOp e1 e2 [Bool]
   
checkCall :: [Expr] -> [Type] -> String -> TCMonad ()
checkCall exprs types errMsg = do 
    args <- mapM checkExprErrMsg exprs
    unless (length exprs == length types) $  throwError $ "Wrong number of parameters in " ++ errMsg
    foldl (>>) (return ()) $ zipWith sameTypesOrErr types args

checkBinOp :: Expr -> Expr -> [Type] -> TCMonad Type
checkBinOp e1 e2 types = do
    t1 <- checkExpr e1    
    t2 <- checkExpr e2
    if t1 == t2 && (elem t1 types) then
        return t1
    else
        throwError  ("Can't match type " ++ (show t1) ++ " of expression " ++ (printTree e1) ++
            " with type " ++ (show t2) ++ " of expression " ++ (printTree e2))

checkStmts :: [Stmt] -> Type -> TCMonad ()
checkStmts [] _ = return ()
checkStmts (stmt:rest) retT = case stmt of 
    Decl t items -> let newIds = (map i2id items) in let repId = hasRepeated newIds in do 
        unless (repId == Nothing) $ throwError $ "Repeated declaration " ++ (show repId)
        env <- ask
        newEnv <- catchError (checkDecls items t . fst3 $ env) (\error -> throwError $ error ++ "\nin declaration\n" ++ (printTree stmt))
        localF (const newEnv) (newIds) $ checkStmts rest retT
    _ -> (case stmt of
        Cond expr st -> do
            t <- checkExprErrMsg expr
            sameTypesOrErr t Bool
            checkStmts [st] retT
        CondElse expr st1 st2 -> do
            t <- checkExprErrMsg expr
            sameTypesOrErr t Bool
            checkStmts [st1] retT
            checkStmts [st2] retT
        While expr st -> do
            t <- checkExprErrMsg expr
            sameTypesOrErr t Bool
            checkStmts [st] retT
        SFor varT varId arrId st -> do
            arrT <- findType arrId
            case arrT of
                Just (Arr t) -> localF (Map.insert varId varT) [] $ checkStmts [st] retT
                _ -> throwError $ printTree stmt
        BStmt (Block stmts) -> catchError (local (\(v, c, _) -> (v, c, [])) $ checkStmts stmts retT)
            (\error -> throwError $ error ++ "\n in block\n" ++ (printTree stmt))
        _ -> (catchError (case stmt of
            Alloc expr t1 -> do
                t2 <- checkExprErrMsg expr
                sameTypesOrErr t2 t1
            ArrAlloc e1 t1 e2 -> do
                t2 <- checkExprErrMsg e1
                t3 <- checkExprErrMsg e2
                sameTypesOrErr t2 (Arr t1)
                sameTypesOrErr t3 Int
            Empty -> return ()
            Ass e1 e2 -> do
                t1 <- checkExprErrMsg e1
                t2 <- checkExprErrMsg e2
                sameTypesOrErr t1 t2
            Incr ident -> do
                t <- findType ident
                case t of
                    Just t -> sameTypesOrErr t Int
                    Nothing -> throwError $ "Unknown variable " ++ (printTree ident)
            Decr ident -> do
                t <- findType ident
                case t of
                    Just t -> sameTypesOrErr t Int
                    Nothing -> throwError $ "Unknown variable " ++ (printTree ident)
            Ret expr -> do
                t <- checkExprErrMsg expr
                sameTypesOrErr retT t
            VRet -> sameTypesOrErr retT Void
            SExp expr -> checkExprErrMsg expr >> return ()
            ) (\error -> throwError $ error ++ "\n in instruction\n" ++ (printTree stmt)))) >> checkStmts rest retT
    

checkExprErrMsg :: Expr -> TCMonad Type
checkExprErrMsg e = catchError (checkExpr e) (\error -> throwError $ error ++ "\n in expression\n" ++ (printTree e))

checkDecls :: [Item] -> Type -> Map.Map Ident Type -> TCMonad (Map.Map Ident Type)
checkDecls [] _ map = return map
checkDecls (item:rest) t map = (typeCorrect t (printTree item)) >> ask >>= \(_, _, d) -> (case item of
    NoInit ident -> do 
        when (elem ident d) $ throwError $ "Redeclaration of " ++ (show ident)
        return $ Map.insert ident t map
    Init ident expr -> do
        when (elem ident d) $ throwError $ "Redeclaration of " ++ (show ident)
        exprT <- checkExprErrMsg expr
        sameTypesOrErr t exprT
        return $ Map.insert ident t map
    InitAlloc ident objT -> do
        when (elem ident d) $ throwError $ "Redeclaration of " ++ (show ident)
        sameTypesOrErr t objT
        return $ Map.insert ident t map
    ArrIAlloc ident arrT expr -> do
        when (elem ident d) $ throwError $ "Redeclaration of " ++ (show ident)
        sameTypesOrErr t (Arr arrT)
        exprT <- checkExprErrMsg expr
        sameTypesOrErr exprT Int
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
    env <- asks $ (Map.lookup ident) . snd3
    case env of
        Nothing -> throwError $ "Unknown class " ++ (printTree ident) ++ " in " ++ eMsg
        Just _ -> return ()

checkFunc :: [Arg] -> [Stmt] -> Type -> TCMonad ()
checkFunc args body t = let paramIds = map a2id args in
    let repId = hasRepeated paramIds in 
        unless (repId == Nothing) (throwError $ "Repeated function parameter " ++ (show $ fromJust repId)) >>
            (localF (foldr (.) id (zipWith Map.insert paramIds (map a2t args))) [] $ checkStmts body t) >> 
                unless (t == Void) (if null body then throwError "No return statement" else checkReturn $ last body)

hasRepeated :: [Ident] -> Maybe Ident
hasRepeated [] = Nothing
hasRepeated (x:xs) = if elem x xs then Just x else hasRepeated xs

checkReturn :: Stmt -> TCMonad ()
checkReturn stmt = case stmt of
    BStmt (Block stmts) -> if null stmts then throwError "No return statement" else checkReturn $ last stmts
    Ret _ -> return ()
    Cond e stmt -> if e == ELitTrue then checkReturn stmt else throwError "No return statement"
    CondElse e iSt eSt -> (unless (e == ELitFalse) (checkReturn iSt)) >> unless (e == ELitTrue) (checkReturn eSt)
    While e stmt -> if e /= ELitFalse then checkReturn stmt else throwError "No return statement"
    _ -> throwError "No return statement"

findClassAncs :: Ident -> TCMonad (Maybe [Ident])
findClassAncs ident = asks $ (maybe Nothing (Just . snd)) . (Map.lookup ident) . snd3 

findClassEnv :: Ident -> TCMonad (Map.Map Ident Type)
findClassEnv ident = asks $ fst . fromJust . (Map.lookup ident) . snd3 

findType :: Ident -> TCMonad (Maybe Type)
findType ident = asks $ (Map.lookup ident) . fst3

sameTypesOrErr :: Type -> Type -> TCMonad ()
sameTypesOrErr t1 t2 = if t1 == t2 then return () else checkAncs t1 t2 $ "can't match types " ++ (printTree t1) ++ " and " ++ (printTree t2)

checkAncs :: Type -> Type -> String -> TCMonad ()
checkAncs (Class c1) (Class c2) eMsg = findClassAncs c2 >>= \ancs -> case ancs of
    Nothing -> throwError eMsg
    Just ancs -> if elem c1 ancs then return () else throwError $ eMsg
checkAncs (Arr t1) (Arr t2) eMsg = checkAncs t1 t2 eMsg
checkAncs _ _ eMsg = throwError eMsg

startEnv :: TCEnv
startEnv = (Map.fromList [(Ident "printInt", Fun Void [Int]),
    (Ident "printString", Fun Void [Str]), (Ident "error", Fun Void []),
    (Ident "readInt", Fun Int []), (Ident "readString", Fun Str [])], Map.empty, [])

localF :: (Map.Map Ident Type -> Map.Map Ident Type) -> [Ident] -> TCMonad a -> TCMonad a
localF func idents monad = local (\(funs, clses, d) -> (func funs, clses, d ++ idents)) monad

a2t :: Arg -> Type
a2t (Arg t _) = t

a2id :: Arg -> Ident
a2id (Arg _ ident) = ident

i2id :: Item -> Ident
i2id it = case it of
    NoInit id -> id
    Init id _ -> id
    InitAlloc id _ -> id

fst3 (x, _, _) = x
snd3 (_, x, _) = x
trd3 (_, _, x) = x
