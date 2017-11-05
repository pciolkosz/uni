module LLVMAssembly where

import AbsInstant
import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Except

type CompilerEnv = (Map.Map String Int, Int)

type CompilerMonad = StateT CompilerEnv (ExceptT String IO)

runCompiler :: CompilerMonad String -> IO (Either String String)
runCompiler monad =
    runExceptT (evalStateT monad emptyEnv)

prepareMonad :: [Stmt] -> CompilerMonad String 
prepareMonad [] = return ""
prepareMonad (stmt:rest) = do
    stmtStr <- compileStmt stmt 
    restStr <- prepareMonad rest 
    return $ stmtStr ++ restStr

header=const $ "@dnl = internal constant [4 x i8] c\"%d\\0A\\00\"\n" ++
        "declare i32 @printf(i8*, ...)\n" ++
        "define void @printInt(i32 %x) {\n" ++
        "entry: %t0 = getelementptr [4 x i8], [4 x i8]* @dnl, i32 0, i32 0\n" ++
            "call i32 (i8*, ...) @printf(i8* %t0, i32 %x)\n" ++
            "ret void\n" ++
        "}\n\n" ++
        "define i32 @main() {\n"
footer="ret i32 0}\n"

compileStmt :: Stmt -> CompilerMonad String
compileStmt stmt =
    case stmt of
        SExp exp -> do
            (expStr, loc) <- compileExp exp
            return $ expStr ++ "call void @printInt(i32 " ++ loc ++ ")\n"
        SAss (Ident ident) exp -> do
            (expStr, resLoc) <- compileExp exp
            (var_id, var_str) <- findVar ident
            return $ var_str ++ expStr ++ "store i32 " ++ resLoc ++ ", i32* %var_" ++ var_id ++ "\n"

findVar :: String -> CompilerMonad (String, String)
findVar ident = do
    (env, asmEnv) <- get
    case Map.lookup ident env of
        Nothing ->
            put (Map.insert ident (Map.size env) env, asmEnv) >>
            let var_id = show $ Map.size env in 
                return (var_id, "%var_" ++ var_id ++ " = alloca i32\n")
        Just vid -> 
            return (show vid, "")

compileExp :: Exp -> CompilerMonad (String, String)
compileExp exp = do
    (env, loc_id) <- get
    put (env, loc_id + 1)
    let loc = "%tmp_" ++ (show loc_id) in
        case exp of
            ExpAdd e1 e2 -> compileBinOp e1 e2 loc "add"
            ExpSub e1 e2 -> compileBinOp e1 e2 loc "sub"
            ExpMul e1 e2 -> compileBinOp e1 e2 loc "mul"
            ExpDiv e1 e2 -> compileBinOp e1 e2 loc "sdiv"
            ExpLit i -> return ("", show i)
            ExpVar (Ident ident) -> do
                var_id <- gets $ Map.lookup ident . fst
                case var_id of
                    Nothing -> throwError $ "Read before use\n"
                    Just vid -> return $ (loc ++ " = load i32, i32* %var_" ++ (show vid) ++ "\n", loc) 

compileBinOp :: Exp -> Exp -> String -> String -> CompilerMonad (String, String)
compileBinOp e1 e2 resLoc op = do 
    (str1, loc1) <- compileExp e1
    (str2, loc2) <- compileExp e2 
    let str3 = resLoc ++ " = " ++ op ++ " i32 " ++ loc1 ++ ", " ++ loc2 ++ "\n" in 
        return $ (str1 ++ str2 ++ str3, resLoc)

emptyEnv :: CompilerEnv
emptyEnv = (Map.empty, 0)
    
