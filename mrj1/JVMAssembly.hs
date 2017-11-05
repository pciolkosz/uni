module JVMAssembly where

import AbsInstant
import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Except

type CompilerEnv = (Map.Map String Int, Int, Int)

type CompilerMonad = StateT CompilerEnv (ExceptT String IO)

runCompiler :: CompilerMonad String -> IO (Either String String)
runCompiler monad =
    runExceptT (evalStateT (monad >>= calcLimits) emptyEnv)

calcLimits :: String -> CompilerMonad String
calcLimits progStr = do
    (_, stack, locals) <- get
    return $ ".limit stack " ++ (show $ stack + 2) ++
        "\n.limit locals " ++ (show locals) ++ "\n" ++ progStr

prepareMonad :: [Stmt] -> CompilerMonad String 
prepareMonad [] = return ""
prepareMonad (stmt:rest) = do
    stmtStr <- compileStmt stmt 
    restStr <- prepareMonad rest 
    return $ stmtStr ++ restStr

header= \filename -> 
        ".class  public " ++ filename ++ "\n" ++
        ".super  java/lang/Object\n" ++
        "\n" ++
        ".method public <init>()V\n" ++
            "aload_0\n" ++
            "invokespecial java/lang/Object/<init>()V\n" ++
            "return\n" ++ 
            ".end method\n" ++
        "\n" ++ 
        ".method public static main([Ljava/lang/String;)V\n"
expHeader="getstatic java/lang/System/out Ljava/io/PrintStream;\n"
expFooter="invokevirtual java/io/PrintStream/println(I)V\n"
footer="return\n.end method\n"

checkNewStack :: Int -> CompilerMonad ()
checkNewStack newStack = modify (\(env, oldStack, vars) ->
    (env, if newStack > oldStack then newStack else oldStack, vars))

compileStmt :: Stmt -> CompilerMonad String
compileStmt stmt =
    case stmt of
        SExp exp -> do
            (expStr, stackS) <- compileExp exp
            checkNewStack stackS
            return $ expHeader ++ expStr ++ expFooter
        SAss (Ident ident) exp -> do
            (expStr, stackS) <- compileExp exp
            checkNewStack stackS
            var_id <- findVar ident
            return $ expStr ++ "istore " ++ (show var_id) ++ "\n"

findVar :: String -> CompilerMonad Int 
findVar ident = do
    (env, locals, vars) <- get
    case Map.lookup ident env of
        Nothing ->
            (put (Map.insert ident (Map.size env) env, locals, vars + 1)) >>
            (return $ Map.size env)
        Just vid ->
            return vid


compileExp :: Exp -> CompilerMonad (String, Int)
compileExp exp = do
    (env, _, _) <- get
    case exp of
        ExpAdd e1 e2 -> compileOp e1 e2 "iadd\n" True
        ExpSub e1 e2 -> compileOp e1 e2 "isub\n" False
        ExpMul e1 e2 -> compileOp e1 e2 "imul\n" True
        ExpDiv e1 e2 -> compileOp e1 e2	"idiv\n" False
        ExpLit i -> return ("bipush " ++ (show i) ++ "\n", 1)
        ExpVar (Ident ident) -> do
            var_id <- gets (\(env, _, _) ->  Map.lookup ident env)
            case var_id of
                Nothing -> throwError $ "Read before use\n"
                Just vid -> return ("iload " ++ (show vid) ++ "\n", 1)

compileOp :: Exp -> Exp -> String -> Bool -> CompilerMonad (String, Int)
compileOp e1 e2 op commutative = do
    (str1, stck1) <- compileExp e1
    (str2, stck2) <- compileExp e2
    if commutative && (stck1 < stck2)  then
        return (str2 ++ str1 ++ op, stck1 + 1)
    else
        return (str1 ++ str2 ++ op, stck2 + 1)

emptyEnv :: CompilerEnv
emptyEnv = (Map.empty, 0, 1)

