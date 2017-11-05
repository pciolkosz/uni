module JvmCompiler where

import AbsInstant
import qualified Data.Map as Map
import Control.Monad.Reader
import Control.Monad.Except
import System.IO

type CompilerEnv = Map.Map String Int

type CompilerMonad = ReaderT CompilerEnv (ExceptT String IO)

compileJVM :: Handle -> Program -> IO ()
compileJVM outFile (Prog stmts) = do
    hPutStr outFile ".class  public Hello\n.super  java/lang/Object\n\n.method public <init>()V\naload_0\ninvokespecial java/lang/Object/<init>()V\nreturn\n.end method\n\n.method public static main([Ljava/lang/String;)V\n.limit stack 1000\n.limit locals 1000\n"
    result <- runCompiler $ prepareJvmMonad outFile stmts
    hPutStr outFile "return\n.end method\n"
    case result of
        Left s -> hPutStrLn stderr $ "Compile Error:\n" ++ s
        Right _ -> return ()

runCompiler :: CompilerMonad () -> IO (Either String ())
runCompiler monad = 
    runExceptT (runReaderT monad emptyEnv)

prepareJvmMonad :: Handle -> [Stmt] -> CompilerMonad ()
prepareJvmMonad _ [] = return ()
prepareJvmMonad handle (stmt:rest) = compileStmt stmt >>= (\(out, envTrans) -> 
    (liftIO $ hPutStr handle out) >> (local envTrans $ prepareJvmMonad handle rest))

compileStmt :: Stmt -> CompilerMonad (String, CompilerEnv -> CompilerEnv)
compileStmt stmt = 
    case stmt of
        SExp exp -> (compileExp exp >>= (\expStr -> return ("getstatic java/lang/System/out Ljava/io/PrintStream;\n" ++ expStr ++ "invokevirtual java/io/PrintStream/println(I)V\n", id)))
        SAss (Ident ident) exp -> 
            (compileExp exp >>= (\expStr -> do
                env <- ask
                case Map.lookup ident env of
                    Nothing -> return (expStr ++ "istore " ++ show (Map.size env) ++ "\n", Map.insert ident (Map.size env))
                    Just var_id -> return (expStr ++ "istore " ++ show var_id ++ "\n", id)))

compileExp :: Exp -> CompilerMonad String
compileExp exp = case exp of
    ExpAdd e1 e2 -> compileExp e1 >>= \str1 -> compileExp e2 >>= \str2 -> return $ str1 ++ str2 ++ "iadd\n"
    ExpSub e1 e2 -> compileExp e1 >>= \str1 -> compileExp e2 >>= \str2 -> return $ str1 ++ str2 ++ "isub\n"
    ExpMul e1 e2 -> compileExp e1 >>= \str1 -> compileExp e2 >>= \str2 -> return $ str1 ++ str2 ++ "imul\n"
    ExpDiv e1 e2 -> compileExp e1 >>= \str1 -> compileExp e2 >>= \str2 -> return $ str1 ++ str2 ++ "idiv\n"
    ExpLit i -> return $ "bipush " ++ show i ++ "\n"
    ExpVar (Ident ident) -> do
        var_id <- asks $ Map.lookup ident 
        case var_id of
            Nothing -> throwError $ "Read before use\n"
            Just id -> return $ "iload " ++ show id ++ "\n"
    

emptyEnv :: CompilerEnv
emptyEnv = Map.empty

