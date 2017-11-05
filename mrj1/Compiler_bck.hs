module Compiler where

import AbsInstant
import qualified Data.Map as Map
import Control.Monad.Reader
import Control.Monad.Except
import System.IO
import CompilerConstants

type CompilerEnv = Map.Map String Int

type CompilerMonad = ReaderT CompilerEnv (ExceptT String IO)

compile :: Handle -> Program -> CompilerStrings -> IO ()
compile outFile (Prog stmts) strs = do
    hPutStr outFile $ header strs
    result <- runCompiler $ prepareMonad outFile stmts strs
    hPutStr outFile $ footer strs
    case result of
        Left s -> hPutStrLn stderr $ "Compile Error:\n" ++ s
        Right _ -> return ()

runCompiler :: CompilerMonad () -> IO (Either String ())
runCompiler monad = 
    runExceptT (runReaderT monad emptyEnv)

prepareMonad :: Handle -> [Stmt] -> CompilerStrings -> CompilerMonad ()
prepareMonad _ [] _ = return ()
prepareMonad handle (stmt:rest) strs = compileStmt stmt strs >>= (\(out, envTrans) -> 
    (liftIO $ hPutStr handle out) >> (local envTrans $ prepareMonad handle rest strs))

compileStmt :: Stmt -> CompilerStrings -> CompilerMonad (String, CompilerEnv -> CompilerEnv)
compileStmt stmt strs = 
    case stmt of
        SExp exp -> (compileExp exp strs >>= (\expStr -> return ((expHeader strs) ++ expStr ++ (expFooter strs), id)))
        SAss (Ident ident) exp -> 
            (compileExp exp strs >>= (\expStr -> do
                env <- ask
                case Map.lookup ident env of
                    Nothing -> return (expStr ++ (store strs $ show (Map.size env)), Map.insert ident (Map.size env))
                    Just var_id -> return (expStr ++ (store strs $ show var_id), id)))

compileExp :: Exp -> CompilerStrings -> CompilerMonad String
compileExp exp strs = case exp of
    ExpAdd e1 e2 -> compileOp e1 e2 strs $ addE strs "" ""
    ExpSub e1 e2 -> compileOp e1 e2 strs $ subE strs "" ""
    ExpMul e1 e2 -> compileOp e1 e2 strs $ mulE strs "" ""
    ExpDiv e1 e2 -> compileOp e1 e2 strs $ divE strs "" ""
    ExpLit i -> return $ lit strs $ show i
    ExpVar (Ident ident) -> do
        var_id <- asks $ Map.lookup ident 
        case var_id of
            Nothing -> throwError $ "Read before use\n"
            Just id -> return $ load strs $ show id

compileOp :: Exp -> Exp -> CompilerStrings -> String -> CompilerMonad String
compileOp e1 e2 strs op = compileExp e1 strs >>= \str1 -> compileExp e2 strs >>= \str2 -> return $ str1 ++ str2 ++ op

emptyEnv :: CompilerEnv
emptyEnv = Map.empty

