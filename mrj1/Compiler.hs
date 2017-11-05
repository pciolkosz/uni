module Compiler where

import AbsInstant
import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Except
import System.IO
import CompilerConstants

type CompilerEnv = (Map.Map String Int, Int)

type CompilerMonad = StateT CompilerEnv (ExceptT String IO)

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
    runExceptT (evalStateT monad emptyEnv)

prepareMonad :: Handle -> [Stmt] -> CompilerStrings -> CompilerMonad ()
prepareMonad _ [] _ = return ()
prepareMonad handle (stmt:rest) strs = compileStmt stmt strs >>= (\out -> 
    (liftIO $ hPutStr handle out) >> prepareMonad handle rest strs)--todo moze da sie lepiej to napisac

compileStmt :: Stmt -> CompilerStrings -> CompilerMonad String
compileStmt stmt strs = 
    case stmt of
        SExp exp -> (compileExp exp strs >>= (\(expStr, loc) -> return $ (expHeader strs) ++ expStr ++ (expFooter strs loc)))
        SAss (Ident ident) exp -> 
            (compileExp exp strs >>= (\(expStr, resLoc) -> do
                (env, exp_id) <- get
                case Map.lookup ident env of
                    Nothing -> put (Map.insert ident (Map.size env) env, exp_id) >> (return $ newVariable strs (show $ Map.size env) ++ expStr ++ (fst (store strs (show $ Map.size env) resLoc)))
                    Just var_id -> return $ expStr ++ (fst (store strs (show var_id) resLoc))))

compileExp :: Exp -> CompilerStrings -> CompilerMonad (String, String)
compileExp exp strs = do
    (env, exp_id) <- get
    put (env, exp_id + 1)
    let loc = newExpLoc strs $ exp_id in
        case exp of
            ExpAdd e1 e2 -> compileOp e1 e2 loc strs $ addE strs
            ExpSub e1 e2 -> compileOp e1 e2 loc strs $ subE strs
            ExpMul e1 e2 -> compileOp e1 e2 loc strs $ mulE strs
            ExpDiv e1 e2 -> compileOp e1 e2 loc strs $ divE strs
            ExpLit i -> return $ lit strs $ show i
            ExpVar (Ident ident) -> do
                var_id <- gets $ Map.lookup ident . fst
                case var_id of
                    Nothing -> throwError $ "Read before use\n"
                    Just id -> return $ (load strs) (show id) loc

compileOp :: Exp -> Exp -> String -> CompilerStrings -> BinOp -> CompilerMonad (String, String)
compileOp e1 e2 loc3 strs op = compileExp e1 strs >>= \(str1, loc1) -> compileExp e2 strs >>= \(str2, loc2) -> let (str3, loc4) = op loc1 loc2 loc3 in return $ (str1 ++ str2 ++ str3, loc3)

emptyEnv :: CompilerEnv
emptyEnv = (Map.empty, 0)

