module Compiler where

{-# LANGUAGE CPP #-}
import AbsInstant
import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Except
import System.IO

#ifdef JVM
import JVMAssembly
#else
import LLVMAssembly
#endif

compile :: Handle -> Program -> IO ()
compile outFile (Prog stmts) = 
    let (processedProg, misc) = preprocessProg stmts in do
        hPutStr outFile $ header "Hello"
        hPutStr outFile misc
        result <- runCompiler $ prepareMonad outFile processedProg
        hPutStr outFile footer 
        case result of
            Left s -> hPutStrLn stderr $ "Compile Error:\n" ++ s
            Right _ -> return ()

runCompiler :: CompilerMonad () -> IO (Either String ())
runCompiler monad = 
    runExceptT (evalStateT monad emptyEnv)

prepareMonad :: Handle -> [Stmt] -> CompilerMonad ()
prepareMonad _ [] = return ()
prepareMonad handle (stmt:rest) = compileStmt stmt >>= 
    (\out -> liftIO $ hPutStr handle out) >> prepareMonad handle rest --todo moze da sie lepiej to napisac

