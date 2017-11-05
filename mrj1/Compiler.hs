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
compile outFile (Prog stmts) = do 
    hPutStr outFile $ header "Hello"
    result <- runCompiler $ prepareMonad stmts 
    case result of
        Left s -> hPutStrLn stderr $ "Compile Error:\n" ++ s
        Right progStr -> hPutStr outFile $ progStr ++ footer

