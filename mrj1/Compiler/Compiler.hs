module Compiler where

{-# LANGUAGE CPP #-}
import AbsInstant
import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Except
import System.IO
import System.FilePath.Posix

#ifdef JVM
import JVMAssembly
#else
import LLVMAssembly
#endif

compile :: FilePath -> Program -> IO ()
compile path (Prog stmts) = let outPath = replaceExtension path outExt in 
    do 
        fHandle <- openFile outPath WriteMode
        hPutStr fHandle $ header $ takeBaseName path
        result <- runCompiler $ prepareMonad stmts 
        case result of
            Left s -> hPutStrLn stderr $ "Compile Error:\n" ++ s
            Right progStr -> hPutStr fHandle $ progStr ++ footer
        hClose fHandle
