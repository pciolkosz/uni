module JVMAssembly where

import AbsInstant
import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Except

type CompilerEnv = Map.Map String Int

type CompilerMonad = StateT CompilerEnv (ExceptT String IO)

preprocessProg :: [Stmt] -> ([Stmt], String)
preprocessProg stmts = let stackLimit = maximum $ map calcStackStmt stmts in
    let varLimit = sum $ map calcVars stmts in
        (stmts, ".limit stack " ++ (show stackLimit) ++ 
        "\n.limit locals " ++ (show varLimit) ++ "\n")

calcStackStmt :: Stmt -> Int
calcStackStmt stmt = case stmt of
    SExp exp -> calcStackExp exp
    SAss ident exp -> calcStackExp exp

calcStackExp :: Exp -> Int
calcStackExp exp = case exp of
    ExpAdd e1 e2 -> max (calcStackExp e1) (calcStackExp e2 + 1)
    ExpSub e1 e2 -> max (calcStackExp e1) (calcStackExp e2 + 1)
    ExpMul e1 e2 -> max (calcStackExp e1) (calcStackExp e2 + 1)
    ExpDiv e1 e2 -> max (calcStackExp e1) (calcStackExp e2 + 1)
    ExpLit l -> 1
    ExpVar v -> 1

calcVars = const 1000

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


compileStmt :: Stmt -> CompilerMonad String
compileStmt stmt =
    case stmt of
        SExp exp -> do
            expStr <- compileExp exp
            return $ expHeader ++ expStr ++ expFooter
        SAss (Ident ident) exp -> do
            expStr <- compileExp exp
            var_id <- findVar ident
            return $ expStr ++ "istore " ++ (show var_id) ++ "\n"

findVar :: String -> CompilerMonad Int 
findVar ident = do
    env <- get
    case Map.lookup ident env of
        Nothing ->
            (put $ Map.insert ident (Map.size env) env) >>
            (return $ Map.size env)
        Just vid ->
            return vid


compileExp :: Exp -> CompilerMonad String
compileExp exp = do
    env <- get
    case exp of
        ExpAdd e1 e2 -> compileOp e1 e2 "iadd\n"
        ExpSub e1 e2 -> compileOp e1 e2 "isub\n"
        ExpMul e1 e2 -> compileOp e1 e2 "imul\n"
        ExpDiv e1 e2 -> compileOp e1 e2	"idiv\n"
        ExpLit i -> return $ "bipush " ++ (show i) ++ "\n"
        ExpVar (Ident ident) -> do
            var_id <- gets $ Map.lookup ident
            case var_id of
                Nothing -> throwError $ "Read before use\n"
                Just vid -> return $ "iload " ++ (show vid) ++ "\n"

compileOp :: Exp -> Exp -> String -> CompilerMonad String
compileOp e1 e2 op = do
    str1 <- compileExp e1
    str2 <- compileExp e2
    return $ str1 ++ str2 ++ op

emptyEnv :: CompilerEnv
emptyEnv = Map.empty

