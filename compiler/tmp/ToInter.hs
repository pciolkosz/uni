module ToInter where

import Intermediate
import AbsLatte
import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer.Lazy
import Control.Monad.Reader
import Data.Maybe

type FuncsTypes = Map.Map Ident Type

type TransState = (Int, Int)

type TransEnv = (Map.Map String Val, Int, FuncsTypes)

type TranslatorMonad = WriterT [Blck] (WriterT [Literal] (StateT TransState (Reader TransEnv)))


translate :: Program -> FuncsTypes -> ([Blck], [Literal], Int)
translate (Program defs) fts = let ((blcks, lits), (regs, _)) = runReader (runStateT (runWriterT $ execWriterT $ prepTransMonad defs) (1, 0)) (Map.empty, 1, fts) in (blcks, lits, regs)

prepTransMonad :: [TopDef] -> TranslatorMonad ()
prepTransMonad [] = return () 
prepTransMonad (def:rest) = transDef def >> prepTransMonad rest 

transDef :: TopDef -> TranslatorMonad ()
transDef (FnDef ftype (Ident ident) args (Block stmts)) = let argsLen = length args in
    local (\(env, locals, fts) -> (Map.union 
        (Map.fromList (zipWith (\(Arg argT (Ident ident)) nr -> 
            (ident, VParam nr argT)) args [argsLen + 1, argsLen..2])) env,
            locals, fts)) $ transStmts (if null stmts then [VRet] else stmts) (FnBlck ident []) >> return ()

getFromEnv :: String -> TranslatorMonad Val
getFromEnv ident = do 
    (env, _, _) <- ask
    return $ fromJust $ Map.lookup ident env

newReg :: TranslatorMonad Int
newReg = do
    (regNr, labNr) <- get
    put (regNr + 1, labNr)
    return regNr

newLocal :: Ident -> Type -> TranslatorMonad a -> TranslatorMonad a
newLocal (Ident ident) locT monad = local (\(env, locals, fts) -> 
    (Map.insert ident (VLocal locals locT) env, locals + 1, fts)) monad

newLabel :: TranslatorMonad String
newLabel = do
    (regNr, labNr) <- get
    put (regNr, labNr + 1)
    return ('L':show labNr)

declAssigns :: Type -> [Item] -> (Instrs -> TranslatorMonad ()) -> [Instrs] -> TranslatorMonad ()
declAssigns _ [] monad prevs = monad $ concat prevs
declAssigns dtype (item:rest) monad prevs = case item of
    NoInit ident -> newLocal ident dtype $ declAssigns dtype rest monad prevs --TODO default init
    Init ident expr -> do
        (_, locNr, _) <- ask
        assignInstrs <- assign (VLocal locNr dtype) expr
        newLocal ident dtype $ declAssigns dtype rest monad (assignInstrs:prevs) 


assign :: Val -> Expr -> TranslatorMonad [Instr]
assign val expr = do
    (expVal, expInstrs) <- transExp expr
    return $ (Iassign val expVal):expInstrs
    

addInstrs :: Instrs -> Blck -> Blck
addInstrs newInstrs blck = case blck of
    Blck name instrs -> Blck name $ newInstrs ++ instrs
    FnBlck name instrs -> FnBlck name $ newInstrs ++ instrs
    NoNameBlck instrs -> NoNameBlck $ newInstrs ++ instrs

tellBlck :: Blck -> TranslatorMonad ()
tellBlck (FnBlck name instrs) = do
    (_, locals, _) <- ask
    tell [Blck name (instrs ++ [IgrowStack locals, Iprolog])]
tellBlck blck = tell [blck]

transStmts :: [Stmt] -> Blck -> TranslatorMonad () 
transStmts [] blck = tellBlck blck
transStmts (stmt:rest) blck = case stmt of
    Empty -> transStmts rest $ addInstrs [Inop] blck
    BStmt (Block stmts) -> tellBlck blck >> transStmts stmts (NoNameBlck []) >> (transStmts rest) (NoNameBlck [])
    Decl dtype items -> declAssigns dtype items (\assignInstrs -> transStmts rest $ addInstrs assignInstrs blck) []
    Ass expr1 expr2 -> do
        (val, exp1Ins) <- transExp expr1
        assignInstrs <- assign val expr2
        transStmts rest $ addInstrs (assignInstrs ++ exp1Ins) blck
    Ret expr -> transExp expr >>= \(val, expInstrs) -> transStmts rest $ addInstrs ((Iret val):expInstrs) blck
    VRet -> transStmts rest $ addInstrs [Vret] blck
    Cond expr stmt -> do
        label <- newLabel
        (val, expInstrs) <- transExp $ Not expr
        transStmts [stmt] $ addInstrs (ICjmp val label:expInstrs) blck
        transStmts rest $ Blck label []
    CondElse expr iStmt eStmt -> do
        ifLabel <- newLabel
        nextLabel <- newLabel
        (val, expInstrs) <- transExp $ expr
        transStmts [eStmt] $ addInstrs (ICjmp val ifLabel:expInstrs) blck
        unless (null rest) $ tellBlck $ NoNameBlck [(IJmp nextLabel)]
        transStmts [iStmt] $ Blck ifLabel []
        unless (null rest) $ transStmts rest $ Blck nextLabel []
    While expr stmt -> do
        checkLabel <- newLabel
        loopLabel <- newLabel
        tellBlck $ addInstrs [IJmp checkLabel] blck
        transStmts [stmt] $ Blck loopLabel []
        (val, expInstrs) <- transExp $ expr
        transStmts rest $ Blck checkLabel (ICjmp val loopLabel:expInstrs)
    SExp expr -> transExp expr >>= \(_, expInstrs) -> transStmts rest $ addInstrs expInstrs blck
    
transExp :: Expr -> TranslatorMonad (Val, [Instr])
transExp expr = case expr of
    EVar (Ident i) -> getFromEnv i >>= (\val -> return (val, []))
    ELitInt i -> return (VConst (fromInteger i), [])
    ELitTrue -> return (VConst 1, [])
    ELitFalse -> return (VConst 0, [])
    EApp fid@(Ident ident) params -> do 
        instrs <- liftM concat (mapM (liftM (\(val, instrs) -> ((Iparam val):instrs)) . transExp) $ reverse params)
        (_, _, fTypes) <- ask
        let retT = fromJust $ Map.lookup fid fTypes
        return (Reg "eax" retT, (IcutStack (length params):Icall ident:instrs)) --TODO placeholder void
    EString lit -> do
        litLabel <- newLabel
        lift . tell $ [(litLabel, lit)]
        return (LitStr litLabel, [])
    Neg expr -> do
        (val, instrs) <- transExp expr
        regNr <- newReg
        let result = (Loc regNr $ getValType val) in 
            return (result, (ISop Ng result val):instrs)
    Not expr -> do
        (val, instrs) <- transExp expr
        regNr <- newReg
        let result = (Loc regNr $ getValType val) in 
            return (result, (ISop Nt result val):instrs)
    EMul expr1 op expr2 -> binOp expr1 expr2 $ Mul op
    EAdd expr1 op expr2 -> binOp expr1 expr2 $ Add op
    ERel expr1 op expr2 -> binOp expr1 expr2 $ Rel op
    EAnd expr1 expr2 -> binOp expr1 expr2 OpAnd
    EOr expr1 expr2 -> binOp expr1 expr2 OpOr

binOp :: Expr -> Expr -> Op -> TranslatorMonad (Val, [Instr])
binOp expr1 expr2 op = do
    (val1, instrs1) <- transExp expr1
    (val2, instrs2) <- transExp expr2 --TODO order to save regs
    regNr <- newReg
    let v1T = getValType val1 in 
        let resultT = getBinOpType (op, v1T, getValType val2) in
            let result = Loc regNr resultT in
                 let resultOp = if (op, resultT) == (Add Plus, Str) then OpStrAdd else op in
                    if bothEax val1 val2 then
                        return (result, (Iop resultOp result (Reg "eax" v1T) (Reg "edx" v1T)):
                            (Iswap:instrs2 ++ Iassign (Reg "edx" v1T) (Reg "eax" v1T):instrs1))
                    else return (result, (Iop resultOp result val1 val2):(instrs2 ++ instrs1))


bothEax :: Val -> Val -> Bool
bothEax (Reg "eax" _) (Reg "eax" _) = True
bothEax _ _ = False

getValType :: Val -> Type
getValType val = case val of
    VConst _ -> Int
    VParam _ t -> t
    VLocal _ t -> t
    Loc _ t -> t
    Reg _ t -> t
    LitStr _ -> Str

getBinOpType :: (Op, Type, Type) -> Type
getBinOpType b = case b of
    (Rel _, Int, Int) -> Bool
    (Add Plus, Str, Str) -> Str
    (_, t1, t2) -> if t1 == t2 then t1 else error $ "Typecheck is wrong" ++ (show t1) ++ (show t2)
