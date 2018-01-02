module ToInter where

import Intermediate
import AbsLatte
import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer.Lazy
import Control.Monad.Reader
import Data.Maybe

type TransState = (Int, Int)

type TransEnv = (Map.Map String Val, Int)

type TranslatorMonad = WriterT [Blck] (StateT TransState (Reader TransEnv))


translate :: Program -> [Blck]
translate (Program defs) = runReader (evalStateT (execWriterT $ prepTransMonad defs) (1, 0)) (Map.empty, 0)

prepTransMonad :: [TopDef] -> TranslatorMonad ()
prepTransMonad [] = return () 
prepTransMonad (def:rest) = transDef def >> prepTransMonad rest 

transDef :: TopDef -> TranslatorMonad ()
transDef (FnDef ftype (Ident ident) args (Block stmts)) =
    local (\(env, locals) -> (Map.union 
        (Map.fromList (zipWith (\(Arg _ (Ident ident)) nr -> (ident, VParam nr)) args [0..(length args - 1)]))
        env, locals)) $ transStmts stmts (Blck ident []) >> return () --TODO add prolog

getFromEnv :: String -> TranslatorMonad Val
getFromEnv ident = asks $ (fromJust . Map.lookup ident) . fst

newReg :: TranslatorMonad Int
newReg = do
    (regNr, labNr) <- get
    put (regNr + 1, labNr)
    return regNr

newLocal :: Ident -> TranslatorMonad a -> TranslatorMonad a
newLocal (Ident ident) monad = local (\(env, locals) -> (Map.insert ident (VLocal locals) env, locals + 1)) monad

newLabel :: TranslatorMonad String
newLabel = do
    (regNr, labNr) <- get
    put (regNr, labNr + 1)
    return ('L':show labNr)

--TODO throw away
--assignVal :: String -> Expr -> (Instrs -> TranslatorMonad a) -> TranslatorMonad a
--assignVal ident exp monad = transExp exp >>= (\(val, expInstrs) -> local (Map.insert ident val) (monad $ expInstrs ++ Iassign val))

declAssigns :: [Item] -> (Instrs -> TranslatorMonad ()) -> [Instrs] -> TranslatorMonad ()
declAssigns [] monad prevs = monad $ concat prevs
declAssigns (item:rest) monad prevs = case item of
    NoInit ident -> newLocal ident $ declAssigns rest monad prevs
    Init ident expr -> do
        (_, locNr) <- ask
        assignInstrs <- assign (VLocal locNr) expr
        newLocal ident $ declAssigns rest monad (assignInstrs:prevs) 


assign :: Val -> Expr -> TranslatorMonad [Instr]
assign val expr = do
    (expVal, expInstrs) <- transExp expr
    return $ expInstrs ++ [(Iassign val expVal)]
    

addInstrs :: Instrs -> Blck -> Blck
addInstrs newInstrs blck = case blck of
    Blck name instrs -> Blck name $ newInstrs ++ instrs
    NoNameBlck instrs -> NoNameBlck $ newInstrs ++ instrs

transStmts :: [Stmt] -> Blck -> TranslatorMonad () 
transStmts [] blck = tell [blck]
transStmts (stmt:rest) blck = case stmt of
    Empty -> transStmts rest $ addInstrs [Inop] blck
    BStmt (Block stmts) -> transStmts stmts (NoNameBlck []) >> (transStmts rest) blck
    Decl dtype items -> declAssigns items (\assignInstrs -> transStmts rest $ addInstrs assignInstrs blck) []
    Ass expr1 expr2 -> do
        (val, exp1Ins) <- transExp expr1
        assignInstrs <- assign val expr2
        transStmts rest $ addInstrs (assignInstrs ++ exp1Ins) blck
    Incr (Ident ident) -> getFromEnv ident >>= \val -> transStmts rest $ addInstrs [Inc val] blck
    Decr (Ident ident) -> getFromEnv ident >>= \val -> transStmts rest $ addInstrs [Inc val] blck
    Ret expr -> transExp expr >>= \(val, expInstrs) -> transStmts rest $ addInstrs ((Iret val):expInstrs) blck
    VRet -> transStmts rest $ addInstrs [Vret] blck -- TODO epilog plus better ret
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
        unless (null rest) $ tell [NoNameBlck [(IJmp nextLabel)]]
        transStmts [iStmt] $ Blck ifLabel []
        unless (null rest) $ transStmts rest $ Blck nextLabel []
    While expr stmt -> do
        checkLabel <- newLabel
        loopLabel <- newLabel
        tell [addInstrs [IJmp checkLabel] blck]
        transStmts [stmt] $ Blck loopLabel []
        (val, expInstrs) <- transExp $ expr
        transStmts rest $ Blck checkLabel (ICjmp val loopLabel:expInstrs)
    SExp expr -> transExp expr >>= \(_, expInstrs) -> transStmts rest $ addInstrs expInstrs blck
    
transExp :: Expr -> TranslatorMonad (Val, [Instr])
transExp expr = case expr of
    EVar (Ident i) -> getFromEnv i >>= (\val -> return (val, []))
    ELitInt i -> return (VConst $ fromInteger i, [])
    ELitTrue -> return (VConst 1, [])
    ELitFalse -> return (VConst 0, [])
    EApp (Ident ident) params -> 
        liftM concat (mapM (liftM (\(val, instrs) -> ((Iparam val):instrs)) . transExp) $ reverse params) >>= 
            (\instrs -> return (Loc 0, (Icall ident:instrs)))
--    EString String -> --TODO
    Neg expr -> do
        (val, instrs) <- transExp expr
        regNr <- newReg
        return (Loc regNr, (ISop Ng (Loc regNr) val):instrs)
    Not expr -> do
        (val, instrs) <- transExp expr
        regNr <- newReg
        return (Loc regNr, (ISop Nt (Loc regNr) val):instrs)
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
    return (Loc regNr, (Iop op (Loc regNr) val1 val2):(instrs2 ++ instrs1))

