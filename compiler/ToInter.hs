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

type TransEnv = Map.Map String Val

type TranslatorMonad = WriterT [Blck] (StateT TransState (Reader TransEnv))


translate :: Program -> [Blck]
translate (Program defs) = runReader (evalStateT (execWriterT $ prepTransMonad defs) (0, 0)) Map.empty

prepTransMonad :: [TopDef] -> TranslatorMonad ()
prepTransMonad [] = return () 
prepTransMonad (def:rest) = transDef def >> prepTransMonad rest -- TODO move Program unpack to run monad

transDef :: TopDef -> TranslatorMonad ()
transDef (FnDef ftype (Ident ident) args (Block stmts)) =
    transStmts stmts (Blck ident []) >> return () --TODO add prolog

getFromEnv :: String -> TranslatorMonad Val
getFromEnv ident = asks (\map -> fromJust $ Map.lookup ident map)

newReg :: TranslatorMonad Int
newReg = do
    (regNr, sth) <- get
    put (regNr + 1, sth) --TODO sth
    return regNr

assignVal :: String -> Expr -> (Instrs -> TranslatorMonad a) -> TranslatorMonad a
assignVal ident exp monad = transExp exp >>= (\(val, expInstrs) -> local (Map.insert ident val) (monad expInstrs))

declAssigns :: [Item] -> TranslatorMonad [Instrs]
declAssigns [] = return []
declAssigns (item:rest) = case item of
    NoInit _ -> declAssigns rest
    Init (Ident ident) expr -> assignVal ident expr $ (\instrs -> do
        restInstrs <- declAssigns rest
        return $ [instrs] ++ restInstrs)

transStmts :: [Stmt] -> Blck -> TranslatorMonad () 
transStmts [] blck = tell [blck]
transStmts (stmt:rest) (Blck name instrs) = case stmt of
    Empty -> transStmts rest (Blck name (Inop:instrs))
    BStmt (Block stmts) -> (local id transStmts stmts (Blck "tmp" [])) >> (transStmts rest) (Blck name instrs) -- TODO fix tmp
    Decl dtype items -> declAssigns items >>= 
        (\assignInstrs -> transStmts rest (Blck name $ (concat . reverse $ assignInstrs) ++ instrs))
    Ass (Ident ident) expr -> assignVal ident expr $ (\expInstrs -> transStmts rest (Blck name (expInstrs ++ instrs)))
    Incr (Ident ident) -> getFromEnv ident >>= \val -> transStmts rest (Blck name $ (Inc val):instrs)
    Decr (Ident ident) -> getFromEnv ident >>= \val -> transStmts rest (Blck name $ (Dec val):instrs)
    Ret expr -> transExp expr >>= (\(val, expInstrs) -> transStmts rest (Blck name $ (Iret val):(expInstrs ++ instrs)))
    VRet -> transStmts rest (Blck name $ (Iret $ Loc 0):instrs) -- TODO epilog plus better ret
    SExp expr -> transExp expr >>= (\(_, expInstrs) -> transStmts rest (Blck name $ expInstrs ++ instrs))
    
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
        return (VLocal regNr, (ISop Ng (VLocal regNr) val):instrs)
    Not expr -> do
        (val, instrs) <- transExp expr
        regNr <- newReg
        return (VLocal regNr, (ISop Nt (VLocal regNr) val):instrs)
    EMul expr1 op expr2 -> do
        (val1, instrs1) <- transExp expr1
        (val2, instrs2) <- transExp expr2 --TODO order to save regs
        regNr <- newReg
        return (VLocal regNr, (Iop (case op of
            Times -> OpMul
            Div -> OpDiv
            Mod -> OpMod) (VLocal regNr) val1 val2):(instrs2 ++ instrs1))
    EAdd expr1 op expr2 -> do
        (val1, instrs1) <- transExp expr1
        (val2, instrs2) <- transExp expr2 --TODO order to save regs
        regNr <- newReg
        return (VLocal regNr, (Iop (case op of
            Plus -> OpAdd
            Minus -> OpSub) (VLocal regNr) val1 val2):(instrs2 ++ instrs1))
    
        







