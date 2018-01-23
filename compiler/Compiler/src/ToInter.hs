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


translate :: Program -> FuncsTypes -> ([Blck], [Literal])
translate (Program defs) fts = 
    let (blcks, lits) = runReader (evalStateT (runWriterT $ execWriterT $ prepTransMonad defs) (0, 0)) (Map.empty, 1, fts) in (blcks, lits)

prepTransMonad :: [TopDef] -> TranslatorMonad ()
prepTransMonad [] = return () 
prepTransMonad (def:rest) = transDef def >> modify (\(labs, _) -> (labs, 0)) >> prepTransMonad rest 

transDef :: TopDef -> TranslatorMonad ()
transDef (FnDef ftype (Ident ident) args (Block stmts)) = let argsLen = length args in
    local (\(env, locals, fts) -> (Map.union 
        (Map.fromList (zipWith (\(Arg argT (Ident ident)) nr -> 
            (ident, VParam nr argT)) args [argsLen + 1, argsLen..2])) env,
            locals, fts)) $ transStmts 
                (if null stmts then [VRet] else stmts) (FnBlck ident []) >>= tellBlck >> return ()

getFromEnv :: String -> TranslatorMonad Val
getFromEnv ident = do 
    (env, _, _) <- ask
    return $ fromJust $ Map.lookup ident env

newReg :: (Int -> TranslatorMonad a) -> TranslatorMonad a
newReg monadF = do
    (_, locals, _) <- ask
    modify (\(labs, maxRegs) -> (labs, max maxRegs (locals + 1)))
    local (\(env, locs, fts) -> (env, locs + 1, fts)) $ monadF locals

newLocal :: Ident -> Type -> TranslatorMonad a -> TranslatorMonad a
newLocal (Ident ident) locT monad = local (\(env, locals, fts) -> 
    (Map.insert ident (VLocal locals locT) env, locals + 1, fts)) monad

newLabel :: TranslatorMonad String
newLabel = do
    (labNr, maxRegs) <- get
    put $ (labNr + 1, maxRegs)
    return ('L':show labNr)

declAssigns :: Type -> [Item] -> (Instrs -> TranslatorMonad a) -> [Instrs] -> TranslatorMonad a
declAssigns _ [] monad prevs = monad $ concat prevs
declAssigns dtype (item:rest) monad prevs = case item of
    NoInit ident -> declAssigns dtype (Init ident (defaultValExpr dtype):rest) monad prevs
    Init ident expr -> do
        (_, locNr, _) <- ask
        assignInstrs <- assign (VLocal locNr dtype) expr
        newLocal ident dtype $ declAssigns dtype rest monad (assignInstrs:prevs)
    ArrIAlloc ident _ expr -> do
        (_, locNr, _) <- ask
        (val, instrs) <- transExp expr
        newLocal ident dtype $ declAssigns dtype rest monad ((Ipop (VLocal locNr dtype):(allocArr val instrs)):prevs)

defaultValExpr :: Type -> Expr
defaultValExpr t = case t of
    Int -> ELitInt 0
    Str -> EString ""
    Bool -> ELitFalse
    Arr _ -> ELitInt 0

assign :: Val -> Expr -> TranslatorMonad [Instr]
assign val expr = do
    (expVal, expInstrs) <- transExp expr
    return $ (Iassign val expVal):expInstrs
    

addInstrs :: Instrs -> Blck -> Blck
addInstrs newInstrs blck = case blck of
    Blck name instrs -> Blck name $ newInstrs ++ instrs
    FnBlck name instrs -> FnBlck name $ newInstrs ++ instrs

tellBlck :: Blck -> TranslatorMonad ()
tellBlck (FnBlck name instrs) = do
    (_, regsAndLocals) <- get
    tell [Blck name (instrs ++ [IgrowStack regsAndLocals, Iprolog])]
tellBlck blck = tell [blck]

transStmts :: [Stmt] -> Blck -> TranslatorMonad Blck
transStmts [] blck = return blck
transStmts (stmt:rest) blck = case stmt of
    ArrAlloc exp1 t exp2 -> do
        ((val1, exp1Instrs), Just (val2, exp2Instrs)) <- transTwoExps exp1 (Just exp2)
        transStmts rest $ addInstrs ((Ipop val1):exp1Instrs ++ (allocArr val2 exp2Instrs)) blck
    Empty -> transStmts rest $ addInstrs [Inop] blck
    BStmt (Block stmts) -> do
        afterBlck <- transStmts stmts blck
        transStmts rest afterBlck
    Decl dtype items -> declAssigns dtype items (\assignInstrs -> transStmts rest $ addInstrs assignInstrs blck) []
    Ass expr1 expr2 -> 
        case expr1 of
            EArrAcc _ _ -> do
                ((val1, _:exp1Ins), Just (val2, exp2Ins)) <- transTwoExps expr1 $ Just expr2
                transStmts rest $ addInstrs (IwritePtr val2:exp1Ins ++ exp2Ins) blck
            _ -> do 
                (val1, exp1Ins) <- transExp expr1
                assignInstrs <- assign val1 expr2
                transStmts rest $ addInstrs (assignInstrs ++ exp1Ins) blck
    Incr (Ident ident) -> getFromEnv ident >>= \val -> transStmts rest $ addInstrs [Inc val] blck
    Decr (Ident ident) -> getFromEnv ident >>= \val -> transStmts rest $ addInstrs [Dec val] blck
    Ret expr -> transExp expr >>= \(val, expInstrs) -> transStmts rest $ addInstrs ((Iret val):expInstrs) blck
    VRet -> transStmts rest $ addInstrs [Vret] blck
    Cond expr stmt -> do
        label <- newLabel
        (val, expInstrs) <- transExp $ Not expr
        oldBlck <- transStmts [stmt] $ addInstrs (ICjmp val label True:expInstrs) blck
        transStmts rest $ addInstrs [Ilabel label] $ oldBlck
    CondElse expr iStmt eStmt -> do
        ifLabel <- newLabel
        nextLabel <- newLabel
        (val, expInstrs) <- transExp $ expr
        oldBlck <- transStmts [eStmt] $ addInstrs (ICjmp val ifLabel True:expInstrs) blck
        if (null rest) then
            transStmts [iStmt] $ addInstrs [Ilabel ifLabel] oldBlck
        else do
            afterIfBlck <- transStmts [iStmt] $ addInstrs [Ilabel ifLabel, IJmp nextLabel] oldBlck
            transStmts rest $ addInstrs [Ilabel nextLabel] afterIfBlck
    While expr stmt -> do
        checkLabel <- newLabel
        loopLabel <- newLabel
        oldBlck <- transStmts [stmt] $ addInstrs [Ilabel loopLabel, IJmp checkLabel] blck
        (val, expInstrs) <- transExp $ expr
        transStmts rest $ addInstrs (ICjmp val loopLabel True:expInstrs ++ [Ilabel checkLabel]) oldBlck
    SExp expr -> transExp expr >>= \(_, expInstrs) -> transStmts rest $ addInstrs expInstrs blck
    SFor t ident coll stmt -> let tmpId = Ident "__tmp_iter__" in
        transStmts (Decl Int [Init tmpId (ELitInt 0)]:While (ERel (EVar tmpId) L (EMember (EVar coll) $ Ident "length")) (BStmt $ Block [Decl t [Init ident (EArrAcc (EVar coll) (EVar tmpId))],stmt,Incr tmpId]):rest) blck

allocArr :: Val -> Instrs -> Instrs
allocArr val instrs = (Ipush $ Reg "eax" Void):IcutStack 2:IwritePtr (Reg "[esp+4]" Int):(Icall 
    "malloc"):(Ipush $ Reg "eax" Void):(plus1Times4 val) ++ (Ipush val):instrs

transExp :: Expr -> TranslatorMonad (Val, [Instr])
transExp exp = do
    (res, _) <- transTwoExps exp Nothing
    return res

-- Translate two exps at once to allow exp2 to be translated in environment left by exp1 
transTwoExps :: Expr -> Maybe Expr -> TranslatorMonad ((Val, [Instr]), Maybe (Val, [Instr]))
transTwoExps exp1 exp2 = case exp1 of
    EArrAcc expr1 expr2 -> do
        ((val1, exp1Instrs), Just (val2, exp2Instrs)) <- transTwoExps expr1 (Just $ plus1Times4Exp expr2)
        let elemT = getArrType . getValType $ val1
        newReg (\regNr -> let result = (VLocal regNr elemT) in
            return (result, IreadPtr result:IcalcPtr val1 val2:exp1Instrs ++ exp2Instrs) >>= mergeExpResults exp2)
    EMember expr _ -> do
        (val, instrs) <- transExp expr
        let elemT = getArrType . getValType $ val
        newReg (\regNr -> let result = (VLocal regNr elemT) in
            return (result, IreadPtr result:Iassign (Reg "eax" Void) val:instrs) >>= mergeExpResults exp2) 
    EMul expr1 op expr2 -> binOp expr1 expr2 exp2 $ Mul op
    EAdd expr1 op expr2 -> binOp expr1 expr2 exp2 $ Add op
    ERel expr1 op expr2 -> binOp expr1 expr2 exp2 $ Rel op
    EAnd expr1 expr2 -> do
        lab <- newLabel
        binOp expr1 expr2 exp2 $ OpAnd lab
    EOr expr1 expr2 -> do 
        lab <- newLabel
        binOp expr1 expr2 exp2 $ OpOr lab
    Neg expr -> do
        (val, instrs) <- transExp expr
        newReg (\regNr -> let result = (VLocal regNr $ getValType val) in 
            return (result, (ISop Ng result val):instrs) >>= mergeExpResults exp2)
    Not expr -> do
        (val, instrs) <- transExp expr
        newReg (\regNr -> let result = (VLocal regNr $ getValType val) in 
            return (result, (ISop Nt result val):instrs) >>= mergeExpResults exp2)
    _ -> (case exp1 of
        EVar (Ident i) -> getFromEnv i >>= (\val -> return (val, []))
        ELitInt i -> return (VConst (fromInteger i) Int, [])
        ELitTrue -> return (VConst 1 Bool, [])
        ELitFalse -> return (VConst 0 Bool, [])
        EApp fid@(Ident ident) params -> do 
            instrs <- liftM concat (mapM (liftM (\(val, instrs) -> ((Ipush val):instrs)) . transExp) $ reverse params)
            (_, _, fTypes) <- ask
            let (Fun retT _) = fromJust $ Map.lookup fid fTypes
            return (Reg "eax" retT, (IcutStack (length params):Icall ident:instrs))
        EString lit -> do
            litLabel <- newLabel
            lift . tell $ [(litLabel, lit)]
            return (LitStr litLabel, [])
        ) >>= (mergeExpResults exp2)
 
mergeExpResults :: Maybe Expr -> (Val, [Instr]) -> TranslatorMonad ((Val, [Instr]), Maybe (Val, [Instr]))
mergeExpResults exp2 e1R = 
    if exp2 == Nothing then return (e1R, Nothing) else do
        (e2R, _) <- transTwoExps (fromJust exp2) Nothing
        return (e1R, Just e2R)


binOp :: Expr -> Expr -> Maybe Expr -> Op -> TranslatorMonad ((Val, [Instr]), Maybe (Val, [Instr]))
binOp expr1 expr2 nextExp op = do
    ((val1, instrs1), maybeE2R) <- transTwoExps expr1 $ Just expr2
    let (val2, instrs2) = fromJust maybeE2R in
        newReg (\regNr -> let a = getLazyInstrs op in
            let v1T = getValType val1 in 
                let resultT = getBinOpType (op, v1T, getValType val2) in
                    let result = VLocal regNr resultT in
                         let resultOp = if (op, resultT) == (Add Plus, Str) then OpStrAdd else op in
                            (if isEax val2 then --check if val2 is eax and push expr1 result for later use if so
                                return $ (result, (Iop resultOp result (Reg "eax" v1T) (Reg "edx" v1T)):
                                    (Ipop (Reg "eax" v1T)):
                                        (Iassign (Reg "edx" v1T) (Reg "eax" v1T)):(instrs2++(Ipush val1:(a++instrs1))))
                            else return (result, (Iop resultOp result val1 val2):(instrs2 ++ a ++ instrs1))
                            ) >>= (mergeExpResults nextExp))

changeValType :: Val -> Type -> Val
changeValType v t = case v of
    VConst i _ -> VConst i t
    VParam i _ -> VParam i t
    VLocal i _ -> VLocal i t
    Reg s _ -> Reg s t

getLazyInstrs :: Op -> [Instr]
getLazyInstrs op = case op of
    OpOr s -> [ICjmp (Reg "eax" Bool) s True]
    OpAnd s -> [ICjmp (Reg "eax" Bool) s False]
    _ -> []

isEax :: Val -> Bool
isEax (Reg "eax" _) = True
isEax _ = False

getArrType :: Type -> Type
getArrType (Arr t) = t

getValType :: Val -> Type
getValType val = case val of
    VConst _ t -> t
    VParam _ t -> t
    VLocal _ t -> t
    Reg _ t -> t
    LitStr _ -> Str

getBinOpType :: (Op, Type, Type) -> Type
getBinOpType b = case b of
    (Rel _, Int, Int) -> Bool
    (Add Plus, Str, Str) -> Str
    (_, t1, t2) -> if t1 == t2 then t1 else error $ "Typecheck is wrong" ++ (show t1) ++ (show t2) ++ (show b)

plus1Times4Exp :: Expr -> Expr
plus1Times4Exp e = EMul (EAdd e Plus (ELitInt 1)) Times $ ELitInt 4

plus1Times4 :: Val -> Instrs
plus1Times4 val = [Iop (Mul Times) (Reg "eax" Int) (Reg "eax" Int) (VConst 4 Int), Iop (Add Plus) (Reg "eax" Int) val (VConst 1 Int)]
