module ToAsm where

import Intermediate
import AbsLatte
import Control.Monad.Writer
import Control.Monad.State
import Data.Char (toLower)

type CompilerState = [Val]

type CompilerMonad = StateT CompilerState (Writer String)

compile :: [Blck] -> String
compile blcks = "section .text\nglobal main\nextern printInt\n\n" ++ 
    (execWriter $ evalStateT (prepCompMonad blcks) []) ++
    "\n\nsection .bss\n__TMPREGS__:\nresb 1024\n" --TODO dynamic resb

prepCompMonad :: [Blck] -> CompilerMonad ()
prepCompMonad [] = return ()
prepCompMonad (blck:rest) = case blck of
    NoNameBlck instrs -> compileInstrs (reverse instrs) >> prepCompMonad rest
    Blck name instrs -> tellLane (name ++ ":") >> compileInstrs (reverse instrs) >> prepCompMonad rest
    FnBlck name instrs -> tellLane (name ++ ":") >> compileInstrs (reverse instrs) >> prepCompMonad rest

compileInstrs :: Instrs -> CompilerMonad ()
compileInstrs [] = return ()
compileInstrs (instr:rest) = (case instr of
    Iret val -> do 
        movToEax val
        tellLane "leave"
        tellLane "ret"
    Vret -> do
        tellLane "leave"
        tellLane "ret"
    Icall l -> do 
        tellLane $ "call " ++ l
        put $ [Loc 0]
    ICjmp val l -> do
        movToEax val
        tellLane $ "test eax, eax"
        tellLane $ "jne " ++ l
    IJmp l -> tellLane $ "jmp " ++ l
    Iassign dst src -> do
        srcRep <- getValRep src
        dstRep <- getValRep dst
        if bothMem dst src
            then do
                movToEax src
                tellLane $ "mov " ++ dstRep ++ ", eax"
                put [src, dst]
            else do
                tellLane $ "mov DWORD " ++ dstRep ++ ", " ++ srcRep
    Iop op dst oper1 oper2 -> do
        dstRep <- getValRep dst
        opr2Rep <- getValRep oper2
        movToEax oper1
        tellLane $ case op of
            Add Plus -> "add eax, " ++ opr2Rep
            Add Minus -> "sub eax, " ++ opr2Rep
            Mul Times -> "imul eax, " ++ opr2Rep
            Mul Div -> "idiv " ++ opr2Rep
            Mul Mod -> "idiv " ++ opr2Rep ++ "\nmov eax, edx"
            OpAnd -> "and eax, " ++ opr2Rep
            OpOr -> "or eax, " ++ opr2Rep
            Rel rop -> "cmp eax, " ++ opr2Rep ++ "\nset" ++ (toLower <$> show rop) ++ " al\nand eax, 0xff"
        tellLane $ "mov " ++ dstRep ++ ", eax"
        put [dst]
    ISop op dst src -> do
        dstRep <- getValRep dst
        movToEax src
        tellLane $ case op of
            Ng -> "neg eax"
            Nt -> "xor eax, 1"
        tellLane $ "mov " ++ dstRep ++ ", eax"
        put [dst]
    Iparam val -> do
        valRep <- getValRep val
        tellLane $ "push DWORD " ++ valRep
    Inc val -> do
        valRep <- getValRep val
        tellLane $ "inc " ++ valRep
    Dec val -> do
        valRep <- getValRep val
        tellLane $ "dec " ++ valRep
    IgrowStack i -> if i == 0 then return () else tellLane $ "sub esp, " ++ (s4 i)
    IcutStack i -> if i == 0 then return () else tellLane $ "add esp, " ++ (s4 i)
    Inop -> tellLane "nop"
    Iprolog -> do
        tellLane "push ebp"
        tellLane "mov ebp, esp"
    ) >> compileInstrs rest



tellLane :: String -> CompilerMonad ()
tellLane s = tell $ s ++ "\n"

getValRep :: Val -> CompilerMonad String
getValRep val = do
    currEax <- get
    return $ if (elem val currEax) then "eax" else case val of
        VConst i -> show i
        VParam i -> "[ebp + " ++ (s4 i) ++ "]"
        VLocal i -> "[ebp - " ++ (s4 i) ++ "]"
        Loc i -> "[__TMPREGS__ + " ++ (s4 i) ++ "]"

bothMem :: Val -> Val -> Bool
bothMem (VConst _) _ = False
bothMem _ (VConst _) = False
bothMem _ _ = True

s4 :: Int -> String
s4 i = show $ i * 4

movToEax :: Val -> CompilerMonad ()
movToEax val = do
    valRep <- getValRep val
    unless (valRep == "eax") $ tellLane ("mov eax, " ++ valRep) >> put [val]
