module ToAsm where

import Intermediate
import AbsLatte
import Control.Monad.Writer
import Control.Monad.State
import Data.Char (toLower)

type CompilerState = [Val]

type CompilerMonad = StateT CompilerState (Writer String)

compile :: ([Blck], [Literal], Int) -> String
compile (blcks, literals, regs) = "section .text\n" ++ externs ++ 
    (execWriter $ evalStateT (prepCompMonad blcks) []) ++  
    "\n\nsection .bss\n__TMPREGS__:\nresb " ++ (show $ regs * 4) ++ 
    "\n\nsection .data\n" ++ (compileLiterals literals)

prepCompMonad :: [Blck] -> CompilerMonad ()
prepCompMonad [] = return ()
prepCompMonad (blck:rest) = case blck of
    NoNameBlck instrs -> compileInstrs (reverse instrs) >> put [] >> prepCompMonad rest
    Blck name instrs -> tellLane (name ++ ":") >> put [] >> compileInstrs (reverse instrs) >> prepCompMonad rest

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
        put $ [Reg "eax" Void] -- void for placeholder
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
        opr1Rep <- getValRep oper1
        unless (op == OpStrAdd || opr1Rep == "edx") $ movToEax oper1
        opr2Rep <- getValRep oper2
        tellLane $ case op of
            Add Plus -> "add eax, " ++ opr2Rep
            Add Minus -> "sub eax, " ++ opr2Rep
            Mul Times -> "imul eax, " ++ opr2Rep
            Mul Div -> "xor edx, edx\nmov ecx, " ++ opr2Rep ++ "\nidiv ecx"
            Mul Mod -> "xor edx, edx\nmov ecx, " ++ opr2Rep ++ "\nidiv ecx\nmov eax, edx"
            OpAnd -> "and eax, " ++ opr2Rep
            OpOr -> "or eax, " ++ opr2Rep
            Rel rop -> "cmp eax, " ++ opr2Rep ++ "\nset" ++ (toLower <$> show rop) ++ " al\nand eax, 0xff"
            OpStrAdd -> "push DWORD " ++ opr2Rep ++ "\npush DWORD " ++ opr1Rep ++ "\ncall __CONCAT_STRINGS__\nsub esp, 8"
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
        tellLane $ "inc DWORD " ++ valRep
    Dec val -> do
        valRep <- getValRep val
        tellLane $ "dec DWORD " ++ valRep
    IgrowStack i -> if i == 0 then return () else tellLane $ "sub esp, " ++ (s4 i)
    IcutStack i -> if i == 0 then return () else tellLane $ "add esp, " ++ (s4 i)
    Inop -> tellLane "nop"
    Iprolog -> do
        tellLane "push ebp"
        tellLane "mov ebp, esp"
    Iswap -> tellLane "xchg eax, edx"
    ) >> compileInstrs rest



tellLane :: String -> CompilerMonad ()
tellLane s = tell $ s ++ "\n"

getValRep :: Val -> CompilerMonad String
getValRep val = do
    currEax <- get
    return $ if (elem val currEax) then "eax" else case val of
        VConst i -> show i
        VParam i _ -> "[ebp + " ++ (s4 i) ++ "]"
        VLocal i _ -> "[ebp - " ++ (s4 i) ++ "]"
        Loc i _ -> "[__TMPREGS__ + " ++ (s4 i) ++ "]"
        Reg r _ -> r
        LitStr label -> label

bothMem :: Val -> Val -> Bool
bothMem (VConst _) _ = False
bothMem _ (VConst _) = False
bothMem (LitStr _) _ = False
bothMem _ (LitStr _) = False
bothMem _ _ = True

s4 :: Int -> String
s4 i = show $ i * 4

movToEax :: Val -> CompilerMonad ()
movToEax val = do
    valRep <- getValRep val
    unless (valRep == "eax") $ tellLane ("mov eax, " ++ valRep)

compileLiterals :: [Literal] -> String
compileLiterals = concat . map (\(lab, val) -> lab ++ "  db '" ++ val ++ "',0\n")

externs = "global main\nextern printInt\nextern printString\nextern readInt\nextern readString\nextern error\nextern __CONCAT_STRINGS__\n\n" 
