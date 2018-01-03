module ToAsm where

import Intermediate
import Control.Monad.Writer
import Data.Char (toLower)

type CompilerMonad = Writer String 

compile :: [Blck] -> String
compile blcks = execWriter $ prepCompMonad blcks

prepCompMonad :: [Blck] -> CompilerMonad ()
prepCompMonad [] = return ()
prepCompMonad (blck:rest) = case blck of
    NonameBlck instrs -> compileInstrs $ reverse instrs >> prepCompMonad rest
    Blck name instrs -> tellLane name >> compileInstrs $ reverse instrs >> prepCompMonad rest

compileInstrs :: Instrs -> CompilerMonad ()
compileInstrs [] = return ()
compileInstrs (instr:rest) = case instr of
    Iret val -> do 
        tellLane $ "mov eax, " ++ (getValRep val)
        tellLane "leave"
        tellLane "ret"
    Vret -> do
        tellLane "leave"
        tellLane "ret"
    Icall l -> tellLane $ "call " ++ l
    ICjmp val l -> let valRep = getValRep val in do
        tellLane "test " ++ valRep ++ ", " ++ valRep
        tellLane "jne " ++ l
    IJmp l -> tellLane $ "jmp " ++ l
    Iassign dst src -> if bothMem dst src 
        then do
            tellLane $ "mov eax, " ++ (getValRep src)
            tellLane $ "mov " ++ (getValRep dst) ++ ", eax"
        else do
            tellLane $ "mov " ++ (getValRep dst) ++ ", " ++ (getValRep src)
    Iop op dst oper1 oper2 -> do
        tellLane $ "mov eax, " ++ (getValRep oper1)
        tellLane $ let opr2Rep = getValRep oper2 in case op of
            Add Plus -> "add eax, " ++ opr2Rep
            Add Minus -> "sub eax, " ++ opr2Rep
            Mul Times -> "imul eax, " ++ opr2Rep
            Mul Div -> "idiv " ++ opr2Rep
            Mul Mod -> "idiv " ++ opr2Rep ++ "\nmov eax, edx"
            OpAnd -> "and eax, " ++ opr2Rep
            OpOr -> "or eax, " ++ opr2Rep
            Rel rop -> "cmp eax, " ++ opr2Rep ++ "set" ++ (toLower . show $ rop) ++ " al\nand eax, 0xff"
        tellLane $ "mov " ++ (getValRep dst) ++ ", eax"
    ISop op dst src -> do
        tellLane $ "mov eax, " ++ (getValRep src)
        tellLane $ case op of
            Ng -> "neg eax"
            Nt -> "xor eax, 1"
        tellLane $ "mov " ++ (getValRep dst) ++ ", eax"
    Iparam val -> tellLane $ "push " ++ (getValRep val)
    Inc val -> tellLane $ "inc " ++ (getValRep val)
    Dec val -> tellLane $ "dec " ++ (getValRep val)
    IgrowStack i -> tellLane $ "sub esp, " ++ (s4 i)
    IcutStack i -> tellLane $ "add esp, " ++ (s4 i)
    Inop -> tellLane "nop"
    Iprolog -> do
        tellLane "push ebp"
        tellLane "mov ebs, esp"



tellLane :: String -> CompilerMonad ()
tellLane s = tell $ s ++ "\n"

getValRep :: Val -> String
getValRep val = case val of
    VConst i -> show i
    VParam i -> "[ebp + " ++ (s4 i) ++ "]"
    VLocal i -> "[ebp - " ++ (s4 i) ++ "]"
    Loc i -> "[__TMPREGS__ + " ++ (s4 i) ++ "]"

bothMem :: Val -> Val -> Bool
bothMem (VConst _) _ = false
bothMem _ (VConst _) = false
bothMem _ _ = true

s4 :: Int -> String
s4 i = show $ i * 4
