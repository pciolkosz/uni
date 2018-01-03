module Intermediate where

import AbsLatte

data Instr
  = Iret Val
  | Vret
  | Icall String 
  | ICjmp Val String
  | IJmp String
  | Iassign Val Val
  | Iop Op Val Val Val
  | ISop SOp Val Val
  | Iparam Val
  | Inc Val
  | Dec Val
  | IgrowStack Int
  | IcutStack Int
  | Inop
  | Iprolog
    deriving Show

data Blck = Blck String Instrs | NoNameBlck Instrs
    deriving Show

data Op = Add AddOp | Mul MulOp | OpAnd | OpOr | Rel RelOp 
    deriving Show

data SOp = Ng | Nt
    deriving Show

data Val = VConst Int | VParam Int | VLocal Int | Loc Int | Name String
    deriving Show

type Instrs = [Instr]
