module Intermediate where

import AbsLatte

data Instr
  = Iret Val
  | Vret
  | Icall String 
  | ICjmp Val String Bool
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
  | Ipush Val
  | Ipop Val
  | Ilabel String
  | IcalcPtr Val Val
  | IreadPtr Val
  | IwritePtr Val
    deriving Show

data Blck = FnBlck String Instrs | Blck String Instrs
    deriving Show

data Op = Add AddOp | Mul MulOp | OpAnd String | OpOr String | Rel RelOp | OpStrAdd 
    deriving (Show, Eq)

data SOp = Ng | Nt
    deriving Show

data Val = VConst Int Type | VParam Int Type | VLocal Int Type | Reg String Type | LitStr String
    deriving (Show, Eq)

type Instrs = [Instr]

type Literal = (String, String)
