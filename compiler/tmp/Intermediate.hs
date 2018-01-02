module Intermediate where

data Instr
  = Iret Val
  | Icall String 
  | ICjmp ROp Val Val Blck
  | Iassign Val Val
  | Iop Op Val Val Val
  | ISop SOp Val Val
  | Iparam Val
  | Inc Val
  | Dec Val
  | Inop
    deriving Show

data Blck = Blck Name Instrs
    deriving Show

data Op = OpAdd | OpSub | OpMul | OpDiv | OpMod | OpAnd | OpOr
    deriving Show

data SOp = Ng | Nt
    deriving Show

data ROp = RLTH | RLE | RGTH | RGE | REQU | RNE
    deriving Show

data Val = VConst Int | VParam Int | VLocal Int | Loc Int | Name String
    deriving Show

type Instrs = [Instr]
