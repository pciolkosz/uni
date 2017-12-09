

module AbsLatte where

-- Haskell module generated by the BNF converter




newtype Ident = Ident String deriving (Eq, Ord, Show, Read)
data Program = Program [TopDef]
  deriving (Eq, Ord, Show, Read)

data TopDef
    = FnDef Type Ident [Arg] Block
    | ClDef Ident [MemberDef]
    | DepClDef Ident Ident [MemberDef]
  deriving (Eq, Ord, Show, Read)

data Arg = Arg Type Ident
  deriving (Eq, Ord, Show, Read)

data MemberDef = MAttr Type [Item] | MMethod Type Ident [Arg] Block
  deriving (Eq, Ord, Show, Read)

data Stmt
    = Alloc Expr Type
    | ArrAlloc Expr Type Expr
    | Empty
    | BStmt Block
    | Decl Type [Item]
    | Ass Expr Expr
    | Incr Ident
    | Decr Ident
    | Ret Expr
    | VRet
    | Cond Expr Stmt
    | CondElse Expr Stmt Stmt
    | While Expr Stmt
    | SExp Expr
    | SFor Type Ident Ident Stmt
  deriving (Eq, Ord, Show, Read)

data Block = Block [Stmt]
  deriving (Eq, Ord, Show, Read)

data Item
    = NoInit Ident
    | Init Ident Expr
    | InitAlloc Ident Type
    | ArrIAlloc Ident Type Expr
  deriving (Eq, Ord, Show, Read)

data Type
    = Arr Type
    | Class Ident
    | Int
    | Str
    | Bool
    | Void
    | Fun Type [Type]
  deriving (Eq, Ord, Show, Read)

data Expr
    = EArrAcc Expr Expr
    | ENull Type
    | EMember Expr Ident
    | EMethod Expr Ident [Expr]
    | EVar Ident
    | ELitInt Integer
    | ELitTrue
    | ELitFalse
    | EApp Ident [Expr]
    | EString String
    | Neg Expr
    | Not Expr
    | EMul Expr MulOp Expr
    | EAdd Expr AddOp Expr
    | ERel Expr RelOp Expr
    | EAnd Expr Expr
    | EOr Expr Expr
  deriving (Eq, Ord, Show, Read)

data AddOp = Plus | Minus
  deriving (Eq, Ord, Show, Read)

data MulOp = Times | Div | Mod
  deriving (Eq, Ord, Show, Read)

data RelOp = LTH | LE | GTH | GE | EQU | NE
  deriving (Eq, Ord, Show, Read)
