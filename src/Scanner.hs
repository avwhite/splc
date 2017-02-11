module Scanner (
    Token(..)
    ) where

data Token =
      IdTok String
    | IntLitTok Integer
    | Op2 BinOp
    | ConsTok 
    | Op1 UnOp
    | LParTok | RParTok
    | LSqBracketTok | RSqBracketTok | LBracketTok | RBracketTok
    | TrueTok | FalseTok | EmptyListTok
    | AssignTok | SemiColonTok | ArrowTok | CommaTok
    | IntTok | BoolTok | CharTok deriving Eq

data BinOp = Minus | Plus | Times | Div | Mod | Eq | Lt | Gt | Le | Ge | Ne
    | And | Or deriving Eq

data UnOp = Not | Neg deriving Eq
