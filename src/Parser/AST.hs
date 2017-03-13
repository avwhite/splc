{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Parser.AST where

import Data.Data
import GHC.Generics hiding (Associativity)

type Identifier = String

data ASTType =
      BoolT
    | IntT
    | CharT
    | PairT ASTType ASTType
    | ListT ASTType
    | PolyT Identifier deriving (Generic, Data, Eq)

data ASTReturnType =
      Void
    | ReturnType ASTType deriving (Generic, Data, Eq)
data ASTFunType = FunType [ASTType] ASTReturnType deriving (Generic, Data, Eq)

data Op1 = Neg | Not deriving (Generic, Data, Eq)

data Op2 =
      Plus | Minus | Times | Div | Mod
    | Equal | Less | Greater | LessEq | GreaterEq | NotEq
    | And | Or
    | Cons deriving (Generic, Data, Eq)

type Precedence = Int
data Associativity = LeftAssoc | RightAssoc

highestPrecedence = 4 :: Precedence

precedence :: Op2 -> Precedence
precedence Equal = 0
precedence NotEq = 0
precedence Less = 1
precedence Greater = 1
precedence LessEq = 1
precedence GreaterEq = 1
precedence Cons = 2
precedence Plus = 3
precedence Minus = 3
precedence Or = 3
precedence Times = 4
precedence Div = 4
precedence Mod = 4
precedence And = 4

assoc :: Precedence -> Associativity
assoc 2 = RightAssoc
assoc _ = LeftAssoc

data Field = Hd | Tl | Fst | Snd deriving (Generic, Data, Eq)

data ASTExp =
      Var Identifier [Field]
    | Op2E Op2 ASTExp ASTExp
    | Op1E Op1 ASTExp
    | FunCallE Identifier [ASTExp]
    | IntE Int
    | CharE Char
    | BoolE Bool
    | NilE
    | PairE ASTExp ASTExp deriving (Generic, Data, Eq)

data ASTStmt =
      IfS ASTExp [ASTStmt] (Maybe [ASTStmt])
    | WhileS ASTExp [ASTStmt]
    | AssignS Identifier [Field] ASTExp
    | FunCallS Identifier [ASTExp]
    | ReturnS (Maybe ASTExp) deriving (Generic, Data, Eq)

data ASTVarDecl = VarDecl (Maybe ASTType) Identifier ASTExp
    deriving (Generic, Data, Eq)
data ASTFunDecl =
    FunDecl Identifier [Identifier] (Maybe ASTFunType) [ASTVarDecl] [ASTStmt]
    deriving (Generic, Data, Eq)

data ASTDecl = FunD ASTFunDecl | VarD ASTVarDecl deriving (Generic, Data, Eq)

data AST = AST [ASTDecl] deriving (Generic, Data, Eq)

data Token =
     AssignTok
    |  VarTok
    | SemiColonTok
    | LParTok
    | RParTok
    | OfTypeTok
    | LBracketTok
    | RBracketTok
    | VoidTok
    | ArrowTok
    | CommaTok
    | LSqBracketTok
    | RSqBracketTok
    | IntTok
    | BoolTok
    | CharTok
    | IfTok
    | ElseTok
    | WhileTok
    | ReturnTok
    | FalseTok
    | TrueTok
    | EmptyListTok
    | HdTok
    | TlTok
    | FstTok
    | SndTok
    | DotTok
    | PlusTok
    | MinusTok
    | TimesTok
    | DivTok
    | ModTok
    | EqTok
    | LtTok
    | GtTok
    | LeTok
    | GeTok
    | NeTok
    | AndTok
    | OrTok
    | ColonTok
    | NotTok
    | IntLitTok Int
    | IdTok String
    | EofTok
    deriving (Data, Eq, Show, Ord)
