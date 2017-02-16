{
{-# LANGUAGE DeriveDataTypeable #-}
module Scanner where

import Data.Data
}
%wrapper "basic"

tokens :-
    $white+ ;
    var {\s -> VarTok}
    \= {\s -> AssignTok}
    \; {\s -> SemiColonTok}
    \( {\s -> LParTok}
    \) {\s -> RParTok}
    :: {\s -> OfTypeTok}
    \{ {\s -> LBracketTok}
    \} {\s -> RBracketTok}
    Void {\s -> VoidTok}
    \-\> {\s -> ArrowTok}
    \, {\s -> CommaTok}
    \[ {\s -> LSqBracketTok}
    \] {\s -> RSqBracketTok}
    Int {\s -> IntTok}
    Bool {\s -> BoolTok}
    Char {\s -> CharTok}
    if {\s -> IfTok}
    else {\s -> ElseTok}
    while {\s -> WhileTok}
    return {\s -> ReturnTok}
    False {\s -> FalseTok}
    True {\s -> TrueTok}
    \[\] {\s -> EmptyListTok}
    hd {\s -> HdTok}
    tl {\s -> TlTok}
    fst {\s -> FstTok}
    snd {\s -> SndTok}
    \. {\s -> DotTok}
    \+ {\s -> PlusTok}
    \- {\s -> MinusTok}
    \* {\s -> TimesTok}
    \/ {\s -> DivTok}
    \% {\s -> ModTok}
    \=\= {\s -> EqTok}
    \< {\s -> LtTok}
    \> {\s -> GtTok}
    \<\= {\s -> LeTok}
    \>\= {\s -> GeTok}
    !\= {\s -> NeTok}
    && {\s -> AndTok}
    \|\| {\s -> OrTok}
    : {\s -> ColonTok}
    ! {\s -> NotTok}
    \-?[0-9]+ {IntLitTok . read}
    [a-z][_a-z0-9]* {IdTok}

{
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
    deriving (Data, Show)
}
