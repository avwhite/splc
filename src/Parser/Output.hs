{-# LANGUAGE RankNTypes #-}
module Parser.Output where

import Data.Data
--import Data.Generics.Aliases
import Data.Tree
import Parser.AST

--THE FOLLWING DEFINITIONS (Q, ext1, extQ, ext1Q) ARE COPIED ALMOST VERBITAM
--FROM THE syb PACKAGE (not included in stackage). THESE WOULD NORMALLY BE 
--FOUND IN Data.Generics.Aliases

-- | The type constructor for queries
newtype Q q x = Q { unQ :: x -> q }

-- | Flexible type extension
ext1 :: (Data a, Typeable t)
     => c a
     -> (forall d. Data d => c (t d))
     -> c a
ext1 def ext = maybe def id (dataCast1 ext)

-- | Extend a generic query by a type-specific case
extQ :: ( Typeable a
        , Typeable b
        )
     => (a -> q)
     -> (b -> q)
     -> a
     -> q
extQ f g a = maybe (f a) g (cast a)

-- | Type extension of queries for type constructors
ext1Q :: (Data d, Typeable t)
      => (d -> q)
      -> (forall e. Data e => t e -> q)
      -> d -> q
ext1Q def ext = unQ ((Q def) `ext1` (Q ext))

--END OF COPIED DEFINITIONS.

--Black generic programming magic happening here. 
data2tree :: Data a => a -> Tree String
data2tree = (gdefault `ext1Q` atList) `extQ` atString `extQ` atAstExp
  where
    atString :: String -> Tree String
    atString x = Node x []

    atList :: Data e => [e] -> Tree String
    atList x = Node "List:" (fmap data2tree x)

    atAstExp :: ASTExp -> Tree String
    atAstExp (Op2E o e1 e2) = 
        Node (showConstr (toConstr o)) [data2tree e1, data2tree e2]
    atAstExp (IntE i) = Node (show i) []
    atAstExp x = gdefault x

    gdefault x = Node (showConstr (toConstr x)) (gmapQ data2tree x)

drawAst :: Data t => t -> IO ()
drawAst = putStr . drawTree . data2tree

type Printer = [String] -> [String]

prettyPrint :: AST -> String
prettyPrint ast = mconcat $ printAST ast $ []

prnt :: String -> Printer
prnt s = \c -> s:c

prntList :: (a -> Printer) -> [a] -> Printer
prntList p l = foldl (.) id (fmap p l)

prntMany :: Int -> Printer -> Printer
prntMany 0 p = id
prntMany i p = p . (prntMany (i-1) p)

indent i = prntMany i (prnt "\t")

prntMaybe :: (a -> Printer) -> Maybe a -> Printer
prntMaybe p Nothing = id
prntMaybe p (Just a) = p a

prntListSep :: Printer -> (a -> Printer) -> [a] -> Printer
prntListSep sep p [] = id
prntListSep sep p (e:[]) = p e
prntListSep sep p (e:es) = p e . prntList (\x -> sep . p x) es

printType :: ASTType -> Printer
printType BoolT = prnt "Bool"
printType IntT = prnt "Int"
printType CharT = prnt "Char"
printType (PairT t1 t2) = 
      prnt "("
    . printType t1
    . prnt ","
    . printType t2
    . prnt ")"
printType (ListT t) = prnt "[" . printType t . prnt "]"
printType (PolyT i)  = prnt i

printRetType :: ASTReturnType -> Printer
printRetType Void = prnt "Void"
printRetType (ReturnType t) = printType t

printFunType :: ASTFunType -> Printer
printFunType (FunType ts rt) = 
      prntListSep (prnt " ") printType ts
    . prnt " -> "
    . printRetType rt

printOp1 :: Op1 -> Printer
printOp1 Neg = prnt "-"
printOp1 Not = prnt "!"

printOp2 :: Op2 -> Printer
printOp2 Plus = prnt "+"
printOp2 Minus = prnt "-"
printOp2 Times = prnt "*"
printOp2 Div = prnt "/"
printOp2 Mod = prnt "%"
printOp2 Equal = prnt "=="
printOp2 Less = prnt "<"
printOp2 Greater = prnt ">"
printOp2 LessEq = prnt "<="
printOp2 GreaterEq = prnt ">="
printOp2 NotEq = prnt "!="
printOp2 And = prnt "&&"
printOp2 Or = prnt "||"
printOp2 Cons = prnt ":"

printField :: Field -> Printer
printField Hd = prnt ".hd"
printField Tl = prnt ".tl"
printField Fst = prnt ".fst"
printField Snd = prnt ".snd"

printExp :: ASTExp -> Printer
printExp (Var i fs) = prnt i . prntList printField fs
--TODO: only print neccesary paranthesis
printExp (Op2E o e1 e2) = 
      prnt "("
    . printExp e1
    . prnt ")"
    . printOp2 o
    . prnt "("
    . printExp e2
    . prnt ")"
printExp (Op1E o e) = printOp1 o . prnt "(" . printExp e . prnt ")"
printExp (FunCallE i args) = 
      prnt i
    . prnt "("
    . prntListSep (prnt ", ") printExp args
    . prnt ")"
printExp (IntE i) = prnt (show i)
printExp (CharE c) = prnt [c]
printExp (BoolE True) = prnt "True"
printExp (BoolE False) = prnt "False"
printExp NilE = prnt "[]"
printExp (PairE e1 e2) = 
      prnt "("
    . printExp e1
    . prnt ","
    . printExp e2
    . prnt ")"

printStmt i (IfS e1 ss1 (Just ss2)) = 
      indent i . prnt "if (" . printExp e1 . prnt ") {\n"
    . prntList (printStmt (i+1)) ss1
    . indent i . prnt "} else {\n"
    . prntList (printStmt (i+1)) ss2
    . indent i . prnt "}\n"
printStmt i (IfS e ss Nothing) = 
      indent i . prnt "if (" . printExp e . prnt ") {\n"
    . prntList (printStmt (i+1)) ss
    . indent i . prnt "}\n"
printStmt i (WhileS e ss) = 
      indent i . prnt "while (" . printExp e . prnt ") {\n"
    . prntList (printStmt (i+1)) ss
    . indent i . prnt "}\n"
printStmt i (AssignS id fs e) =
      indent i . prnt id . prntList printField fs . prnt " = "
    . printExp e . prnt ";\n"
printStmt i (FunCallS id args) =
      indent i . prnt id
    . prnt "("
    . prntListSep (prnt ", ") printExp args
    . prnt ");\n"
printStmt i (ReturnS (Just e)) =
      indent i . prnt "return " . printExp e . prnt ";\n"
printStmt i (ReturnS Nothing) = indent i . prnt "return;\n"

printVarDecl i (VarDecl (Just t) id e) =
      indent i
    . printType t . prnt " " . prnt id
    . prnt " = " . printExp e . prnt ";\n"
printVarDecl i (VarDecl Nothing id e) =
      indent i . prnt "var " . prnt id . prnt " = " . printExp e . prnt ";\n"

printFunDecl i (FunDecl id args mt dcls ss) =
      indent i . prnt id . prnt "("
    . prntListSep (prnt ", ") prnt args
    . prnt ") :: " . prntMaybe printFunType mt . prnt " {\n"
    . prntList (printVarDecl (i+1)) dcls
    . prntList (printStmt (i+1)) ss
    . indent i . prnt "}\n"

printDecl i (FunD d) = printFunDecl i d
printDecl i (VarD d) = printVarDecl i d

printAST dcls = prntListSep (prnt "\n") (printDecl 0) dcls
