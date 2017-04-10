{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Semantics.Types where
    
import Data.Maybe
import Data.Data
import Data.Set as Set

import Control.Monad.State

import Parser.AST

data TVarId = NamedTV String | InternalTV Int deriving (Data, Ord, Show, Eq)

data Type =
      TBool
    | TInt
    | TChar
    | TVoid
    | TPair Type Type
    | TList Type
    | TArrow [Type] Type
    | TVar TVarId deriving (Data, Show, Eq)

tmap :: (TVarId -> Type) -> Type -> Type
tmap f (TPair t1 t2) = TPair (tmap f t1) (tmap f t2)
tmap f (TList t) = TList (tmap f t)
tmap f (TArrow ts t) = TArrow (fmap (tmap f) ts) (tmap f t)
tmap f (TVar a) = f a
tmap f t = t

data StmtListType = Function | Control deriving (Eq)
data ReturnStatus = Found | NotFound deriving (Eq)

setRets :: Bool -> [Bool] -> [Bool]
setRets rs [] = error "Return status stack empty"
setRets rs (l:ls) = rs:ls

data InfState = InfState {
    ctx :: [TypeContext],
    freshNum :: Int,
    rets :: [Bool]
    } deriving (Show)

data TypeError = NoUnify Type Type | NotInContext Identifier deriving (Show)

type Inference a = StateT InfState (Either TypeError) a

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e (Just a) = Right a
maybeToEither e Nothing = Left e

infer :: Inference a -> Either TypeError a
infer a = evalStateT a
    (InfState {freshNum = 0, rets = [False], ctx = [mempty]})

freshVar :: Inference Type
freshVar = do
    state <- get
    put (state {freshNum = (freshNum state) + 1})
    pure (TVar (InternalTV (freshNum state)))

implicitReturnNeeded :: Inference Bool
implicitReturnNeeded = do
    state <- get
    pure (rets state == [False])

returnFound :: Inference ()
returnFound = do
    state <- get
    put (state {rets = setRets True (rets state)})

enterControl :: Inference ()
enterControl = do
    state <- get
    put (state {rets = False:(rets state)})

leaveControl :: Int -> Bool -> Inference ()
leaveControl i b = do
    state <- get
    let (subRets,(h:t)) = splitAt i (rets state)
    let status = b && all id subRets
    put (state {rets = (status || h):t})

ctxLookup :: String -> Inference TypeScheme
ctxLookup id = do
    state <- get
    let (TypeContext c) = head $ ctx state
    lift $ maybeToEither (NotInContext id) (lookup id c)

ctxAdd :: (String, TypeScheme) -> Inference ()
ctxAdd b = do
    state <- get
    let h = head $ ctx state
    let t = tail $ ctx state
    put (state {ctx = (h `with` b):t})

pushCtx :: Inference ()
pushCtx = do
    state <- get
    let c = ctx state
    put (state {ctx = (head c):c})

popCtx :: Inference ()
popCtx = do
    state <- get
    let c = ctx state
    put (state {ctx = tail c})

getCtx :: Inference TypeContext
getCtx = do
    state <- get
    pure (head $ ctx state)

data TypeScheme = TypeScheme [TVarId] Type deriving (Show)

newtype TypeContext = TypeContext [(Identifier,TypeScheme)] deriving (Monoid, Show)

--Should have a better Eq instance!!!
data Substitution = Substitution [(TVarId, Type)] deriving (Show, Eq)

class Substable a where
    subst :: Substitution -> a -> a

instance Substable Type where
    subst (Substitution s) t = tmap subst' t where
        subst' :: TVarId -> Type
        subst' id = fromMaybe (TVar id) (lookup id s)

instance Substable TypeContext where
    subst s (TypeContext c) = TypeContext $ fmap (\(id,t) -> (id, subst s t)) c

--TODO: Should the variables bound in the quantifier by ignored?
instance Substable TypeScheme where
    subst s (TypeScheme a t) = TypeScheme a (subst s t)

instance Substable Substitution where
    subst s1 s2 = s1 <> s2

instance (Substable a, Substable s, Functor m) => Substable (StateT s m a) where
    subst s = fmap (subst s) . withStateT (subst s)

instance (Substable a) => Substable (Either e a) where
    subst s = fmap (subst s)

instance (Substable a, Substable b) => Substable (a -> b) where
    subst s f = \a -> subst s (f (subst s a))

instance Substable InfState where
    subst s state = state {ctx = (subst s h):t} where
        h = head $ ctx state
        t = tail $ ctx state



--When composing substitutions the resulting list will have overwritten
--Substitutions at the end using this definition.
instance Monoid Substitution where
    mempty = Substitution []
    mappend (Substitution s1) s2'@(Substitution s2) = Substitution $
        fmap (\(id,t) -> (id, subst s2' t)) s1 ++ s2

infixr 6 <>
(<>) :: (Monoid a) => a -> a -> a
(<>) = mappend

synTypeToType :: ASTType -> Type
synTypeToType BoolT = TBool
synTypeToType IntT = TInt
synTypeToType CharT = TChar
synTypeToType (PairT t1 t2) =
    TPair (synTypeToType t1) (synTypeToType t2)
synTypeToType (ListT t) = TList (synTypeToType t)
synTypeToType (PolyT id) = TVar (NamedTV id)

synRetTypeToType :: ASTReturnType -> Type
synRetTypeToType Void = TVoid
synRetTypeToType (ReturnType t) = synTypeToType t

synFunTypeToType :: ASTFunType -> Type
synFunTypeToType (FunType argts rett) =
    TArrow (fmap synTypeToType argts) (synRetTypeToType rett)

concrete :: TypeScheme -> [Type] -> Type
concrete (TypeScheme vars t) ts = subst (Substitution (zip vars ts)) t

with :: TypeContext -> (Identifier, TypeScheme) -> TypeContext
with (TypeContext ctx) schm = TypeContext (schm:ctx)

vars :: Type -> Set.Set TVarId
vars (TPair t1 t2) = (vars t1) `union` (vars t2)
vars (TList t) = vars t
vars (TArrow ts t) = (unions $ fmap vars ts) `union` (vars t)
vars (TVar a) = singleton a
vars t = empty

ctxVars ::  TypeContext -> Set.Set TVarId
ctxVars (TypeContext []) = empty
ctxVars (TypeContext ((id, TypeScheme bounds t):rest)) =
        (vars t \\ fromList bounds)
        `union` ctxVars (TypeContext rest)

mgu' t1 t2 s = subst s mgu t1 t2

mgu :: Type -> Type -> Either TypeError  Substitution
mgu (TVar id1) (TVar id2)
    | id1 == id2 = Right mempty
    | otherwise = Right (Substitution [(id1, (TVar id2))])
mgu (TVar id) t
    | notMember id (vars t) = Right (Substitution [(id, t)])
    | otherwise = Left (NoUnify (TVar id) t)
mgu t (TVar id)
    | notMember id (vars t) = Right (Substitution [(id, t)])
    | otherwise = Left (NoUnify t (TVar id))
mgu (TList t) (TList s) = mgu t s
mgu (TPair t1 t2) (TPair u1 u2) =
    mgu t1 u1 >>=
    mgu' t2 u2
mgu (TArrow ts t) (TArrow us u) =
    mgu_aux mempty ts us >>=
    mgu' t u where
        mgu_aux s [] [] = Right s
        mgu_aux s (t1:t2) (u1:u2) = do
            s' <- mgu (subst s t1) (subst s u1)
            mgu_aux (s <> s') t2 u2
mgu t1 t2
    | t1 == t2 = Right mempty
    | otherwise = Left (NoUnify t1 t2)

opInType Plus = TInt
opInType Minus = TInt
opInType Times = TInt
opInType Div = TInt
opInType Mod = TInt
opInType Equal = TInt
opInType Less = TInt
opInType Greater = TInt
opInType LessEq = TInt
opInType GreaterEq = TInt
opInType NotEq = TInt
opInType And = TBool
opInType Or = TBool
opInType Cons = undefined

opOutType Plus = TInt
opOutType Minus = TInt
opOutType Times = TInt
opOutType Div = TInt
opOutType Mod = TInt
opOutType Equal = TBool
opOutType Less = TBool
opOutType Greater = TBool
opOutType LessEq = TBool
opOutType GreaterEq = TBool
opOutType NotEq = TBool
opOutType And = TBool
opOutType Or = TBool
opOutType Cons = undefined

typeInferExp' e t s = subst s (typeInferExp e) t

typeInferExp :: ASTExp -> Type -> Inference Substitution
typeInferExp (IntE _) t = lift $ mgu t TInt
typeInferExp (BoolE _) t = lift $ mgu t TBool
typeInferExp (CharE _) t = lift $ mgu t TChar
typeInferExp NilE t =
    freshVar >>= \a ->
    lift (mgu t (TList a))
typeInferExp (Op2E Cons e1 e2) t =
    freshVar >>= \a ->
    typeInferExp  e1 a >>=
    typeInferExp' e2 (TList a) >>=
    lift . (mgu' t (TList a))
typeInferExp (Op2E o e1 e2) t =
    typeInferExp  e1 (opInType o) >>=
    typeInferExp' e2 (opInType o) >>=
    lift . (mgu' t (opOutType o))
typeInferExp (Op1E Neg e) t =
    typeInferExp e t >>=
    lift . (mgu' t TInt)
typeInferExp (Op1E Not e) t =
    typeInferExp e TBool >>=
    lift . (mgu' t TBool)
typeInferExp (PairE e1 e2) t =
    freshVar >>= \a ->
    freshVar >>= \b ->
    typeInferExp e1 a >>=
    typeInferExp' e2 b >>=
    lift . (mgu' t (TPair a b))
typeInferExp (FunCallE id es) t = do
    vs <- replicateM (length es) freshVar
    s <- typeInferExp (Var id []) (TArrow vs t)
    foldM (flip $ \(e, a) -> typeInferExp' e a) s (zip es vs)
typeInferExp (Var id []) t = do
    schm@(TypeScheme bounds _) <- ctxLookup id
    vs <- replicateM (length bounds) freshVar
    lift $ mgu (concrete schm vs) t
typeInferExp (Var id fields) t = do
    let field = last fields
    let r = init fields
    case field of
        Hd -> do
            a <- freshVar
            typeInferExp (Var id r) (TList a) >>= lift . (mgu' t a)
        Tl -> do
            a <- freshVar
            typeInferExp (Var id r) (TList a)
                >>= lift . (mgu' t (TList a))
        Fst -> do
            a <- freshVar
            b <- freshVar
            typeInferExp (Var id r) (TPair a b) >>= lift . (mgu' t a)
        Snd  -> do
            a <- freshVar
            b <- freshVar
            typeInferExp (Var id r) (TPair a b) >>= lift . (mgu' t b)

typeInferStmtList' ss t s =
    subst s (typeInferStmtList ss) t

typeInferStmtList ::
           [ASTStmt]
        -> Type
        -> Inference Substitution
typeInferStmtList [] t = do
    b <- implicitReturnNeeded
    if b then
        lift $ mgu t TVoid
    else
        pure mempty
typeInferStmtList ((AssignS id fs e):ss) t = do
    schm@(TypeScheme bounds _) <- ctxLookup id
    vs <- replicateM (length bounds) freshVar
    s <- typeInferExp e (concrete schm vs)
    typeInferStmtList' ss t s
typeInferStmtList (ReturnS (Just e):ss) t =
    returnFound >>
    typeInferExp e t >>=
    typeInferStmtList' ss t
typeInferStmtList (ReturnS Nothing:ss) t =
    returnFound >>
    lift (mgu t TVoid) >>=
    typeInferStmtList' ss t
typeInferStmtList (WhileS e body:ss) t = do
    s1 <- typeInferExp e TBool
    enterControl
    pushCtx
    s2 <- typeInferStmtList' body t s1
    popCtx
    leaveControl 1 False
    typeInferStmtList' ss t s2
typeInferStmtList (IfS e body Nothing:ss) t = do
    s1 <- typeInferExp e TBool
    enterControl
    pushCtx
    s2 <- typeInferStmtList' body t s1
    popCtx
    leaveControl 1 False
    typeInferStmtList' ss t s2
typeInferStmtList (IfS e body1 (Just body2):ss) t = do
    s1 <- typeInferExp e TBool
    enterControl
    pushCtx
    s2 <- typeInferStmtList' body1 t s1
    popCtx
    enterControl
    pushCtx
    s3 <- typeInferStmtList' body2 t s2
    popCtx
    leaveControl 2 True
    typeInferStmtList' ss t s3

typeInferVarDeclList' ds t s =
    subst s (typeInferVarDeclList ds) t

typeInferVarDeclList :: [ASTVarDecl] -> Type -> Inference Substitution
typeInferVarDeclList [] t = pure mempty
typeInferVarDeclList ((VarDecl _ id e):ds) t =
    freshVar >>= \a ->
    ctxAdd (id, TypeScheme [] a) >>
    typeInferExp e a >>=
    typeInferVarDeclList' ds t
--typeInferVarDeclList ((VarDecl (Just anno) id e):ds) t =
--    ctxAdd (id, TypeScheme [] (synTypeToType anno)) >>
--    typeInferExp e (synTypeToType anno) >>=
--    typeInferVarDeclList' ds t

typeInferAst' ds t s = subst s (typeInferAst ds) t

typeInferAst :: AST -> Type -> Inference Substitution
typeInferAst (AST []) t = pure mempty
typeInferAst (AST (VarD (VarDecl _ id e):ds)) t =
    freshVar >>= \a ->
    ctxAdd (id, TypeScheme [] a) >>
    typeInferExp e a >>=
    typeInferAst' (AST ds) t
--typeInferAst (AST (VarD (VarDecl (Just anno) id e):ds)) t =
--    ctxAdd (id, TypeScheme [] (synTypeToType anno)) >>
-- typeInferExp e (synTypeToType anno) >>=
--    typeInferAst' (AST ds) t
--This is the only place we have the let binding without value
--restriction
typeInferAst (AST (FunD (FunDecl id args _ vds ss):ds)) t = do
    vs <- replicateM (length args) freshVar
    retType  <- freshVar
    funType<- freshVar
    pushCtx
    ctxAdd (id, TypeScheme [] funType)
    mapM (\(arg, v) -> ctxAdd (arg, TypeScheme [] v)) (zip args vs)
    s1 <- typeInferVarDeclList vds retType
                >>= typeInferStmtList' ss retType
    s2 <- lift $ mgu' funType (TArrow vs retType) s1
    popCtx
    ctx <- getCtx
    let freeTVars = vars (subst s2 funType) \\ ctxVars (subst s2 ctx)
    ctxAdd (id, TypeScheme (toList freeTVars) (subst s2 funType))
    typeInferAst' (AST ds) t s2
--typeInferAst (AST (FunD (FunDecl id args (Just anno) vds ss):ds)) t = do
--    let funType@(TArrow targs retType) = synFunTypeToType anno
--    ctx <- getCtx
--    let freeTVars = vars funType \\ ctxVars ctx
--    ctxAdd (id, TypeScheme (toList freeTVars) funType)
--    pushCtx
--    --ctxAdd (id, TypeScheme (toList $ vars funType) funType)
--    mapM (\(arg, v) -> ctxAdd (arg, TypeScheme [] v)) (zip args targs)
--    s <- typeInferVarDeclList vds retType
--                >>= typeInferStmtList' ss retType
--    popCtx
--    typeInferAst' (AST ds) t s

--typeInferStmt :: ASTStmt -> TypeContext -> Type -> Inference Substitution
--typeInferStmt (IfS


--Keep this for the presentation to show before vs after
--typeInferExp (Op2E o e1 e2) ctx t = do
--    s1 <- typeInferExp e1 ctx (opInType o)
--    s2 <- typeInferExp e2 (subst s1 ctx) (opInType o)
--    s3 <- lift $ mgu (subst (s1 <> s2) t) (opOutType o)
--    pure (s1 <> s2 <> s3)
