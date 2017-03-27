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

type Inference a = StateT Int Maybe a

infer :: Inference a -> Maybe a
infer a = evalStateT a 0

freshVar :: Inference Type
freshVar = do
    freshVar <- get
    put (freshVar + 1)
    pure (TVar (InternalTV freshVar))


data TypeScheme = TypeScheme [TVarId] Type

newtype TypeContext = TypeContext [(Identifier,Type)] deriving (Monoid)

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

instance (Substable a, Functor m) => Substable (StateT s m a) where
    subst s = fmap (subst s)

instance (Substable a) => Substable (Maybe a) where
    subst s = fmap (subst s)

instance (Substable a, Substable b) => Substable (a -> b) where
    subst s f = \a -> subst s (f (subst s a))


--When composing substitutions the resulting list will have overwritten
--Substitutions at the end using this definition.
instance Monoid Substitution where
    mempty = Substitution []
    mappend (Substitution s1) s2'@(Substitution s2) = Substitution $
        fmap (\(id,t) -> (id, subst s2' t)) s1 ++ s2

infixr 6 <>
(<>) :: (Monoid a) => a -> a -> a
(<>) = mappend

vars :: Type -> Set.Set TVarId
vars (TPair t1 t2) = (vars t1) `union` (vars t2)
vars (TList t) = vars t
vars (TArrow ts t) = (unions $ fmap vars ts) `union` (vars t)
vars (TVar a) = singleton a
vars t = empty

mgu' t1 t2 s = subst s mgu t1 t2

mgu :: Type -> Type -> Maybe Substitution
mgu (TVar id1) (TVar id2)
    | id1 == id2 = Just mempty
    | otherwise = Just (Substitution [(id1, (TVar id2))])
mgu (TVar id) t
    | notMember id (vars t) = Just (Substitution [(id, t)])
    | otherwise = Nothing
mgu t (TVar id)
    | notMember id (vars t) = Just (Substitution [(id, t)])
    | otherwise = Nothing
mgu (TList t) (TList s) = mgu t s
mgu (TPair t1 t2) (TPair u1 u2) =
    mgu t1 u1 >>=
    mgu' t2 u2
mgu (TArrow ts t) (TArrow us u) =
    mgu_aux mempty ts us >>=
    mgu' t u where
        mgu_aux s [] [] = Just s
        mgu_aux s (t1:t2) (u1:u2) = do
            s' <- mgu (subst s t1) (subst s u1)
            mgu_aux (s <> s') t2 u2
mgu t1 t2
    | t1 == t2 = Just mempty
    | otherwise = Nothing

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

typeInferExp' e ctx t s = subst s (typeInferExp e) ctx t

typeInferExp :: ASTExp -> TypeContext -> Type -> Inference Substitution
typeInferExp (IntE _) ctx t = lift $ mgu t TInt
typeInferExp (BoolE _) ctx t = lift $ mgu t TBool
typeInferExp (CharE _) ctx t = lift $ mgu t TChar
typeInferExp NilE ctx t =
    freshVar >>= \a ->
    lift (mgu t (TList a))
typeInferExp (Op2E Cons e1 e2) ctx t =
    freshVar >>= \a ->
    typeInferExp  e1 ctx a >>=
    typeInferExp' e2 ctx (TList a) >>=
    lift . (mgu' t (TList a))
typeInferExp (Op2E o e1 e2) ctx t =
    typeInferExp  e1 ctx (opInType o) >>=
    typeInferExp' e2 ctx (opInType o) >>=
    lift . (mgu' t (opOutType o))

--Keep this for the presentation to show before vs after
--typeInferExp (Op2E o e1 e2) ctx t = do
--    s1 <- typeInferExp e1 ctx (opInType o)
--    s2 <- typeInferExp e2 (subst s1 ctx) (opInType o)
--    s3 <- lift $ mgu (subst (s1 <> s2) t) (opOutType o)
--    pure (s1 <> s2 <> s3)
