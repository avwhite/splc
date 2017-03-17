{-# LANGUAGE DeriveDataTypeable #-}
module Semantics.Types where
    
import Data.Maybe
import Data.Data
import Data.Set as Set

type TVarId = String

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

data TypeScheme = TypeScheme [TVarId] Type

data Substitution = Substitution [(TVarId, Type)] deriving (Show)

subst :: Substitution -> Type -> Type
subst (Substitution s) = tmap subst' where
    subst' :: TVarId -> Type
    subst' id = fromMaybe (TVar id) (lookup id s)

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
mgu (TPair t1 t2) (TPair u1 u2) = do
    s  <- mgu t1 u1
    s' <- mgu (subst s t2) (subst s u2)
    pure (s <> s')
mgu (TArrow ts t) (TArrow us u) = do
    s <- mgu_aux mempty ts us
    s' <- mgu (subst s t) (subst s u)
    pure (s <> s') where
        mgu_aux s [] [] = Just s
        mgu_aux s (t1:t2) (u1:u2) = do
            s' <- mgu (subst s t1) (subst s u1)
            mgu_aux (s <> s') t2 u2
mgu t1 t2
    | t1 == t2 = Just mempty
    | otherwise = Nothing
