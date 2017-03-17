{-# LANGUAGE DeriveDataTypeable #-}
module Semantics.Types where
    
import Data.Maybe
import Data.Data

type TVarId = String

data Type =
      TBool
    | TInt
    | TChar
    | TVoid
    | TPair Type Type
    | TList Type
    | TArrow [Type] Type
    | TVar TVarId deriving (Data, Show)

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
