{-# LANGUAGE RankNTypes #-}
module Parser.Output where

import Data.Data
--import Data.Generics.Aliases
import Data.Tree

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
data2tree = (gdefault `ext1Q` atList) `extQ` atString
  where
    atString :: String -> Tree String
    atString x = Node x []

    atList :: Data e => [e] -> Tree String
    atList x = Node "List:" (fmap data2tree x)

    gdefault x = Node (showConstr (toConstr x)) (gmapQ data2tree x)

drawAst :: Data t => t -> IO ()
drawAst = putStr . drawTree . data2tree
