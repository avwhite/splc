{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Parser.Output where

import Data.Data
--import Data.Generics.Aliases
import Data.Tree
import Diagrams.TwoD.Layout.Tree
import Diagrams.Prelude
import Diagrams.Backend.SVG

--Black generic programming magic happening here. 
data2tree :: Data a => a -> Tree String
--data2tree = gdefault `extQ` atString
data2tree = gdefault
  where
    --atString (x::String) = Node x []
    gdefault x = Node (showConstr (toConstr x)) (gmapQ data2tree x)

--This type restricts us to the SVG backend (I think), but lets us do stuff
--without adding type annotations
graphicalData :: Data t => t -> Diagram B
graphicalData t =
  renderTree ((<> circle 1 # scaleX 4.0 # fc white) . text)
             (~~)
             (symmLayout' (with & slHSep .~ 8 & slVSep .~ 8) (data2tree t)) # centerXY # pad 1.1
