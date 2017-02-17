module Parser.Output where

import Data.Data
--import Data.Generics.Aliases
import Data.Tree

--Black generic programming magic happening here. 
data2tree :: Data a => a -> Tree String
--data2tree = gdefault `extQ` atString
data2tree = gdefault
  where
    --atString (x::String) = Node x []
    gdefault x = Node (showConstr (toConstr x)) (gmapQ data2tree x)

drawAst :: Data t => t -> IO ()
drawAst = putStr . drawTree . data2tree
