module Parser where

import Parser.AST
import Parser.Combinators
import Parser.Scanner
import Parser.Parser

parseSpl src = do
    tokens <- toEither $ scan src
    ast <- toEither $ parse (splp <* eof) tokens
    return ast
