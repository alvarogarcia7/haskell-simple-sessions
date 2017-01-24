module ParserWithParsec where

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language (javaStyle)

-- From http://stackoverflow.com/a/26539615/1181094
-- modified to support boolean expressions

lexer = makeTokenParser javaStyle

expr = buildExpressionParser table term
     <?> "expression"

term = identifier lexer 
     <?> "simple expression"

table = [ 
         [prefix "NOT" not_]
        ,[binary "AND" (and_) AssocLeft]
        ,[binary "OR" (or_) AssocLeft]
        ]

binary  name fun assoc = Infix (do{ reservedOp lexer name; return fun }) assoc
prefix  name fun       = Prefix (do{ reservedOp lexer name; return fun })

parseExpression :: String -> Either ParseError String
parseExpression expression = parse expr "(unknown)" expression

not_ "T" = "F" 
not_ _ = "T"

and_ "T" "T" = "T"
and_ _ _ = "F"

or_ "F" "F" = "F"
or_ _ _ = "T"
