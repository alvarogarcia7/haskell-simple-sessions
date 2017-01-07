import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language (javaStyle)

-- From http://stackoverflow.com/a/26539615/1181094

lexer = makeTokenParser javaStyle

expr    = buildExpressionParser table term
        <?> "expression"

term    =  parens lexer expr 
        <|> natural lexer
        <?> "simple expression"

table   = [ [prefix "-" negate, prefix "+" id ]
        , [postfix "++" (+1)]
        , [binary "*" (*) AssocLeft, binary "/" (div) AssocLeft ]
        , [binary "+" (+) AssocLeft, binary "-" (-)   AssocLeft ]
        ]

binary  name fun assoc = Infix (do{ reservedOp lexer name; return fun }) assoc
prefix  name fun       = Prefix (do{ reservedOp lexer name; return fun })
postfix name fun       = Postfix (do{ reservedOp lexer name; return fun })

parseExpression :: String -> Either ParseError Integer
parseExpression expression = parse expr "(unknown)" expression
