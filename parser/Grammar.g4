grammar Grammar;

stat: expr (';' expr)* ';'?;

expr:   expr 'AND' expr
    |   expr 'OR' expr
    |   'NOT' expr
    |   BOOL         
    ;

BOOL: 'T' | 'F' ;

WS  :   [ \t\n\r]+ -> skip ;

