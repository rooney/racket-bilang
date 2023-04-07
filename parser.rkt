#lang brag

expres : /feeds? expr4
@expr4 : expr3 /(SPACE|feeds)?
@expr3 : apply3|macro3|cline
       | expr2
@expr2 : apply2|macro2
       | macroL|macro1
       | exprL
@exprL : cline1|clineO
       | exprk
@exprk : comma1|commaO|comma
       | expr1
@expr1 : apply1
       | exprQ
@exprQ : keyval|key /COLON
       | prop|field /COLON
       | xtend
       | exprO
@exprO : applyO
       | hole
       | dot
       | atom
       | grouping
       | expr0
@expr0 : apply0
       | e

macro  : DOTDOT|OP
@m1    : (comma|commaO|comma1|expr1) /SPACE
@mL    : (cline|clineO|cline1)       /SPACE
@mR    :                             /SPACE (exprk|macro1)
macro1 :          macro mR
       |     m1   macro mR?
macroL :  mL      macro mR?
macro2 : (mL|m1)? macro mR? indent
macro3 : (mL|m1)? macro mR? indent? /feeds expr3

@comma : (commaO|comma1|expr1) /SPACE?  /COMMA
@cline : (clineO|cline1)       /SPACE?  /COMMA
       |                expr2  /NEWLINE /COMMA

comma1 : (comma|commaO) /SPACE expr1
cline1 : (cline|clineO) /SPACE expr1

commaO : comma (op|dot|grouping)+
clineO : cline (op|dot|grouping)+

apply3 : (exprL|apply2) /feeds expr3
apply2 :  exprL indent
apply1 :  exprQ /SPACE expr1
applyO :  exprO(op|dot|grouping)+
       |        op(dot|grouping)+
apply0 : (expr0|op)e
@e     : id
       | int|dec
       | string

op     : OP
id     : ID
idx    : ID (/DOT ID)*
int    : INTEGER
dec    : DECIMAL
key    : idx | (OP|ID|INTEGER|DECIMAL)+
pubkey :         /COLON key?
keyval : @key    /COLON exprO
prop   : @field  /COLON exprO
atom   : pubkey? /COLON idx?
field  : /DOT (OP|OP? ID)
dot    : /DOT (OP|OP? ID|grouping)
hole   : /HOLE
string : /QUOTE /INDENT (STRING|interp|NEWLINE)* /DEDENT /UNQUOTE
       | /QUOTE         (STRING|interp)*                 /UNQUOTE
interp : INTERPOLATE (brace|indent)

@grouping : paren|bracket|brace|generator
paren     : /LPAREN grouped? /RPAREN
bracket   : /LBRACK grouped? /RBRACK
brace     : /LBRACE grouped? /RBRACE
generator : /LBRACE /DOTDOT /SPACE expr4 /RBRACE
          | /LBRACE /DOTDOT (id|@indent) /RBRACE
@grouped  : /SPACE? expr4 /SPACE?
          | @indent /feeds
          | op
          | DOTDOT
xtend     : /DOTDOT (id|grouping)
indent    : /INDENT expres /DEDENT
pseudent  : /INDENT pseudent? /DEDENT
feeds     : /(NEWLINE|pseudent)+
