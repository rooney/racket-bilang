#lang brag

expres : /feeds? expr4
@expr4 : expr3 /(SPACE|feeds)?
@expr3 : apply3|macro3|combo
       | exprZ
@exprZ : combo0|combo1
       | macro0|macro1|macroL
       | expr2
@expr2 : apply2|comma
       | exprK
@exprK : comma1|comma0
       | expr1
@expr1 : apply1
       | exprQ
@exprQ : k0
       | key /COLON
       | spread
       | expr0
@expr0 : apply0
       | bottom
       | atom
       | exprG
@exprG : applyG
       | grouping
       | prop
       | this
       | e

@m1    : (comma|comma0|comma1|expr1) /SPACE
@mL    : (combo|combo0|combo1)       /SPACE
@mR    :                             /SPACE (exprK|macro1)
macro0 :          op
macro1 :          op mR
       |  m1      op mR?
macroL :     mL   op mR?
macro2 : (m1|mL)? op mR? dent
macro3 : (m1|mL)? op mR? dent? /feeds expr3

@comma : (comma0|comma1|expr1) /SPACE? /COMMA
@combo : (combo0|combo1)       /SPACE? /COMMA
       |                exprZ  /NEWLINE /COMMA

comma1 : (comma|comma0) /SPACE expr1
combo1 : (combo|combo0) /SPACE expr1
comma0 :  comma         (prop|op|grouping)+
combo0 :  combo         (prop|op|grouping)+

apply3 : expr2 /feeds expr3
apply2 : (expr1|comma0|combo0|comma|combo) dent
apply1 : exprQ /SPACE expr1
apply0 : (applyG|grouping) e
       | (exprG|op) (prop|op)+
applyG : (expr0|op) grouping+
apply  : (times|id|op) (int|dec|id|times+)
parse  : (times|id|op|apply) string
times  : (int|dec) id
@e     : times|parse|apply|string|int|dec|id

op     : OP
id     : ID
idx    : ID (/DOT ID)*
int    : INTEGER
dec    : DECIMAL
k0     : key /COLON expr0
key    : idx | (OP|ID|INTEGER|DECIMAL)+
atom   : (COLON @key?)? COLON @idx?
prop   : /DOT (OP|OP? @id|grouping)
this   : /THIS
feeds  : /(NEWLINE|pseudent)+
string : /QUOTE /INDENT (STRING|interp|NEWLINE)* /DEDENT /UNQUOTE
       | /QUOTE         (STRING|interp)*                 /UNQUOTE
interp : INTERPOLATE (brace|dent)

@grouping : @paren|bracket|brace|generator
paren     : /LPAREN grouped? /RPAREN
bracket   : /LBRACK grouped? /RBRACK
brace     : /LBRACE grouped? /RBRACE
bottom    : /LBRACE /SPREAD  /RBRACE
generator : /LBRACE /SPREAD /SPACE expr4      /RBRACE
          | /LBRACE /SPREAD (id|@dent) /RBRACE
@grouped  : /SPACE? expr4 /SPACE?
          | @dent /feeds
          | OP
spread    : /SPREAD (id|grouped)
dent      : /INDENT expres    /DEDENT
pseudent  : /INDENT pseudent? /DEDENT
