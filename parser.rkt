#lang brag

expres : /feeds? expr4
@expr4 : expr3 /(SPACE|feeds)?
@expr3 : apply3|macro3|break
       | expr2
@expr2 : apply2|macro2
       | macroL|macro1
       | exprL
@exprL : break1|breakO
       | exprk
@exprk : comma1|commaO|comma
       | expr1
@expr1 : apply1
       | exprQ
@exprQ : keyv
       | prop
       | exprO
@exprO : applyO
       | dot
       | atom
       | param
       | hole
       | bracket
       | expr0
@expr0 : apply0
       | e

macro  : DOTS|@op
@m1    : (comma|commaO|comma1|expr1) /SPACE
@mL    : (break|breakO|break1)       /SPACE
@mR    :                             /SPACE (exprk|macro1)
macro1 :          macro mR
       |     m1   macro mR?
macroL :  mL      macro mR?
macro2 : (mL|m1)? macro mR? indent
macro3 : (mL|m1)? macro mR? indent? /feeds expr3

comma  : (commaO|comma1|expr1) /SPACE?  /COMMA
break  : (breakO|break1)       /SPACE?  /COMMA
       |                expr2  /NEWLINE /COMMA

comma1 : (comma|commaO) /SPACE expr1
break1 : (break|breakO) /SPACE expr1

commaO : comma (op|dot|bracket)+
breakO : break (op|dot|bracket)+

apply3 : (exprL|apply2) /feeds expr3
apply2 :  exprL indent
apply1 :  exprQ /SPACE expr1
applyO :  exprO (op|dot|bracket)+
apply0 : (expr0|op) e
@e     : id
       | int|dec
       | string

int    : INT
dec    : DEC
op     : OP|SLASH
id     : ID
@opid  : @op|ID
@keyf  : @op|ID|INT|DEC|DOTS
@idx   : opid   (DOT opid)* op? | braces
key    : keyf+ (/DOT keyf+)*    | braces | @key? DOTS+ DOT
keyv   : @key /COLON exprO?
atom   :      /COLON @key?
param  : /LPAREN /COLON ((DOT|SLASH)? @idx)? /RPAREN
prop   :                 (DOT|SLASH)  @idx /COLON exprO?
dot    :                 (DOT|SLASH) (@idx|bracket)
       |                  DOT int
hole   : /HOLE
string : /QUOTE /INDENT (STRING|interp|NEWLINE)* /DEDENT /UNQUOTE
       | /QUOTE         (STRING|interp)*                 /UNQUOTE
interp : INTERPOLATE    (braces|indent)

parens   : /LPAREN subexpr /RPAREN
braces   : /LBRACE subexpr /RBRACE
square   : /LSQUARE csv    /RSQUARE
@bracket : square|parens|braces
@subexpr : op
         | @indent /feeds
         | /SPACE? expr4? /SPACE?
@csv     :                /SPACE? (expr1 (/NEWLINE? /COMMA /SPACE expr1)* /COMMA? /SPACE?)?
         | /INDENT (/hole /SPACE)? expr1 (/NEWLINE? /COMMA /SPACE expr1)* /COMMA? /NEWLINE* /DEDENT /NEWLINE
indent   : /INDENT expres    /DEDENT
pseudent : /INDENT pseudent? /DEDENT
feeds    : /(NEWLINE|pseudent)+
