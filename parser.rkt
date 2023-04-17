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
@exprQ : key  /COLON | kv
       | comp /COLON | qual
       | quot
       | xtend
       | exprO
@exprO : applyO
       | circle
       | hole
       | dot
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

commaO : comma (op|dot|circle)+
clineO : cline (op|dot|circle)+

apply3 : (exprL|apply2) /feeds expr3
apply2 :  exprL indent
apply1 :  exprQ /SPACE expr1
applyO :  exprO(op|dot|circle)+
       |        op(dot|circle)+
apply0 : (expr0|op)e
@e     : id
       | int|dec
       | string

op     : OP
id     : ID
@idx   : ID (/DOT ID)*
int    : INTEGER
dec    : DECIMAL
key    : (OP|ID|INTEGER|DECIMAL)+|braces|idx
pub    :       /COLON @key?
kv     : @key  /COLON exprO
qual   : @comp /COLON exprO
quot   : pub?  /COLON (braces|idx)?
comp   : /DOT (OP|ID|braces)
dot    : /DOT (OP|ID|circle|int)
hole   : /HOLE
string : /QUOTE /INDENT (STRING|interp|NEWLINE)* /DEDENT /UNQUOTE
       | /QUOTE         (STRING|interp)*                 /UNQUOTE
interp : INTERPOLATE (braces|indent)

@circle  : parens|brackets|braces|loop
parens   : /LPAREN group? /RPAREN
brackets : /LBRACK tuple? /RBRACK
braces   : /LBRACE group? /RBRACE
loop     : /LBRACE /DOTDOT /SPACE expr4 /RBRACE
         | /LBRACE /DOTDOT (id|@indent) /RBRACE
@group   : /SPACE? expr4 /SPACE?
         | op
         | @indent /feeds
         | DOTDOT
xtend    : /DOTDOT (id|circle)
@tuple   :               /SPACE? expr1 (/NEWLINE? /COMMA /SPACE expr1)* /COMMA? /SPACE?
         | /INDENT /hole /SPACE  expr1 (/NEWLINE? /COMMA /SPACE expr1)* /COMMA? /NEWLINE* /DEDENT /NEWLINE
indent   : /INDENT expres /DEDENT
pseudent : /INDENT pseudent? /DEDENT
feeds    : /(NEWLINE|pseudent)+
