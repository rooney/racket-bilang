#lang brag

expres : /feeds? expr5
@expr5 : expr3 /feeds? /SPACE?
       | expr4
@expr4 : apply4
       | props
@expr3 : apply3|macro3|break
       | expr2
@expr2 : apply2|macro2|macro1
       | exprL
@exprL : break1|breakO
       | exprk
@exprk : comma1|commaO|comma
       | expr1
@expr1 : apply1
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

props  : prop (/SPACE prop)*

@mL    : (exprL|break) /SPACE
@mR    :               /SPACE exprk
macro  : @op | DOT DOT
macro1 :     macro kwargs
       | mL  macro kwargs?
       | mL? macro kwargs? mR
macro2 : mL? macro kwargs? mR? indent
macro3 : mL? macro kwargs? mR? indent? /feeds expr3

comma  : (commaO|comma1|expr1) /SPACE?  /COMMA
break  : (breakO|break1)       /SPACE?  /COMMA
       |                expr2  /NEWLINE /COMMA

comma1 : (comma|commaO) /SPACE expr1
break1 : (break|breakO) /SPACE expr1

commaO : comma (op|dot|bracket)+
breakO : break (op|dot|bracket)+

apply4 : (exprL|apply2) /feeds props
apply3 : (exprL|apply2) /feeds expr3
apply2 :  exprL kwargs? (/SPACE kv2|indent)
apply1 :  exprO kwargs? /SPACE (kv|expr1)
applyO :  exprO (op|dot|bracket)+
apply0 : (expr0|op) e
       |  exprO string
@e     : id
       | int|dec
       | string

int    : INT
dec    : DEC
op     : OP|SLASH|THROW|CATCH
dot    : (DOT|SLASH) key
@key   : @id (/DOT @id)* @op? | braces
id     : ID | /LPAREN @op /RPAREN
kv     : key /COLON exprO
kv1    : key /COLON /SPACE (expr1|macro1)
kv2    : key /COLON /SPACE macro2
prop   : @dot /COLON exprO
atom   :         /COLON key?
param  : /LPAREN /COLON key? /RPAREN
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
         | /SPACE? expr5?
kwargs   : (/SPACE kv)+
@csv     :                /SPACE? (expr1 (/NEWLINE? /COMMA /SPACE expr1)* /COMMA? /SPACE?)?
         | /INDENT (/hole /SPACE)? expr1 (/NEWLINE? /COMMA /SPACE expr1)* /COMMA? /NEWLINE* /DEDENT /NEWLINE
indent   : /INDENT expres    /DEDENT
pseudent : /INDENT pseudent? /DEDENT
feeds    : /(NEWLINE|pseudent)+
