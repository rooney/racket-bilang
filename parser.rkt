#lang brag

expres : /feeds? expr4
@expr4 : expr3 /feeds? /SPACE?
@expr3 : apply3|macro3
       | expr2
@expr2 : apply2|macro2|macro1
       | exprL
@exprL : break1|breakO|break
       | exprk
@exprk : comma1|commaO|comma
       | expr1
@expr1 : apply1
       | exprO
@exprO : applyO
       | atom|param
       | hole|bracket
       | prop|prot|dot
       | expr0
@expr0 : apply0
       | e

@mL    : (exprL|break) /SPACE
@mR    :               /SPACE exprk
macro1 :     @op kwargs
       | mL  @op kwargs?
       | mL? @op kwargs? mR
macro2 : mL? @op kwargs? mR? indent
macro3 : mL? @op kwargs? mR? indent? /feeds expr3
macroB :     @op kwargs? mR? indent

comma  : (commaO|comma1|expr1) /SPACE?  /COMMA
break  : (breakO|break1)       /SPACE?  /COMMA
       |                expr2  /NEWLINE /COMMA
break2 : (apply2|macro2)       /NEWLINE /COMMA

break1 : (break|breakO) /SPACE expr1
comma1 : (comma|commaO) /SPACE expr1

breakO : break (op|dot|bracket)+
commaO : comma (op|dot|bracket)+

apply3 : (exprL|apply2) /feeds expr3
apply2 : (exprL|break2) kwargs? (/SPACE kv2|indent)
apply1 :  exprO         kwargs? /SPACE (kv|expr1)
applyO :  exprO (op|dot|bracket)+
apply0 : (expr0|op) e
       |  exprO string
@e     : id
       | int|dec
       | string

int    : INT
dec    : DEC
op     : OP|DASH|SLASH|THROW|CATCH|DOT DOT
id     : ID|/LPAREN @op /RPAREN
@key   : @id @dot* @op?
kv     : key /COLON exprO
kv1    : key /COLON /SPACE (expr1|macro1)
kv2    : key /COLON /SPACE (expr2|macro2)
param  : (key /COLON)? /LPAREN /COLON COLON? (@op|DASH? key?) /RPAREN
atom   :                       /COLON COLON? (@op|DASH? ID?)
prop   : /DOT (DASH ID (DOT key)?|key) /COLON
prot   : /DOT DASH @id
dot    : (DOT|SLASH) @id
hole   : /HOLE
string : /QUOTE /INDENT (STRING|interp|NEWLINE)* /DEDENT /UNQUOTE
       | /QUOTE         (STRING|interp)*                 /UNQUOTE
interp : INTERPOLATE    (braces|indent)

parens   : /LPAREN subexpr /RPAREN
braces   : /LBRACE subexpr /RBRACE
square   : /LSQUARE csv    /RSQUARE
@bracket : square|parens|braces
@subexpr : @indent /feeds
         | /SPACE? expr4?
kwargs   : (/SPACE kv)+
@csv     :                /SPACE? (expr1 (/NEWLINE? /COMMA /SPACE expr1)* /COMMA? /SPACE?)?
         | /INDENT (/HOLE /SPACE)? expr1 (/NEWLINE? /COMMA /SPACE expr1)* /COMMA? /NEWLINE* /DEDENT /NEWLINE
indent   : /INDENT expres    /DEDENT
pseudent : /INDENT pseudent? /DEDENT
feeds    : /(NEWLINE|pseudent)+
