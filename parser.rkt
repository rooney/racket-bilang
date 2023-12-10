#lang brag

expres : /feeds? expr4
@expr4 : expr3 /feeds? /SPACE?
@expr3 : apply3
       | exprB
@exprB : break2|break1|breakO|break
       | exprZ
@exprZ : macroZ
       | expr2
@expr2 : apply2|comma2
       | exprk
@exprk : comma1|commaO|comma
       | expr1
@expr1 : apply1
       | exprO
@exprO : applyO
       | atom
       | expr0
@expr0 : apply0
       | dot
       | prop
       | slash
       | bracket
       | self
       | it
       | e

@mL    : (exprk|break1|breakO|break) /SPACE
@mJ    :                             /SPACE exprk
@mZ    :                             /SPACE exprZ
@macro : LPAREN RPAREN
       | LBRACE RBRACE
       | DOT COLON
       | def COLON
       | BUBLET
       | op
macroZ :     macro kwargs
       | mL  macro kwargs?
       | mL? macro kwargs? mZ
       | mL? macro kwargs? mJ indent?
       | mL? macro kwargs?    indent

comma  : (commaO|comma1|apply1) /SPACE?  /COMMA
break  : (breakO|break1)        /SPACE?  /COMMA
       |                exprB   /NEWLINE /COMMA
break1 : (@break|breakO) kwargs? /SPACE (kv |expr1)
break2 : (@break|breakO) kwargs? /SPACE (kv2|apply2)
comma1 : (@comma|commaO) kwargs? /SPACE (kv |expr1)
comma2 : (@comma|commaO) kwargs? /SPACE (kv2|apply2)

breakO : @break (bracket|dot|slash|op)+
commaO : @comma (bracket|dot|slash|op)+

apply3 : (expr2|break2|break1|breakO|break|macro|macroZ) /feeds expr3
apply2 :  exprO /SPACE apply2
       |  exprO kwargs? (/SPACE kv2|indent)
apply1 :  exprO kwargs? /SPACE (kv|expr1)
applyO :  atom   bracket+
apply0 :  expr0 (bracket|dot|slash|op)+
       |  exprO string
       |  op e
       | (int|dec) id
@e     :  int|dec|id
       |  string
       |  quant

quant  : (INTEGER|DECIMAL) IDENTIFIER
int    : INTEGER
dec    : DECIMAL
id     : (DOLLAR|DASH)? IDENTIFIER
op     : OPERATOR|DOLLAR|DASH|SLASH
key    :              @op|@id
atom   :      /COLON (@op|IDENTIFIER|COLON)?
prop   : @def /COLON exprO
kv     : @key /COLON exprO
kv2    : @key /COLON (/SPACE exprZ|@indent)
def    : self op? (/DOT @key op?)*
       |          (/DOT @key op?)+
dot    :           /DOT @id
self   :           /DOT /SLASH DOLLAR? IDENTIFIER
slash  :                /SLASH         IDENTIFIER
it     : /IT
@str   : /QUOTE /INDENT (STRING|interp|NEWLINE)* /DEDENT /UNQUOTE
       | /QUOTE         (STRING|interp)*                 /UNQUOTE
@strix : /BACKTICK str
string : strix | str
interp : INTERPOLATE (braces|indent)

braces   :         /LBRACE subexpr  /RBRACE
concur   : /LPAREN /LBRACE subexpr? /RBRACE /RPAREN
parens   : /LPAREN         subexpr          /RPAREN
square   : /LSQUARE        array?           /RSQUARE
@bracket : square|braces|parens|concur
@subexpr : @indent /feeds
         | /SPACE? expr4
kwargs   : (/SPACE kv)+
@array   :             /SPACE? (expr1 (/NEWLINE? /COMMA /SPACE expr1)* /COMMA? /SPACE?)?
         | /INDENT (/IT /SPACE)? expr1 (/NEWLINE? /COMMA /SPACE expr1)* /COMMA? /NEWLINE* /DEDENT /NEWLINE
indent   : /INDENT @expres   /DEDENT
pseudent : /INDENT pseudent? /DEDENT
feeds    : /(NEWLINE|pseudent)+
