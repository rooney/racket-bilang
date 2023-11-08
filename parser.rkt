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
       | atom
       | expr0
@expr0 : apply0
       | bracket
       | dot
       | prop
       | slash
       | self
       | it
       | e

@mL    : (exprL|break) /SPACE
@mR    :               /SPACE exprk
macro  : op|bubble|def COLON|DOT COLON|LPAREN RPAREN|LBRACE RBRACE
macro1 :     @macro kwargs
       | mL  @macro kwargs?
       | mL? @macro kwargs? mR
macro2 : mL? @macro kwargs? mR? indent
macro3 : mL? @macro kwargs? mR? indent? /feeds expr3
macroB :     @macro kwargs? mR? indent

comma  : (commaO|comma1|apply1) /SPACE?  /COMMA
break  : (breakO|break1)        /SPACE?  /COMMA
       |                expr2   /NEWLINE /COMMA
break2 : (apply2|macro2)        /NEWLINE /COMMA

break1 : (break|breakO) /SPACE expr1
comma1 : (comma|commaO) /SPACE expr1

breakO : break (bracket|dot|slash|op)+
commaO : comma (bracket|dot|slash|op)+

apply3 : (exprL|apply2) /feeds expr3
apply2 : (exprL|break2) kwargs? (/SPACE kv2|indent)
apply1 :  exprO         kwargs? /SPACE (kv|expr1)
applyO :  atom   bracket+
apply0 :  expr0 (bracket|dot|slash|op)+
       |  exprO string
       |  op e
       | (int|dec) id
@e     :  int|dec|id
       |  string
       |  quant

quant  : (INT|DEC) ID
int    : INT
dec    : DEC
id     : (DOLLAR|DASH)? ID
op     : OP|DOLLAR|DASH|SLASH
key    :              @op|@id
atom   :      /COLON (@op|ID|COLON)?
bubble : (key /COLON)? /LPAREN /COLON @key dot* op? /RPAREN
prop   : @def /COLON exprO
kv     : @key /COLON exprO
kv2    : @key /COLON (/SPACE expr2|@indent)
def    : (@self|@self? @dot+) @op?
dot    : /DOT @key
self   : /DOT /SLASH DOLLAR? ID
slash  : /SLASH ID
it     : /IT
@str   : /QUOTE /INDENT (STRING|interp|NEWLINE)* /DEDENT /UNQUOTE
       | /QUOTE         (STRING|interp)*                 /UNQUOTE
@strix : /BACKTICK str
string : strix | str
interp : INTERPOLATE (braces|indent)

braces   :         /LBRACE subexpr  /RBRACE
concur   : /LPAREN /LBRACE subexpr? /RBRACE /RPAREN
parens   : /LPAREN         subexp1          /RPAREN
square   : /LSQUARE        array?           /RSQUARE
@bracket : square|braces|parens|concur
@subexpr : @indent /feeds | /SPACE? expr4
@subexp1 : @indent /feeds | /SPACE? (apply3|macro3
                                    |apply2|macro2|macro1
                                    |break1|breakO|break
                                    |comma1|commaO|comma
                                    |apply1) /feeds? /SPACE?
kwargs   : (/SPACE kv)+
@array   :              /SPACE? (expr1 (/NEWLINE? /COMMA /SPACE expr1)* /COMMA? /SPACE?)?
         | /INDENT (/IT /SPACE)? expr1 (/NEWLINE? /COMMA /SPACE expr1)* /COMMA? /NEWLINE* /DEDENT /NEWLINE
indent   : /INDENT expres    /DEDENT
pseudent : /INDENT pseudent? /DEDENT
feeds    : /(NEWLINE|pseudent)+
