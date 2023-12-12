#lang brag

expres : /feeds? expr4
@expr4 : expr3 /feeds? /SPACE?
@expr3 : apply3
       | exprB
@exprB : break2|break1|breakO|break
       | expr2
@expr2 : apply2
       | comma2|comma1|commaO|comma
       | macroX
       | expr1
@expr1 : apply1
       | exprO
@exprO : applyO
       | atom
       | expr0
@expr0 : apply0
       | dot|prop|slash|self
       | bracket
       | it
       | e
@exprL : expr1
       | comma1|commaO|comma
       | break1|breakO|break

@macro : LPAREN RPAREN
       | LBRACE RBRACE
       | DOT COLON
       | def COLON
       | BUBLET
       | op
macroX :                 macro kwargs
       |  exprL /SPACE   macro kwargs?
       | (exprL /SPACE)? macro kwargs? /SPACE (expr2|kv2)
       | (exprL /SPACE)? macro kwargs? indent

comma  : (commaO|comma1|apply1) /SPACE?  /COMMA
break  : (breakO|break1)        /SPACE?  /COMMA
       |                 exprB  /NEWLINE /COMMA
breakO :  @break (bracket|dot|slash|op)+
commaO :  @comma (bracket|dot|slash|op)+
comma1 : (@comma|commaO) kwargs? /SPACE (expr1|kv)
comma2 : (@comma|commaO) kwargs? /SPACE (expr2|kv2)
break1 : (@break|breakO) kwargs? /SPACE (expr1|kv)
break2 : (@break|breakO) kwargs? /SPACE (expr2|kv2)

apply3 : (exprB|macro) (/SPACE key /COLON)? /feeds expr3
apply2 :  exprO kwargs? (/SPACE (expr2|kv2)|@indent)
apply1 :  exprO kwargs? /SPACE (expr1|kv)
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
prop   : @def /COLON        exprO
kv     : @key /COLON        exprO
kv2    : @key /COLON /SPACE expr2
       | @key /COLON @indent
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

braces   : /LBRACE  subexpr /RBRACE
parens   : /LPAREN  subexpr /RPAREN
square   : /LSQUARE array?  /RSQUARE
@bracket : square|braces|parens
@subexpr : @indent /feeds
         | /SPACE? expr4
kwargs   : (/SPACE kv)+
@array   :              /SPACE? (expr1 (/NEWLINE? /COMMA /SPACE expr1)* /COMMA? /SPACE?)?
         | /INDENT (/IT /SPACE)? expr1 (/NEWLINE? /COMMA /SPACE expr1)* /COMMA? /NEWLINE* /DEDENT /NEWLINE
indent   : /INDENT @expres   /DEDENT
pseudent : /INDENT pseudent? /DEDENT
feeds    : /(NEWLINE|pseudent)+
