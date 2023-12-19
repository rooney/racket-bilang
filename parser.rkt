#lang brag

expres : /feeds? expr4
@expr4 : expr3 /feeds? /SPACE?
@expr3 : apply3
       | exprB
@exprB : break2|break1|breakO|break
       | expr2
@expr2 : apply2
       | macro2
       | comma2|comma1|commaO|comma
       | expr1
@expr1 : apply1
       | exprO
@exprO : applyO
       | expr0
@expr0 : apply0
       | dot|prop|slash|self
       | bracket
       | it
       | e
@exprl : expr1|comma1|commaO|comma
@exprL : exprl|break1|breakO|break
@exprI : exprl|commaI|macroI|applyI

@macro : LPAREN /RPAREN
       | LBRACE /RBRACE
       | DOT /COLON
       | def /COLON
       | bublet
       | op
macroI :                 macro kwargs
       |  exprl /SPACE   macro kwargs?
       | (exprl /SPACE)? macro kwargs? spaceI
macro2 :                 macro kwargs
       |  exprL /SPACE   macro kwargs?
       | (exprL /SPACE)? macro kwargs? space2

comma  : (commaO|comma1|expr1) /SPACE? /COMMA
break  : (breakO|break1)       /SPACE? /COMMA
       |                exprB /NEWLINE /COMMA
breakO :  @break (bracket|dot|slash|op)+
commaO :  @comma (bracket|dot|slash|op)+
comma1 : (@comma|commaO) kwargs? space1
commaI : (@comma|commaO) kwargs? spaceI
comma2 : (@comma|commaO) kwargs? space2
break1 : (@break|breakO) kwargs? space1 | exprB /feeds kv kwargs? space1?
break2 : (@break|breakO) kwargs? space2 | exprB /feeds kv kwargs? space2
                                        | exprB /feeds kv2
apply3 : (exprB|macro) (/SPACE key /COLON)? /feeds expr3
apply2 :  exprO kwargs? space2
apply1 :  exprO kwargs? space1
applyI :  exprO kwargs? spaceI
applyO :  atom ((int|dec) id|e)? bracket*
apply0 :  expr0 (dot|op|slash|bracket)+
       |  exprO string
       |  op e
       | (int|dec|id) id
@e     :  int|dec|id
       |  string

int    :  INTEGER
dec    :  DECIMAL
op     :  DOLLAR|DASH|SLASH|OPERATOR
id     : (DOLLAR|DASH)? IDENTIFIER
key    : @op|@id
atom   :      /COLON (@op|IDENTIFIER|COLON)?
prop   : @def /COLON        exprO
kv     : @key /COLON        exprO
kvI    : @key /COLON /SPACE exprI
kv2    : @key /COLON /SPACE expr2
       | @key /COLON @indent
bublet : (key /COLON)? BUBLET
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
                     
parens   : /LPAREN  subexpr /RPAREN
braces   : /LBRACE  subexpr /RBRACE
square   : /LBRACKET array? /RBRACKET
@bracket : braces|parens|square
@subexpr :  /SPACE? expr4                | indent /feeds
@space2  :  /SPACE          (kv2|apply2) | indent
@spaceI  :  /SPACE (expr1|kv|kvI|applyI)
@space1  :  /SPACE (expr1|kv)
kwargs   : (/SPACE        kv)+
@array   :  /SPACE  expr1 (/COMMA /SPACE expr1)* (/NEWLINE /SPACE? (/COMMA /SPACE expr1)+)* /SPACE
         |          exprO        (/SPACE exprO)* (/NEWLINE                (/SPACE exprO)+)*
         | /INDENT        /COMMA (/SPACE exprI|@indent)  (/NEWLINE /COMMA (/SPACE exprI|@indent))* /DEDENT /NEWLINE
indent   : /INDENT @expres   /DEDENT
pseudent : /INDENT pseudent? /DEDENT
feeds    : /(NEWLINE|pseudent)+
