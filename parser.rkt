#lang brag

expres : /feeds? expr4
@expr4 : expr3 /feeds? /SPACE?
@expr3 : apply3
       | exprB
@exprB : break2|break1|breakO|break
       | expr2
@expr2 : apply2
       | expl2
       | comma2|comma1|commaO|comma
       | expr1
@expr1 : apply1
       | exprO
@exprO : applyO
       | ion|anion
       | expr0
@expr0 : apply0
       | dot|slash
       | it
       | e
@exprl : expr1|comma1|commaO|comma
@exprL : exprl|break1|breakO|break
@exprI : exprl|commaI|applyI|explI

explI  :                 radic kwargs
       |  exprl /SPACE   radic kwargs?
       | (exprl /SPACE)? radic kwargs? (spaceI|/SPACE atom)
       | (exprl /SPACE)? radic kwargs? 
expl2  :                 radic kwargs
       |  exprL /SPACE   radic kwargs?
       | (exprL /SPACE)? radic kwargs? (space2|/SPACE (atom|expr2))

comma  : (commaO|comma1|expr1) /SPACE? /COMMA
comma_ : (commaO|comma1|expr1) /SPACE  /COMMA
break  : (breakO|break1)       /SPACE? /COMMA | exprB /NEWLINE /COMMA
break_ : (breakO|break1)       /SPACE  /COMMA | exprB /NEWLINE /COMMA
breakO :  @break (group|dot|slash|radic)+
commaO :  @comma (group|dot|slash|radic)+
comma2 : (@comma|commaO) kwargs? space2 | @comma_  kv2
commaI : (@comma|commaO) kwargs? spaceI | @comma_  kvI
comma1 : (@comma|commaO) kwargs? space1 | @comma_ (kv|exprO)
break1 : (@break|breakO) kwargs? space1 | @break_ (kv|exprO)
break2 : (@break|breakO) kwargs? space2 | @break_  kv2 
       |                              exprB /feeds kv2
apply3 : (exprB|radic) (/SPACE key /COLON)? /feeds expr3
apply2 :  exprO kwargs? space2
apply1 :  exprO kwargs? space1
applyI :  exprO kwargs? spaceI
applyO :  anion (id|string|group) group*
       |    ion e group*
apply0 :  expr0 (dot|slash|radic|group)+
       |  exprO string
       |  radic e
       |  (int|dec) id
@e     :   int|dec|id
       |   string
       |   group

int    :  INTEGER
dec    :  DECIMAL
radic  :  atom
       |  DOT @key? COLON
       |  LPAREN RPAREN
       |  LBRACE RBRACE
       | (DOLLAR|DASH|SLASH|RADICAL|QUESTION-MARK) PRIME?
nuclei : (DOLLAR|DASH)? IDENTIFIER (QUESTION-MARK|DASH)? PRIME?
id     :  IDENTIFIER PRIME?
ion    :      /COLON (COLON|@key)
anion  :      /COLON
kv     : @key /COLON        exprO
kvI    : @key /COLON /SPACE exprI
kv2    : @key /COLON /SPACE expr2
       | @key /COLON @indent
key    :  @radic | @id
slash  :    /SLASH @id
dot    :      /DOT @id
it     : /IT
str    : /QUOTE /INDENT (STRING|interp|NEWLINE)* /DEDENT /UNQUOTE
       | /QUOTE         (STRING|interp)*                 /UNQUOTE
bacstr : /BACKTICK @str
string : @bacstr | @str
interp : INTERPOLATE (braces|indent)

atom     : /LPAREN /PROTON ((@nuclei? COLON? /SPACE COLON)? @nuclei)? /RPAREN
         | /LPAREN /PROTON            COLON                           /RPAREN
parens   : /LPAREN  (subexpr|radic) /RPAREN
braces   : /LBRACE   subexpr        /RBRACE
brackets : /LBRACKET array?         /RBRACKET
@group   :  parens | braces | brackets
@subexpr :  /SPACE? expr4                | indent /feeds
@space2  :  /SPACE          (kv2|apply2) | indent
@spaceI  :  /SPACE (expr1|kv|kvI|applyI)
@space1  :  /SPACE (expr1|kv)
kwargs   : (/SPACE        kv)+
@array   :  /SPACE  exprI                        (/NEWLINE /SPACE? /COMMA  /SPACE exprI)*   /SPACE
         |          exprO (/SPACE exprO)*        (/NEWLINE                (/SPACE exprO)+)*
         | /INDENT /COMMA (/SPACE exprI|@indent) (/NEWLINE /COMMA (@indent|/SPACE exprI))*  /DEDENT /NEWLINE
indent   : /INDENT @expres   /DEDENT
pseudent : /INDENT pseudent? /DEDENT
feeds    : /(NEWLINE|pseudent)+
