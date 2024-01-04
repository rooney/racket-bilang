#lang brag

expres : /feeds? expr4
@expr4 : expr3 /feeds? /SPACE?
@expr3 : apply3
       | exprB
@exprB : break2|break1|breakO|break
       | expr2
@expr2 : apply2
       | explo2
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
@exprI : exprl|commaI|exploI|applyI

exploI :                 radical kwargs
       |  exprl /SPACE   radical kwargs?
       | (exprl /SPACE)? radical kwargs? (spaceI|/SPACE atom)
       | (exprl /SPACE)? radical kwargs? 
explo2 :                 radical kwargs
       |  exprL /SPACE   radical kwargs?
       | (exprL /SPACE)? radical kwargs? (space2|/SPACE (atom|expr2))

comma  : (commaO|comma1|expr1) /SPACE? /COMMA
commas : (commaO|comma1|expr1) /SPACE  /COMMA
break  : (breakO|break1)       /SPACE? /COMMA | exprB /NEWLINE /COMMA
breaks : (breakO|break1)       /SPACE  /COMMA | exprB /NEWLINE /COMMA
breakO :  @break (group|dot|slash|radical)+
commaO :  @comma (group|dot|slash|radical)+
comma2 : (@comma|commaO) kwargs? space2 | @commas  kv2
commaI : (@comma|commaO) kwargs? spaceI | @commas  kvI
comma1 : (@comma|commaO) kwargs? space1 | @commas (kv|exprO)
break1 : (@break|breakO) kwargs? space1 | @breaks (kv|exprO)
break2 : (@break|breakO) kwargs? space2 | @breaks  kv2 
       |                              exprB /feeds kv2
apply3 : (exprB|radical) (/SPACE key /COLON)? /feeds expr3
apply2 :  exprO kwargs? space2
apply1 :  exprO kwargs? space1
applyI :  exprO kwargs? spaceI
applyO :  anion (id|string|group) group*
       |    ion e group*
apply0 :  expr0 (dot|slash|radical|group)+
       |  exprO string
       |  radical e
       |  (int|dec) id
@e     :   int|dec|id
       |   string
       |   group

int     :  INTEGER
dec     :  DECIMAL
radical :  atom
        |  DOT @key? COLON
        |  LPAREN RPAREN
        |  LBRACE RBRACE
        | (DOLLAR|DASH|SLASH|RADICAL|QUESTION-MARK) PRIME?
nucleon : (DOLLAR|DASH)? IDENTIFIER (QUESTION-MARK|DASH)? PRIME?
id      :  IDENTIFIER PRIME?
ion     :      /COLON (COLON|@key)
anion   :      /COLON
kv      : @key /COLON        exprO
kvI     : @key /COLON /SPACE exprI
kv2     : @key /COLON /SPACE expr2
        | @key /COLON @indent
key     :  @radical | @id
slash   :      /SLASH @id
dot     :        /DOT @id
it      : /IT
str     : /QUOTE /INDENT (STRING|interpt|NEWLINE)* /DEDENT /UNQUOTE
        | /QUOTE         (STRING|interpt)*                 /UNQUOTE
backstr : /BACKTICK @str
string  : @backstr | @str
interpt : INTERPOLATE (braces|indent)

atom     : /LPAREN /PROTON ((@nucleon? COLON? /SPACE COLON)? @nucleon)? /RPAREN
         | /LPAREN /PROTON             COLON                            /RPAREN
parens   : /LPAREN  (subexpr|radical)         /RPAREN
braces   : /LBRACE   subexpr                  /RBRACE
brackets : /LBRACKET array?                   /RBRACKET
@group   :  parens | braces | brackets
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
