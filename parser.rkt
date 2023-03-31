#lang brag

expres : /feeds? expr4
@expr4 : expr3 /(SPACE|feeds)?
@expr3 : apply3|macro3|combo
       | exprZ
@exprZ : comboZ|comboK|comboQ|comboO|combo1
       | macro2|macro1
       | expr2
@expr2 : apply2|applyZ|commaZ|comma
       | exprK
@exprK : commaK|comma1|commaQ|commaO
       | applyL|applyR
       | expr1
@expr1 : apply1
       | exprQ
@exprQ : applyQ
       | spread
       | expr0
@expr0 : apply0
       | bottom
       | atom
       | exprG
@exprG : applyG
       | grouping
       | prop
       | this
       | e

@macro : OP (/SPACE k0 (/SPACE? /COMMA)?)*
@m1    : (expr1|comma1|commaQ|commaO|comma|combo1|comboQ|comboO|combo) /SPACE
@mL    : expr1 /SPACE
@mR    : /SPACE (exprK|macroL|kL)
@mZ    : /SPACE (applyZ|commaZ|kZ)
@m3    : (mR|mZ)? (nkey+|nkey* /feeds expr3)
macro1 : m1? @macroR | m1 macro
macroL : mL? @macroR | mL macro
macroR :     macro mR
macroZ : mL? macro mZ
macro2 : m1? macro mZ
macro3 : m1? macro m3

@comma : (commaO|commaQ|comma1|expr1) /SPACE? /COMMA
@combo : (comboO|comboQ|combo1)       /SPACE? /COMMA
       | exprZ                        /NEWLINE /COMMA

comboZ : (combo|comboO|comboQ) /SPACE (applyZ|kZ)
comboK : (combo|comboO|comboQ) /SPACE (applyL|applyR|kL|kR)
combo1 : (combo|comboO|comboQ) /SPACE expr1
comboQ : (combo|comboO)       (/SPACE k0)+
comboO :  combo               (prop|op|grouping)+

commaZ : (comma|commaO|commaQ) /SPACE (applyZ|kZ)
commaK : (comma|commaO|commaQ) /SPACE (applyL|applyR|kL|kR)
comma1 : (comma|commaO|commaQ) /SPACE expr1
commaQ : (comma|commaO)       (/SPACE k0)+
commaO :  comma               (prop|op|grouping)+

apply3 : expr2 /feeds expr3
apply2 : expr2 nkey+
       | (expr1|applyR|commaQ|commaO|comboQ|comboO|comma|combo) block
applyZ : exprQ /SPACE (applyZ|kZ)
applyL : exprQ /SPACE (applyL|kL)
applyR : exprQ /SPACE (applyR|kR)
apply1 : exprQ /SPACE expr1
applyQ : expr0 (/SPACE k0)+
apply0 : (applyG|grouping) e
       | (exprG|op) (prop|op)+
applyG : (expr0|op) grouping+
apply  : (times|id|op) (int|dec|id|times+)
parse  : (times|id|op|apply) string
times  : (int|dec) id
@e     : times|parse|apply|string|int|dec|id

op     : OP
id     : ID
idx    : ID (/DOT ID)*
int    : INTEGER
dec    : DECIMAL
@k0    : key /COLON expr0
@kL    : key /COLON /SPACE (applyL|macroL|expr1)
@kR    : key /COLON /SPACE (applyR|macroR)
@kZ    : key /COLON /SPACE (applyZ|macroZ)
       | key /COLON dent
@kx    : kZ | k0 (/SPACE k0)*
       | kL | kR block? 
nkey   : /feeds kx
key    : idx | (OP|ID|INTEGER|DECIMAL)+
atom   : (COLON @key?)? COLON @idx?
prop   : /DOT (OP|OP? @id|grouping)
this   : /THIS
feeds  : /NEWLINE+
string : /QUOTE /INDENT (STRING|interp|NEWLINE)* /DEDENT /UNQUOTE
       | /QUOTE         (STRING|interp)*                 /UNQUOTE
interp : INTERPOLATE (brace|dent)

@grouping : @paren|bracket|brace|generator
paren     : /LPAREN (grouped|k0|key /COLON)? /RPAREN
bracket   : /LBRACK  grouped?                /RBRACK
brace     : /LBRACE  grouped?                /RBRACE
bottom    : /LBRACE /SPREAD                      /RBRACE
generator : /LBRACE /SPREAD /SPACE expr4 /SPACE? /RBRACE
          | /LBRACE /SPREAD (id|@dent /feeds)    /RBRACE
@grouped  : /SPACE? expr4 /SPACE?
          | @dent /feeds
          | OP
spread    : /SPREAD (id|grouped)

@block   : keyblock | dent
keyblock : /INDENT kx nkey* /feeds? /DEDENT
dent     : /INDENT /feeds? expr4    /DEDENT
