#lang brag

expres : /feeds? expr4
@expr4 : expr3 /(SPACE|feeds)?
@expr3 : apply3|macro3|split
       | expr2
@expr2 : apply2|macro2
       | macroL|macro1|macro0
       | exprL
@exprL : split1|split0
       | exprk
@exprk : comma1|comma0|comma
       | expr1
@expr1 : apply1
       | exprQ
@exprQ : key /COLON
       | kv
       | xtend
       | exprO
@exprO : applyO
       | bottom
       | atom
       | exprG
@exprG : applyG
       | grouping
       | expr0
@expr0 : apply0
       | this
       | dot
       | e

@m1    : (comma|comma0|comma1|expr1) /SPACE
@mL    : (split|split0|split1)       /SPACE
@mR    :                             /SPACE (exprk|macro1|macro0)
macro0 :          op
macro1 :          op mR
       |     m1   op mR?
macroL :  mL      op mR?
macro2 : (mL|m1)? op mR? indent
macro3 : (mL|m1)? op mR? indent? /feeds expr3

@comma : (comma0|comma1|expr1) /SPACE? /COMMA
@split : (split0|split1)       /SPACE? /COMMA
       |                expr2  /NEWLINE /COMMA

comma1 : (comma|comma0) /SPACE expr1
split1 : (split|split0) /SPACE expr1

comma0 : comma (dot|op|grouping)+
split0 : split (dot|op|grouping)+

apply3 : (exprL|apply2) /feeds expr3
apply2 :  exprL indent
apply1 :  exprQ /SPACE expr1
applyO : (exprG|op) (dot|op)+
applyG : (exprO|op) grouping+
apply0 : (expr0|op) e
@e     : id
       | int|dec
       | string

op     : OP
id     : ID
idx    : ID (/DOT ID)*
int    : INTEGER
dec    : DECIMAL
key    : idx | (OP|ID|INTEGER|DECIMAL)+
kv     : key /COLON exprO
atom   : (COLON key?)? COLON idx?
dot    : /DOT (OP|OP? @id|grouping)
this   : /THIS
feeds  : /(NEWLINE|pseudent)+
string : /QUOTE /INDENT (STRING|interp|NEWLINE)* /DEDENT /UNQUOTE
       | /QUOTE         (STRING|interp)*                 /UNQUOTE
interp : INTERPOLATE (brace|indent)

@grouping : @paren|bracket|brace|generator
paren     : /LPAREN grouped? /RPAREN
bracket   : /LBRACK grouped? /RBRACK
brace     : /LBRACE grouped? /RBRACE
bottom    : /LBRACE /DOTDOT  /RBRACE
generator : /LBRACE /DOTDOT /SPACE expr4 /RBRACE
          | /LBRACE /DOTDOT (id|@indent) /RBRACE
@grouped  : /SPACE? expr4 /SPACE?
          | @indent /feeds
          | OP
xtend     : /DOTDOT (id|grouped)
indent    : /INDENT expres /DEDENT
pseudent  : /INDENT pseudent? /DEDENT
