top = expr
    ;

expr = prefix-op primary
     | primary infix-op primary
     | primary
     ;

primary = "(" expr ")"
        | block
        | val
        | mut
        | def
        | id
        | num
        | str
        ;

block = "{" { expr } "}" ;

val = val-dec "=" expr ;
val-dec = "val" id [ ":" type ] ;
mut = mut-dec "=" expr ;
mut-dec = "mut" id [ ":" type ] ;

def = def-dec "=" expr ;
def-dec = "def" id def-args ;
def-args = "(" [ def-arg { "," def-arg } ] ")" ;
def-arg = id ":" type ;

type = id
     | id type-args
     ;
type-args = "[" [type, { "," type }] "]" ;

id = letter , { alphanumeric } ;
letter = ? all letters ? ;
alphanumeric = ? all alphanumeric characters ? ;

num = [ "-" ] , { digit } [ "." { digit } ] ;
digit = ? all digits ? ;

str = "\"" str-char "\"" ;
str-char = ? any character except double quote ? ;

prefix-op = "!" ;
infix-op = "-" | "+" | "*" | "/" | "%" ;
