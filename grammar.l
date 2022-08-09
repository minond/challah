top = module
    | import
    | expr
    | val
    | mut
    | def
    ;

module = "module" id [ ids ] ;
import = "import" id [ ids ] qualified ;
ids = "(" id qualified { "," id qualified } ")" ;
qualified = [ "as" id ] ;

expr = prefix-op primary
     | primary infix-op primary
     | primary
     ;

primary = "(" expr ")"
        | block
        | app
        | id
        | num
        | str
        ;

block = "{" { expr } "}" ;

app = primary app-args ;
app-args = "(" [ expr { "," expr } ] ")" ;

val = val-dec "=" expr ;
val-dec = "val" id [ ":" type ] ;
mut = mut-dec "=" expr ;
mut-dec = "mut" id [ ":" type ] ;

def = def-dec "=" expr ;
def-dec = "def" id def-params ;
def-params = "(" [ def-arg { "," def-arg } ] ")" ;
def-arg = id ":" type ;

type = id
     | id type-params
     ;
type-params = "[" [ type { "," type } ] "]" ;

id = letter , { alphanumeric } ;
letter = ? all letters ? ;
alphanumeric = ? all alphanumeric characters ? ;

num = [ "-" ] , { digit } [ "." { digit } ] ;
digit = ? all digits ? ;

str = "\"" str-char "\"" ;
str-char = ? any character except double quote ? ;

prefix-op = "!" ;
infix-op = "-" | "+" | "*" | "/" | "%" ;
