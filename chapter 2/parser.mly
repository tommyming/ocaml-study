%{
open Ast  (* Import your AST representation *)
%}

%start xml
%type <Ast.xml> xml

%% 

xml:
  | element { $1 }

element:
  | LANGLE IDENT attributes RANGLE content RANGLE SLASH IDENT RANGLE { Element($2, $3, $5) }
  
attributes:
  | /* empty */ { [] }
  | attributes IDENT EQUALS QUOTE IDENT QUOTE { ($2, $5) :: $1 }

content:
  | /* empty */ { [] }
  | content IDENT { $1 @ [$2] }