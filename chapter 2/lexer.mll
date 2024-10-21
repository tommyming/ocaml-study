{
open Parser  (* Import the parser module for token types *)
exception SyntaxError of string
}

rule read = parse
  | '<' { LANGLE }          (* Start tag *)
  | '>' { RANGLE }         (* End tag *)
  | "</" { SLASH }         (* Closing tag *)
  | '=' { EQUALS }         (* Attribute assignment *)
  | '"' { QUOTE }          (* Start of attribute value *)
  | [^<>"=]+ { IDENT $0 }  (* Identifiers or text content *)
  | eof { EOF }            (* End of file *)
  | _ { raise (SyntaxError "Unexpected character") }

{
let () = print_endline "Lexer generated."
}