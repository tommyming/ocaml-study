(* Define the types for the AST representation of XML *)

type attribute = string * string  (* (name, value) *)

type xml_content =
  | Text of string                (* Text content within an element *)
  | Element of xml_element        (* Nested XML element *)

and xml_element = {
  name: string;                  (* Name of the XML element *)
  attributes: attribute list;    (* List of attributes *)
  content: xml_content list;      (* List of content (text or nested elements) *)
}

(* Function to convert XML to a string representation for debugging *)
let rec string_of_xml_content = function
  | Text text -> Printf.sprintf "\"%s\"" text
  | Element elem -> string_of_xml_element elem

and string_of_xml_element elem =
  let attrs = 
    List.map (fun (name, value) -> Printf.sprintf "%s=\"%s\"" name value) elem.attributes
    |> String.concat " " 
  in
  let content = 
    List.map string_of_xml_content elem.content 
    |> String.concat "" 
  in
  Printf.sprintf "<%s %s>%s</%s>" elem.name attrs content elem.name

(* Example usage *)
let example_xml = {
  name = "note";
  attributes = [("to", "Tove"); ("from", "Jani")];
  content = [Text "Don't forget me this weekend!"];
}