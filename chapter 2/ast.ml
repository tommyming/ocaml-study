(*
XML is mainly on about the with strings and JSON like format.
TODO: check the proper AST Tree.
*)

type attribute = {
  name: string;
  value: string;
}

type xml_node =
  | Element of {
      name: string;
      attributes: attribute list;
      children: xml_node list;
    }
  | Text of string
  | Comment of string
  | CDATA of string