(*
Write an ML function i n t e r p : stm→unit that "interprets" a program
in this language. To write in a "functional" style - without assignment (: =) or
arrays - maintain a list of (variable,integer) pairs, and produce new versions
of this list at each AssignStm. 
*)

type id = string

type exp = 
  | IdExp of id
  | NumExp of int
  | OpExp of exp * exp
  | BinOp of string

type stm =
  | CompoundStm of stm * stm
  | AssignStm of id * exp
  | PrintStm of exp list

type table = (id * int) list

let rec lookup (x: id) (env: table) : int =
  match env with
  | [] -> failwith ("Unbound variable: " ^ x)
  | (y, v)::rest -> if x = y then v else lookup x rest

let rec interp_exp (e: exp) (env: table) : int =
  match e with
  | NumExp n -> n
  | IdExp x -> lookup x env
  | OpExp(e1, e2) -> 
      let v1 = interp_exp e1 env in
      let v2 = interp_exp e2 env in
      v1 + v2
  | BinOp _ -> failwith "Binary operation not implemented"

let rec interp_stm (s: stm) (env: table) : table =
  match s with
  | CompoundStm(s1, s2) ->
      let env' = interp_stm s1 env in
      interp_stm s2 env'
  | AssignStm(x, e) ->
      let v = interp_exp e env in
      (x, v)::env
  | PrintStm exps ->
      List.iter (fun e -> 
        Printf.printf "%d " (interp_exp e env)
      ) exps;
      Printf.printf "\n";
      env

let interp (s: stm) : unit =
  let _ = interp_stm s [] in
  ()