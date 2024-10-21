(*
Write an ML function i n t e r p : stm→unit that "interprets" a program
in this language. To write in a "functional" style - without assignment (: =) or
arrays - maintain a list of (variable,integer) pairs, and produce new versions
of this list at each AssignStm. 
*)

type variable = string
type aexp = 
  | Num of int
  | Var of variable
  | Add of aexp * aexp
  | Sub of aexp * aexp
  | Mul of aexp * aexp

type bexp =
  | True
  | False
  | Eq of aexp * aexp
  | Le of aexp * aexp
  | Not of bexp
  | And of bexp * bexp

type stm =
  | AssignStm of variable * aexp
  | SkipStm
  | CompoundStm of stm * stm
  | IfStm of bexp * stm * stm
  | WhileStm of bexp * stm

type environment = (variable * int) list

let rec lookup var env =
  match env with
  | [] -> 0  (* Default value if variable not found *)
  | (v, value) :: rest -> if v = var then value else lookup var rest

let rec eval_aexp a env =
  match a with
  | Num n -> n
  | Var x -> lookup x env
  | Add (a1, a2) -> eval_aexp a1 env + eval_aexp a2 env
  | Sub (a1, a2) -> eval_aexp a1 env - eval_aexp a2 env
  | Mul (a1, a2) -> eval_aexp a1 env * eval_aexp a2 env

let rec eval_bexp b env =
  match b with
  | True -> true
  | False -> false
  | Eq (a1, a2) -> eval_aexp a1 env = eval_aexp a2 env
  | Le (a1, a2) -> eval_aexp a1 env <= eval_aexp a2 env
  | Not b1 -> not (eval_bexp b1 env)
  | And (b1, b2) -> eval_bexp b1 env && eval_bexp b2 env

let rec interp (s: stm) (env: environment) : environment =
  match s with
  | AssignStm (x, a) ->
      let value = eval_aexp a env in
      (x, value) :: List.filter (fun (v, _) -> v <> x) env
  | SkipStm -> env
  | CompoundStm (s1, s2) ->
      let env' = interp s1 env in
      interp s2 env'
  | IfStm (b, s1, s2) ->
      if eval_bexp b env then interp s1 env else interp s2 env
  | WhileStm (b, s) ->
      if eval_bexp b env
      then interp (CompoundStm (s, WhileStm (b, s))) env
      else env

let run_program (s: stm) : unit =
  let final_env = interp s [] in
  List.iter (fun (var, value) -> Printf.printf "%s = %d\n" var value) final_env