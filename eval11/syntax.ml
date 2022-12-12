(* op : binary operator *)
type op = Plus | Minus | Times | Divide | Equal | Less

(* op_to_string : op -> string *)
let op_to_string op = match op with
    Plus -> " + "
  | Minus -> " - "
  | Times -> " * "
  | Divide -> " / "
  | Equal -> " = "
  | Less -> " < "

(* e : syntax *)
type e = Num of int
       | Bool of bool
       | Var of string
       | Fun of string * e
       | App of e * e
       | Op of e * op * e
       | If of e * e * e
       | TryWith of e * string * string * e
       | Rec of string * string * e * e
       | CallD of e
       | CallS of e

(* to_string : e -> string *)
let rec to_string exp = match exp with
    Num n -> string_of_int n
  | Bool b -> if b then "true" else "false"
  | Var x -> x
  | Fun (x, e) -> "(fun " ^ x ^ " -> " ^ to_string e ^ ")"
  | App (e0, e1) -> "(" ^ to_string e0 ^ " " ^ to_string e1 ^ ")"
  | Op (e0, op, e1) -> "(" ^ to_string e0 ^ op_to_string op ^ to_string e1 ^ ")"
  | If (e0, e1, e2) ->
    "(if " ^ to_string e0 ^
    " then " ^ to_string e1 ^ " else " ^ to_string e2 ^ ")"
  | TryWith (e0, x, y, e1) ->
    "(try " ^ to_string e0 ^ " with (" ^ x ^ ", " ^ y ^ ") -> "
    ^ to_string e1 ^ ")"
  | Rec (g, x, e0, e1) ->
    "(let rec " ^ g ^ " " ^ x ^ " = "
    ^ to_string e0 ^ " in " ^ to_string e1 ^ ")"
  | CallD e -> "call_d(" ^ to_string e ^ ")"
  | CallS e -> "call_s(" ^ to_string e ^ ")"

(* print : e -> unit *)
let print exp =
  let str = to_string exp in
  print_string str
