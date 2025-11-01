open Ast

(** [parse s] parses [s] into an AST. *)
let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

(** Error messages required by the tests *)
let unbound_var_err = "Unbound variable"
let bop_err = "Operator and operand type mismatch"
let if_guard_err = "Guard of if must be boolean"

(** Values are only ints and bools. *)
let is_value = function
  | Int _ | Bool _ -> true
  | _ -> false

(** Substitute value [v] for free occurrences of variable [x] in [e]. *)
let rec subst (e : expr) (v : expr) (x : string) : expr =
  match e with
  | Int _ | Bool _ -> e
  | Var y -> if y = x then v else e
  | Binop (b, e1, e2) -> Binop (b, subst e1 v x, subst e2 v x)
  | If (e1, e2, e3) -> If (subst e1 v x, subst e2 v x, subst e3 v x)
  | Let (y, e1, e2) ->
      let e1' = subst e1 v x in
      if y = x then
        (* x is shadowed here; don't substitute into e2 *)
        Let (y, e1', e2)
      else
        Let (y, e1', subst e2 v x)

(** One small-step reduction. *)
let rec step (e : expr) : expr =
  match e with
  | Int _ | Bool _ -> failwith "precondition violated"
  | Var _ -> failwith unbound_var_err

  | Binop (bop, e1, e2) ->
      if is_value e1 then
        if is_value e2 then step_bop bop e1 e2
        else Binop (bop, e1, step e2)
      else Binop (bop, step e1, e2)

  | Let (x, e1, e2) when is_value e1 -> subst e2 e1 x
  | Let (x, e1, e2) -> Let (x, step e1, e2)

  | If (e1, e2, e3) ->
      match e1 with
      | Bool true -> e2
      | Bool false -> e3
      | Int _ -> failwith if_guard_err
      | _ -> If (step e1, e2, e3)

and step_bop bop e1 e2 =
  match bop, e1, e2 with
  | Add,  Int a, Int b -> Int (a + b)
  | Mult, Int a, Int b -> Int (a * b)
  | Leq,  Int a, Int b -> Bool (a <= b)
  | _ -> failwith bop_err

(** Evaluate to a value by stepping until done. *)
let rec eval (e : expr) : expr =
  if is_value e then e else e |> step |> eval

(** Top-level interpreter: parse, eval, return value. *)
let interp (s : string) : expr =
  s |> parse |> eval
