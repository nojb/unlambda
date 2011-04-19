module L = Lambda

type exp =
  | Let of string * exp * exp
  | App of exp * exp list
  | Var of string
  | Lam of string list * exp
  | True
  | False
  | If of exp * exp * exp
  | Int of int
  | IsZero of exp
  | Add of exp * exp
  | Sub of exp * exp
  | Pow of exp * exp
  | PrintInt of exp
  | Rec of string * (string list * exp) * exp
  | Seq of exp * exp

let t = L.Lam ("_t", L.Lam ("_f", L.App (L.Var "_t", L.V)))
let f = L.Lam ("_t", L.Lam ("_f", L.App (L.Var "_f", L.V)))

let rec compile = function
  | Let (x, y, z) ->
      compile (App (Lam ([x], y), [z]))
  | App (f, []) ->
      L.App (compile f, L.V)
  | App (f, [x]) ->
      L.App (compile f, compile x)
  | App (f, x :: xs) ->
      compile (App (App (f, [x]), xs))
  | Var x ->
      L.Var x
  | Lam ([], y) ->
      L.Lam ("_", compile y)
  | Lam ([x], y) ->
      L.Lam (x, compile y)
  | Lam (x :: xs, y) ->
      compile (Lam ([x], Lam (xs, y)))
  | True ->
      L.Lam ("_t", L.Lam ("_", L.App (L.Var "_t", L.V)))
  | False ->
      L.Lam ("_", L.Lam ("_f", L.App (L.Var "_f", L.V)))
  | If (x, y, z) ->
      L.App (L.App (compile x, L.Lam ("_", compile y)), L.Lam ("_", compile z))
  | Int 0 ->
      L.Lam ("_f", L.Lam ("_z", L.Var "_z"))
  | Int 1 ->
      L.Lam ("_f", L.Lam ("_z", L.App (L.Var "_f", L.Var "_z")))
  | Int n ->
      L.Lam ("_f", L.Lam ("_z",
        L.App (L.Var "_f", L.App (L.App (compile (Int (n-1)), L.Var "_f"),
          L.Var "_z"))))
  | IsZero (x) ->
      L.App (L.App (compile x, L.Lam ("_", t)), f)
  | Add (x, y) ->
      L.Lam ("_f", L.Lam ("_z",
        L.App (L.App (compile x, L.Var "_f"),
          L.App (L.App (compile y, L.Var "_f"), L.Var "_z"))))
  | Pow (x, y) ->
      L.Lam ("_f", L.Lam ("_z",
        L.App (L.App (compile x, L.App (compile y, L.Var "_f")),
          L.Var "_z")))
  | PrintInt (x) ->
      L.App (L.Lam ("_", L.App (L.Dot '\n', L.V)),
        L.App (L.App (compile x, L.Dot ('*')), L.V))
  | Rec (x, (xs, y), z) ->
      let ycomb =
        L.Lam ("_f",
          L.App (L.Lam ("_x", L.App (L.Var "_f", L.App (L.Var "_x", L.Var "_x"))),
            L.Lam ("_x", L.App (L.Var "_f", L.App (L.Var "_x", L.Var "_x"))))) in
      L.App (L.Lam (x, compile z),
        L.App (ycomb, L.Lam (x, compile (Lam (xs, y)))))
  | Seq (x, y) ->
      L.App (L.Lam ("_", compile y), compile x)
