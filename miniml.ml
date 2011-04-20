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
  | Mul of exp * exp
  | Sub of exp * exp
  | Pow of exp * exp
  | Rec of string * (string list * exp) * exp

let v = L.Lam ("_", L.Var "_")
let t = L.Lam ("_t", L.Lam ("_f", L.App (L.Var "_t", v)))
let f = L.Lam ("_t", L.Lam ("_f", L.App (L.Var "_f", v)))

let rec compile = function
  | Let (x, y, z) ->
      compile (App (Lam ([x], z), [y]))
  | App (f, []) ->
      L.App (compile f, v)
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
      t
      (* L.Lam ("_t", L.Lam ("_", L.App (L.Var "_t", v))) *)
  | False ->
      f
      (* L.Lam ("_", L.Lam ("_f", L.App (L.Var "_f", v))) *)
  | If (x, y, z) ->
      L.App (L.App (compile x, L.Lam ("_", compile y)), L.Lam ("_", compile z))
  | Int 0 ->
      L.Lam ("_f", L.Lam ("_z", L.Var "_z"))
  | Int 1 ->
      L.Lam ("_f", L.Lam ("_z", L.App (L.Var "_f", L.Var "_z")))
  | Int n ->
      L.Lam ("_f", L.Lam ("_z",
        L.App (L.App (compile (Int (n-1)), L.Var "_f"),
          L.App (L.Var "_f", L.Var "_z"))))
      (* L.Lam ("_f", L.Lam ("_z",
        L.App (L.Var "_f", L.App (L.App (compile (Int (n-1)), L.Var "_f"),
          L.Var "_z")))) *)
  | IsZero (x) ->
      L.App (L.App (compile x, L.Lam ("_", f)), t)
  | Add (x, y) ->
      L.Lam ("_f", L.Lam ("_z",
        L.App (L.App (compile x, L.Var "_f"),
          L.App (L.App (compile y, L.Var "_f"), L.Var "_z"))))
  | Mul (x, y) ->
      L.Lam ("_f",
        L.App (compile x, L.App (compile y, L.Var "_f")))
  | Sub (x, y) ->
      let pred =
        Lam (["_n"; "_f"; "_x"],
          App (Var "_n", [Lam (["_g"; "_h"], App (Var "_g", [Var "_f"]));
            Lam (["_u"], Var "_x"); Lam (["_u"], Var "_u")])) in
      L.App (L.App (compile x, compile pred), compile y)
  | Pow (x, y) ->
      L.App (compile x, compile y)
  | Rec (x, (xs, y), z) ->
      let ycomb =
        L.Lam ("_f",
          L.App (L.Lam ("_x", L.App (L.Var "_f", L.App (L.Var "_x", L.Var "_x"))),
            L.Lam ("_x", L.App (L.Var "_f", L.App (L.Var "_x", L.Var "_x"))))) in
      L.App (L.Lam (x, compile z),
        L.App (ycomb, L.Lam (x, compile (Lam (xs, y)))))
