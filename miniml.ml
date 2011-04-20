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
  | And of exp * exp
  | Eq of exp * exp
  | Or of exp * exp
  | Cons of exp * exp
  | Nil
  | IsNil of exp
  | Pair of exp * exp
  | Fst of exp
  | Snd of exp
  | Head of exp
  | Tail of exp

let v = L.Lam ("_", L.Var "_")
let t = L.Lam ("_a", L.Lam ("_b", L.App (L.Var "_a", v)))
let f = L.Lam ("_a", L.Lam ("_b", L.App (L.Var "_b", v)))

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
  | False ->
      f
  | If (x, y, z) ->
      L.App (L.App (compile x, L.Lam ("_", compile y)),
        L.Lam ("_", compile z))
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
          App (Var "_n", [Lam (["_g"; "_h"], App (Var "_h",
            [App (Var "_g", [Var "_f"])]));
            Lam (["_u"], Var "_x"); Lam (["_u"], Var "_u")])) in
      L.App (L.App (compile y, compile pred), compile x)
  | Pow (x, y) ->
      L.App (compile y, compile x)
  | Rec (x, (xs, y), z) ->
      let ycomb =
        L.App (L.Lam ("_y",
          L.Lam ("_F", L.App (L.Var "_F", L.Lam ("_x",
            L.App (L.App (L.App (L.Var "_y", L.Var "_y"), L.Var "_F"),
              L.Var "_x"))))),
          L.Lam ("_y", L.Lam ("_F", L.App (L.Var "_F", L.Lam ("_x",
            L.App (L.App (L.App (L.Var "_y", L.Var "_y"), L.Var "_F"),
              L.Var "_x")))))) in
        (* L.Lam ("_f",
          L.App (L.Lam ("_x", L.App (L.Var "_f", L.App (L.Var "_x", L.Var "_x"))),
            L.Lam ("_x", L.App (L.Var "_f", L.App (L.Var "_x", L.Var "_x")))))
      in *)
      L.App (L.Lam (x, compile z),
        L.App (ycomb, L.Lam (x, compile (Lam (xs, y)))))
  | And (x, y) ->
      compile (If (x, y, False))
  | Eq (x, y) ->
      compile (And (IsZero (Sub (x, y)), IsZero (Sub (y, x))))
  | Or (x, y) ->
      compile (If (x, True, y))
  | Pair (x, y) ->
      L.Lam ("_f",
        L.App (L.App (L.Var "_f", compile x), compile y))
  | Fst (x) ->
      L.App (compile x, L.Lam ("_x", L.Lam ("_", L.Var "_x")))
  | Snd (x) ->
      L.App (compile x, L.Lam ("_", L.Lam ("_y", L.Var "_y")))
  | Nil ->
      L.Lam ("_c", L.Lam ("_n", L.Var "_n"))
  | IsNil (x) ->
      L.App (L.App (compile x, L.Lam ("_", L.Lam ("_", f))), t)
  | Cons (x, y) ->
      L.Lam ("_c", L.Lam ("_n",
        L.App (L.App (L.Var "_c", compile x),
          L.App (L.App (compile y, L.Var "_c"), L.Var "_n"))))
  | Head (x) ->
      L.App (L.App (compile x, L.Lam ("_x", L.Lam ("_", L.Var "_x"))), f)
  | Tail (x) ->
      compile (Fst (App (x,
        [Lam (["_x"; "_p"], Pair (Snd (Var "_p"),
          Cons (Var "_x", Snd (Var "_p")))); Pair (Nil, Nil)])))
