module S = Set.Make (String)
module Un = Unlambda

type lambda =
  | Var of string
  | Lam of string * lambda
  | App of lambda * lambda
  | S
  | K
  | I
  | V
  | D of lambda
  | Dot of char

open Printf

let rec output fmt = function
  | Var x -> fprintf fmt "%s" x
  | Lam (x, y) -> fprintf fmt "(lambda (%s) %a)" x output y
  | App (x, y) -> fprintf fmt "(%a %a)" output x output y
  | S -> fprintf fmt "S"
  | K -> fprintf fmt "K"
  | I -> fprintf fmt "I"
  | V -> fprintf fmt "V"
  | D (x) -> fprintf fmt "`d%a" output x
  | Dot x -> fprintf fmt "(print_char %c)" x

let rec fv = function
  | Var x -> S.singleton x
  | Lam (x, e) -> S.remove x (fv e)
  | App (x, y) -> S.union (fv x) (fv y)
  | _ -> S.empty

let rec abstract = function
  | Var x ->
      Var x
  | App (e1, e2) ->
      App (abstract e1, abstract e2)
  | Lam (x, e) when not (S.mem x (fv e)) ->
      D (App (K, abstract e))
      (* App (K, abstract e) *)
  | Lam (x, Var y) when x = y ->
      I
  | Lam (x, Lam (y, e)) when (S.mem x (fv e)) ->
      abstract (Lam (x, abstract (Lam (y, e))))
  (* | Lam (x, App (e, Var y)) when y = x && (not (S.mem x (fv e))) ->
      abstract e *)
  | Lam (x, App (e1, e2)) ->
      App (App (S, abstract (Lam (x, e1))),
        abstract (Lam (x, e2)))
  | _ as v ->
      v

let rec unlambda = function
  | Var v ->
      failwith (Printf.sprintf "free var: %s" v)
  | Lam _ -> assert false
  | App (x, y) -> Un.TICK (unlambda x, unlambda y)
  | S -> Un.TERM Un.S
  | K -> Un.TERM Un.K
  | I -> Un.TERM Un.I
  | V -> Un.TERM Un.V
  | D (x) -> Un.D (unlambda x)
  | Dot (c) -> Un.TERM (Un.Dot c)
