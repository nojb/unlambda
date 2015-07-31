module M = Map.Make (String)

type lambda =
  | Var of string
  | Lam of string * lambda
  | App of lambda * lambda

type value =
  | Cls of (value -> value)

open Format

let rec pr_exp0 ppf = function
  | Var s -> fprintf ppf "%s" s
  | lam -> fprintf ppf "@[<1>(%a)@]" pr_lambda lam

and pr_app ppf = function
  | e ->  fprintf ppf "@[<2>%a@]" pr_other_applications e

and pr_other_applications ppf f =
  match f with
  | App (f, arg) -> fprintf ppf "%a@ %a" pr_app f pr_exp0 arg
  | f -> pr_exp0 ppf f

and pr_lambda ppf = function
  | Lam (s, lam) ->
      fprintf ppf "@[<1>\\%s.@ %a@]" s pr_lambda lam
  | e -> pr_app ppf e

let print_lambda =
  pr_lambda std_formatter

let rec pr_unlambda ppf = function
  | Var x -> fprintf ppf "$%s" x
  | Lam (x, y) -> fprintf ppf "^%s%a" x pr_unlambda y
  | App (x, y) -> fprintf ppf "`%a%a" pr_unlambda x pr_unlambda y

let rec eval env = function
  | Var x -> M.find x env
  | Lam (x, y) -> Cls (fun u -> eval (M.add x u env) y)
  | App (x, y) -> apply (eval env x) (eval env y)

and apply (Cls f) z =
  f z

let natify f =
  let count = ref 0 in
  ignore (apply (apply f (Cls (fun v -> incr count; v))) f);
  !count

let boolify f =
  let b = ref false in
  ignore (apply (apply (apply f (Cls (fun v -> b := true; v)))
    (Cls (fun v -> b := false; v))) f);
  !b

let listify f g =
  let u = ref [] in
  ignore (apply (apply f (Cls (fun l -> Cls (fun v -> u := g l :: !u; v)))) f);
  List.rev !u

let int_listify f =
  listify f natify
