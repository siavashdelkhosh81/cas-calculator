open Base
open Calculator.Ast

let failures = ref 0

let parse_expr (input : string) : expr =
  match Calculator.Parser.parse (Calculator.Lexer.tokenize input) with
  | Expression tree -> tree
  | Let_binding _ -> failwith ("test input is a let binding: " ^ input)

let rec equal_expr (left : expr) (right : expr) : bool =
  match (left, right) with
  | Num a, Num b -> Q.equal a b
  | Var a, Var b -> String.equal a b
  | Add (a1, a2), Add (b1, b2)
  | Sub (a1, a2), Sub (b1, b2)
  | Mul (a1, a2), Mul (b1, b2)
  | Div (a1, a2), Div (b1, b2)
  | Expo (a1, a2), Expo (b1, b2) -> equal_expr a1 b1 && equal_expr a2 b2
  | Func (n1, a), Func (n2, b) -> String.equal n1 n2 && equal_expr a b
  | Neg a, Neg b -> equal_expr a b
  | _ -> false

(* Fully parenthesized rendering, only for failure messages. *)
let rec show_expr (tree : expr) : string =
  match tree with
  | Num q -> Q.to_string q
  | Var name -> name
  | Add (a, b) -> Printf.sprintf "(%s + %s)" (show_expr a) (show_expr b)
  | Sub (a, b) -> Printf.sprintf "(%s - %s)" (show_expr a) (show_expr b)
  | Mul (a, b) -> Printf.sprintf "(%s * %s)" (show_expr a) (show_expr b)
  | Div (a, b) -> Printf.sprintf "(%s / %s)" (show_expr a) (show_expr b)
  | Expo (a, b) -> Printf.sprintf "(%s ^ %s)" (show_expr a) (show_expr b)
  | Func (name, a) -> Printf.sprintf "%s(%s)" name (show_expr a)
  | Neg a -> Printf.sprintf "(-%s)" (show_expr a)

(* Simplifying [input] and [expected] must land on the same canonical tree,
   and simplify must be idempotent on the result. *)
let check_simplifies ~(input : string) ~(expected : string) : unit =
  let actual = Calculator.Simplify.simplify (parse_expr input) in
  let wanted = Calculator.Simplify.simplify (parse_expr expected) in
  if not (equal_expr actual wanted) then begin
    Stdlib.Printf.printf "FAIL: simplify %s = %s, expected %s\n" input
      (show_expr actual) (show_expr wanted);
    Int.incr failures
  end;
  let again = Calculator.Simplify.simplify actual in
  if not (equal_expr again actual) then begin
    Stdlib.Printf.printf "FAIL: simplify not idempotent on %s: %s became %s\n"
      input (show_expr actual) (show_expr again);
    Int.incr failures
  end

(* Check the exact output tree, so a simplifier that collapses everything to
   one canonical blob cannot pass by accident. *)
let check_tree ~(input : string) ~(expected : expr) : unit =
  let actual = Calculator.Simplify.simplify (parse_expr input) in
  if not (equal_expr actual expected) then begin
    Stdlib.Printf.printf "FAIL: simplify %s = %s, expected tree %s\n" input
      (show_expr actual) (show_expr expected);
    Int.incr failures
  end

(* Float evaluation with fixed variable values — used to check that
   simplification preserves the value of the expression. *)
let rec eval_float ~(x : float) ~(y : float) (tree : expr) : float =
  match tree with
  | Num q -> Q.to_float q
  | Var "x" -> x
  | Var "y" -> y
  | Var name -> failwith ("eval_float: unexpected variable " ^ name)
  | Add (a, b) -> eval_float ~x ~y a +. eval_float ~x ~y b
  | Sub (a, b) -> eval_float ~x ~y a -. eval_float ~x ~y b
  | Mul (a, b) -> eval_float ~x ~y a *. eval_float ~x ~y b
  | Div (a, b) -> eval_float ~x ~y a /. eval_float ~x ~y b
  | Expo (a, b) -> Float.( ** ) (eval_float ~x ~y a) (eval_float ~x ~y b)
  | Neg a -> Float.neg (eval_float ~x ~y a)
  | Func (name, a) -> (
      let arg = eval_float ~x ~y a in
      match name with
      | "sin" -> Float.sin arg
      | "cos" -> Float.cos arg
      | "ln" -> Float.log arg
      | "exp" -> Float.exp arg
      | "sqrt" -> Float.sqrt arg
      | "abs" -> Float.abs arg
      | _ -> failwith ("eval_float: unexpected function " ^ name))

let check_value_preserved ~(input : string) : unit =
  let x = 2.5 and y = -1.5 in
  let original = parse_expr input in
  let simplified = Calculator.Simplify.simplify original in
  let before = eval_float ~x ~y original in
  let after = eval_float ~x ~y simplified in
  if Float.(abs (before - after) > 1e-9 *. Float.max 1.0 (abs before)) then begin
    Stdlib.Printf.printf
      "FAIL: simplify changed the value of %s: %g became %g (as %s)\n" input
      before after (show_expr simplified);
    Int.incr failures
  end

(* (input, canonically equal form) pairs — the PRD edge-case table plus the
   identity, power, and known-function rules. *)
let table : (string * string) list =
  [ ("x + x", "2*x");
    ("x - x", "0");
    ("x + 2 + x + 3", "2*x + 5");
    ("2*x*3", "6*x");
    ("x/x", "1");
    ("x^2 * x^3", "x^5");
    ("(x + 1) - (x + 1)", "0");
    ("0*sin(x)", "0");
    ("sin(x) + sin(x)", "2*sin(x)");
    ("x*(1/x)", "1");
    ("-(-x)", "x");
    ("2 + 3", "5");
    ("x + y - y + 1", "x + 1");
    ("(x^2 * x) / x", "x^2");
    ("3 + x + 2", "x + 5");
    ("sin(0) + x", "x");
    ("x * 1 + 0", "x");
    ("x + 0", "x");
    ("x * 0", "0");
    ("x^1", "x");
    ("x^0", "1");
    ("1^x", "1");
    ("(x^2)^3", "x^6");
    ("2^3", "8");
    ("2^(-2)", "1/4");
    ("cos(0)", "1");
    ("ln(1)", "0");
    ("exp(0)", "1");
    ("sqrt(4)", "2");
    ("abs(-3)", "3");
    ("2*x + 3*x", "5*x") ]

let () =
  List.iter table ~f:(fun (input, expected) ->
      check_simplifies ~input ~expected;
      check_value_preserved ~input);
  (* Exact expected trees, so canonical-collapse bugs cannot hide. *)
  check_tree ~input:"x" ~expected:(Var "x");
  check_tree ~input:"x + x" ~expected:(Mul (Num (Q.of_int 2), Var "x"));
  check_tree ~input:"x - x" ~expected:(Num Q.zero);
  check_tree ~input:"sin(x)" ~expected:(Func ("sin", Var "x"));
  check_tree ~input:"sin(1)"
    ~expected:(Func ("sin", Num Q.one)) (* stays exact, never a float *);
  (* Different expressions must stay different. *)
  let two_x = Calculator.Simplify.simplify (parse_expr "x + x") in
  let just_x = Calculator.Simplify.simplify (parse_expr "x") in
  if equal_expr two_x just_x then begin
    Stdlib.Printf.printf "FAIL: 2*x and x simplified to the same tree\n";
    Int.incr failures
  end;
  if !failures > 0 then begin
    Stdlib.Printf.printf "%d simplify test(s) failed\n" !failures;
    Stdlib.exit 1
  end
  else Stdlib.print_endline "All simplify tests passed"
