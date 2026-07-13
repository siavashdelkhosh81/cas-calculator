open Base
open Calculator.Ast

let failures = ref 0

let parse_expr (input : string) : expr =
  match Calculator.Parser.parse (Calculator.Lexer.tokenize input) with
  | Expression tree -> tree
  | Let_binding _ | Solve _ -> failwith ("test input is not an expression: " ^ input)

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
  | Diff (a, va), Diff (b, vb) -> equal_expr a b && String.equal va vb
  | _ -> false

(* The derivative of [input] with respect to x must simplify to the same
   canonical tree as [expected]. *)
let check_diff ~(input : string) ~(expected : string) : unit =
  let derivative =
    Calculator.Simplify.simplify (Calculator.Diff.diff (parse_expr input) "x")
  in
  let wanted = Calculator.Simplify.simplify (parse_expr expected) in
  if not (equal_expr derivative wanted) then begin
    Stdlib.Printf.printf "FAIL: diff(%s, x) = %s, expected %s\n" input
      (Calculator.Printer.to_string derivative)
      (Calculator.Printer.to_string wanted);
    Int.incr failures
  end

(* Evaluate [inputs] in order, threading the environment, and check that the
   last one renders exactly as [expected]. This pins the printed output the
   user actually sees. *)
let check_output ~(inputs : string list) ~(expected : string) : unit =
  let env = Map.empty (module String) in
  let final =
    List.fold inputs ~init:(Ok ("", env)) ~f:(fun acc input ->
        match acc with
        | Error _ as err -> err
        | Ok (_, env) -> Calculator.Eval.evaluate ~env ~input)
  in
  let label = String.concat ~sep:"; " inputs in
  match final with
  | Ok (text, _env) ->
      if not (String.equal text expected) then begin
        Stdlib.Printf.printf "FAIL: %s = %s, expected %s\n" label text expected;
        Int.incr failures
      end
  | Error err ->
      Stdlib.Printf.printf "FAIL: %s returned error %s, expected %s\n" label
        (Calculator.Calc_error.to_string err)
        expected;
      Int.incr failures

let check_error ~(input : string) ~(expected_error : Calculator.Calc_error.error)
    : unit =
  let env = Map.empty (module String) in
  match Calculator.Eval.evaluate ~env ~input with
  | Error err ->
      if not (Poly.equal err expected_error) then begin
        Stdlib.Printf.printf "FAIL: %s returned error %s, expected %s\n" input
          (Calculator.Calc_error.to_string err)
          (Calculator.Calc_error.to_string expected_error);
        Int.incr failures
      end
  | Ok (text, _) ->
      Stdlib.Printf.printf "FAIL: %s = %s, expected error %s\n" input text
        (Calculator.Calc_error.to_string expected_error);
      Int.incr failures

(* Numeric cross-check: the symbolic derivative evaluated at [x0] must match
   the central-difference slope (f(x0+h) - f(x0-h)) / 2h. This catches wrong
   rules that still look plausible when printed. *)
let eval_at ~(x0 : float) (tree : expr) : float =
  let env =
    Map.singleton (module String) "x" (Calculator.Value.Approx x0)
  in
  Calculator.Value.to_float (Calculator.Eval.eval ~env tree)

let check_slope ~(input : string) ~(points : float list) : unit =
  let tree = parse_expr input in
  let derivative = Calculator.Diff.diff tree "x" in
  List.iter points ~f:(fun x0 ->
      let h = 1e-5 in
      let numeric =
        (eval_at ~x0:(x0 +. h) tree -. eval_at ~x0:(x0 -. h) tree) /. (2. *. h)
      in
      let symbolic = eval_at ~x0 derivative in
      let tolerance = 1e-4 *. Float.max 1.0 (Float.abs numeric) in
      if Float.(abs (numeric - symbolic) > tolerance) then begin
        Stdlib.Printf.printf
          "FAIL: d/dx %s at %g: symbolic %g, numeric slope %g\n" input x0
          symbolic numeric;
        Int.incr failures
      end)

(* (expression, expected derivative) — every rule in the PRD table. *)
let rule_table : (string * string) list =
  [ ("7", "0");
    ("x", "1");
    ("y", "0");
    ("x^2", "2*x");
    ("x^3 + 2*x", "3*x^2 + 2");
    ("x^2 + y^2", "2*x");
    ("x*y", "y");
    ("x*sin(x)", "sin(x) + x*cos(x)");
    ("x/(x + 1)", "1/(x + 1)^2");
    ("-x^2", "-2*x");
    ("x^(1/2)", "1/2 * x^(-1/2)");
    ("1/x", "-1/x^2");
    ("2^x", "2^x * ln(2)");
    ("sin(x)", "cos(x)");
    ("cos(x)", "-sin(x)");
    ("tan(x)", "1/cos(x)^2");
    ("sin(x^2)", "2*x*cos(x^2)");
    ("exp(x)", "exp(x)");
    ("exp(2*x)", "2*exp(2*x)");
    ("ln(x)", "1/x");
    ("log(x)", "1/(x*ln(10))");
    ("log2(x)", "1/(x*ln(2))");
    ("sqrt(x)", "1/(2*sqrt(x))");
    ("asin(x)", "1/sqrt(1 - x^2)");
    ("acos(x)", "-1/sqrt(1 - x^2)");
    ("atan(x)", "1/(1 + x^2)");
    ("sinh(x)", "cosh(x)");
    ("cosh(x)", "sinh(x)");
    ("tanh(x)", "1/cosh(x)^2") ]

(* (expression, safe sample points) for the numeric cross-check. Points stay
   inside every function's domain (|x| < 1 for asin/acos, x > 0 for ln/sqrt,
   away from tan's poles). *)
let slope_table : (string * float list) list =
  [ ("x^2", [ 0.5; 1.3; -2.0 ]);
    ("x^3 + 2*x", [ 0.5; 1.3; -2.0 ]);
    ("x^(1/2)", [ 0.5; 1.3 ]);
    ("1/x", [ 0.5; 1.3; -2.0 ]);
    ("x/(x + 1)", [ 0.5; 1.3 ]);
    ("2^x", [ 0.5; 1.3 ]);
    ("x^x", [ 0.5; 1.3 ]);
    ("sin(x)", [ 0.5; 1.3; -2.0 ]);
    ("cos(x)", [ 0.5; 1.3; -2.0 ]);
    ("tan(x)", [ 0.5; 1.3 ]);
    ("sin(x^2)", [ 0.5; 1.3 ]);
    ("x*sin(x)", [ 0.5; 1.3 ]);
    ("exp(2*x)", [ 0.5; 1.3 ]);
    ("ln(x)", [ 0.5; 1.3 ]);
    ("log(x)", [ 0.5; 1.3 ]);
    ("log2(x)", [ 0.5; 1.3 ]);
    ("sqrt(x)", [ 0.5; 1.3 ]);
    ("asin(x)", [ 0.3; 0.7 ]);
    ("acos(x)", [ 0.3; 0.7 ]);
    ("atan(x)", [ 0.5; 1.3 ]);
    ("sinh(x)", [ 0.5; 1.3 ]);
    ("cosh(x)", [ 0.5; 1.3 ]);
    ("tanh(x)", [ 0.5; 1.3 ]) ]

let () =
  List.iter rule_table ~f:(fun (input, expected) ->
      check_diff ~input ~expected);
  List.iter slope_table ~f:(fun (input, points) ->
      check_slope ~input ~points);

  (* What the REPL prints, end to end. *)
  check_output ~inputs:[ "diff(x^2, x)" ] ~expected:"2*x";
  check_output ~inputs:[ "diff(x, x)" ] ~expected:"1";
  check_output ~inputs:[ "diff(y, x)" ] ~expected:"0";
  check_output ~inputs:[ "diff(7, x)" ] ~expected:"0";
  check_output ~inputs:[ "diff(sin(x), x)" ] ~expected:"cos(x)";
  check_output ~inputs:[ "diff(cos(x), x)" ] ~expected:"-sin(x)";
  check_output ~inputs:[ "diff(ln(x), x)" ] ~expected:"1/x";
  check_output ~inputs:[ "diff(1/x, x)" ] ~expected:"-1/x^2";
  check_output ~inputs:[ "diff(sin(x^2), x)" ] ~expected:"2*x*cos(x^2)";
  check_output ~inputs:[ "diff(x*sin(x), x)" ] ~expected:"sin(x) + x*cos(x)";
  check_output ~inputs:[ "diff(exp(2*x), x)" ] ~expected:"2*exp(2*x)";
  check_output ~inputs:[ "diff(x^2 + y^2, x)" ] ~expected:"2*x";

  (* diff is a normal expression: it nests and mixes with arithmetic. *)
  check_output ~inputs:[ "diff(diff(x^3, x), x)" ] ~expected:"6*x";
  check_output ~inputs:[ "diff(x^2, x) + 1" ] ~expected:"2*x + 1";
  check_output ~inputs:[ "diff(x, x) + 1" ] ~expected:"2";

  (* Bindings substitute into the body, but never into the diff variable. *)
  check_output ~inputs:[ "let x = 5"; "diff(x^2, x)" ] ~expected:"2*x";
  check_output ~inputs:[ "let c = 3"; "diff(c*x^2, x)" ] ~expected:"6*x";

  (* Clear errors. *)
  check_error ~input:"diff(abs(x), x)"
    ~expected_error:(Calculator.Calc_error.Not_differentiable "abs");
  check_error ~input:"diff(floor(x), x)"
    ~expected_error:(Calculator.Calc_error.Not_differentiable "floor");
  check_error ~input:"diff(x^2)"
    ~expected_error:Calculator.Calc_error.Expected_comma;
  check_error ~input:"diff(x^2, 3)"
    ~expected_error:Calculator.Calc_error.Expected_variable_name;
  check_error ~input:"diff(x^2, x"
    ~expected_error:Calculator.Calc_error.Missing_rparen;

  if !failures > 0 then begin
    Stdlib.Printf.printf "%d diff test(s) failed\n" !failures;
    Stdlib.exit 1
  end
  else Stdlib.print_endline "All diff tests passed"
