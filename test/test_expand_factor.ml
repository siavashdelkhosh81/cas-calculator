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
  | Diff (a, va), Diff (b, vb) -> equal_expr a b && String.equal va vb
  | _ -> false

(* Evaluate [inputs] in order, threading the environment, and check that the
   last one renders exactly as [expected]. *)
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

(* Expanding [input] must land on the same canonical tree as simplifying
   [expected]. *)
let check_expands ~(input : string) ~(expected : string) : unit =
  let actual = Calculator.Simplify.expand (parse_expr input) in
  let wanted = Calculator.Simplify.simplify (parse_expr expected) in
  if not (equal_expr actual wanted) then begin
    Stdlib.Printf.printf "FAIL: expand %s = %s, expected %s\n" input
      (Calculator.Printer.to_string actual)
      (Calculator.Printer.to_string wanted);
    Int.incr failures
  end

(* Round-trip identity: expand(factor(p)) must equal expand(p) — factoring
   must never change what the polynomial *is*. *)
let check_roundtrip ~(input : string) : unit =
  let tree = parse_expr input in
  let refactored = Calculator.Simplify.expand (Calculator.Factor.factor tree) in
  let expanded = Calculator.Simplify.expand tree in
  if not (equal_expr refactored expanded) then begin
    Stdlib.Printf.printf "FAIL: expand(factor(%s)) = %s, expected %s\n" input
      (Calculator.Printer.to_string refactored)
      (Calculator.Printer.to_string expanded);
    Int.incr failures
  end

(* Exact numeric identity: the original and its factored/expanded forms must
   agree at sample rational points — Q.equal, no epsilon. *)
let value_at (tree : expr) (x : Q.t) : string =
  let env = Map.singleton (module String) "x" (Calculator.Value.Exact x) in
  Calculator.Value.to_string (Calculator.Eval.eval ~env tree)

let check_value_identity ~(input : string) : unit =
  let tree = parse_expr input in
  let variants =
    [ ("factor", Calculator.Factor.factor tree);
      ("expand", Calculator.Simplify.expand tree) ]
  in
  let samples = [ Q.of_int 2; Q.of_int (-3); Q.of_ints 7 2; Q.zero; Q.one ] in
  List.iter variants ~f:(fun (name, transformed) ->
      List.iter samples ~f:(fun x ->
          let before = value_at tree x in
          let after = value_at transformed x in
          if not (String.equal before after) then begin
            Stdlib.Printf.printf "FAIL: %s(%s) at x=%s: %s, expected %s\n" name
              input (Q.to_string x) after before;
            Int.incr failures
          end))

(* --- Poly unit tests ------------------------------------------------- *)

let poly_of (input : string) : Calculator.Polynomial.t =
  Calculator.Polynomial.of_expr (parse_expr input) ~var:"x"

let poly_equal (a : Calculator.Polynomial.t) (b : Calculator.Polynomial.t) : bool =
  Array.length a = Array.length b && Array.for_all2_exn a b ~f:Q.equal

let show_poly (p : Calculator.Polynomial.t) : string =
  Calculator.Printer.to_string
    (Calculator.Simplify.simplify (Calculator.Polynomial.to_expr p ~var:"x"))

(* divmod invariant: quotient * divisor + remainder = dividend, and the
   remainder's degree is below the divisor's. *)
let check_divmod ~(dividend : string) ~(divisor : string) : unit =
  let p = poly_of dividend and d = poly_of divisor in
  let quotient, remainder = Calculator.Polynomial.divmod p d in
  let rebuilt =
    Calculator.Polynomial.add (Calculator.Polynomial.mul quotient d) remainder
  in
  if not (poly_equal rebuilt p) then begin
    Stdlib.Printf.printf "FAIL: divmod %s by %s: %s * %s + %s <> dividend\n"
      dividend divisor (show_poly quotient) divisor (show_poly remainder);
    Int.incr failures
  end;
  if Calculator.Polynomial.degree remainder >= Calculator.Polynomial.degree d then begin
    Stdlib.Printf.printf "FAIL: divmod %s by %s: remainder degree too high\n"
      dividend divisor;
    Int.incr failures
  end

let check_not_a_polynomial ~(input : string) : unit =
  match poly_of input with
  | _ ->
      Stdlib.Printf.printf "FAIL: of_expr %s should not be a polynomial\n"
        input;
      Int.incr failures
  | exception Calculator.Calc_error.Calc_error Calculator.Calc_error.Not_a_polynomial
    -> ()

let () =
  (* expand: PRD table, canonical-tree equality *)
  check_expands ~input:"(x+1)^2" ~expected:"x^2 + 2*x + 1";
  check_expands ~input:"(x+1)*(x-1)" ~expected:"x^2 - 1";
  check_expands ~input:"(x+y)^3"
    ~expected:"x^3 + 3*x^2*y + 3*x*y^2 + y^3";
  check_expands ~input:"(x+1)^0" ~expected:"1";
  check_expands ~input:"x*(x+1) - x^2" ~expected:"x";
  check_expands ~input:"(x+y)*(x-y)" ~expected:"x^2 - y^2";
  check_expands ~input:"2*(x+3)" ~expected:"2*x + 6";
  check_expands ~input:"sin(x)*(x+1)" ~expected:"x*sin(x) + sin(x)";
  check_expands ~input:"(x+1)^(-2)" ~expected:"(x+1)^(-2)" (* stays put *);

  (* expand: what the REPL prints *)
  check_output ~inputs:[ "expand((x+1)^2)" ] ~expected:"x^2 + 2*x + 1";
  check_output ~inputs:[ "expand((x+1)^0)" ] ~expected:"1";
  check_output ~inputs:[ "expand(x*(x+1) - x^2)" ] ~expected:"x";
  check_output ~inputs:[ "expand((x+y)*(x-y))" ] ~expected:"x^2 - y^2";
  check_output ~inputs:[ "expand(2*(x+3))" ] ~expected:"2*x + 6";
  check_output ~inputs:[ "expand(sin(x)*(x+1))" ]
    ~expected:"sin(x) + x*sin(x)";
  check_output ~inputs:[ "expand((x+1)*(x-1))" ] ~expected:"x^2 - 1";
  check_output ~inputs:[ "diff(expand((x+1)^2), x)" ] ~expected:"2*x + 2";
  (* bound variables substitute before expanding *)
  check_output
    ~inputs:[ "let x = 2"; "expand((x+1)^50)" ]
    ~expected:(Z.to_string (Z.pow (Z.of_int 3) 50));

  (* expand((x+1)^50): 51 terms, no blowup *)
  (let env = Map.empty (module String) in
   match Calculator.Eval.evaluate ~env ~input:"expand((x+1)^50)" with
   | Ok (text, _) when String.is_substring text ~substring:"x^50" -> ()
   | Ok (text, _) ->
       Stdlib.Printf.printf "FAIL: expand((x+1)^50) = %s, expected x^50 term\n"
         text;
       Int.incr failures
   | Error err ->
       Stdlib.Printf.printf "FAIL: expand((x+1)^50) returned error %s\n"
         (Calculator.Calc_error.to_string err);
       Int.incr failures);

  (* factor: PRD table, exact printed output *)
  check_output ~inputs:[ "factor(x^2 - 1)" ] ~expected:"(x - 1)*(x + 1)";
  check_output ~inputs:[ "factor(x^2 + 2*x + 1)" ] ~expected:"(x + 1)^2";
  check_output ~inputs:[ "factor(x^2 + 1)" ] ~expected:"x^2 + 1";
  check_output ~inputs:[ "factor(6*x + 9)" ] ~expected:"3*(2*x + 3)";
  check_output ~inputs:[ "factor(x^3 - x)" ] ~expected:"x*(x - 1)*(x + 1)";
  check_output ~inputs:[ "factor(2*x^2 - 8)" ] ~expected:"2*(x - 2)*(x + 2)";
  check_output ~inputs:[ "factor(5)" ] ~expected:"5";
  check_output ~inputs:[ "factor(x)" ] ~expected:"x";
  check_output ~inputs:[ "factor(x + 1)" ] ~expected:"x + 1";
  check_output ~inputs:[ "factor(2*x^2 + x - 1)" ]
    ~expected:"(2*x - 1)*(x + 1)";
  (* bound variables substitute first: this factors 2*x^2 - 8 *)
  check_output ~inputs:[ "let c = 2"; "factor(c*x^2 - 8)" ]
    ~expected:"2*(x - 2)*(x + 2)";

  (* factor: clear errors *)
  check_error ~input:"factor(x^2 + x*y)"
    ~expected_error:Calculator.Calc_error.Not_a_polynomial;
  check_error ~input:"factor(sin(x)^2 - 1)"
    ~expected_error:Calculator.Calc_error.Not_a_polynomial;
  check_error ~input:"factor(x^(1/2))"
    ~expected_error:Calculator.Calc_error.Not_a_polynomial;
  check_error ~input:"factor(1/x)"
    ~expected_error:Calculator.Calc_error.Not_a_polynomial;

  (* round-trip identity + exact numeric identity *)
  List.iter ~f:(fun input ->
      check_roundtrip ~input;
      check_value_identity ~input)
    [ "x^2 - 1";
      "x^2 + 2*x + 1";
      "x^2 + 1";
      "6*x + 9";
      "x^3 - x";
      "2*x^2 - 8";
      "2*x^2 + x - 1";
      "x^4 - 1";
      "x^3 + 3*x^2 + 3*x + 1";
      "x^2/2 - 1/2" ];

  (* Poly unit tests *)
  check_divmod ~dividend:"x^3 - x" ~divisor:"x - 1";
  check_divmod ~dividend:"x^3 + 2*x + 5" ~divisor:"x^2 + 1";
  check_divmod ~dividend:"2*x^2 + x - 1" ~divisor:"2*x - 1";
  check_divmod ~dividend:"x + 1" ~divisor:"x^2 + 1" (* dividend smaller *);

  (* eval_at: Horner at 3/2 on x^3 - 2*x + 5 is 27/8 - 3 + 5 = 43/8 *)
  (let p = poly_of "x^3 - 2*x + 5" in
   let got = Calculator.Polynomial.eval_at p (Q.of_ints 3 2) in
   if not (Q.equal got (Q.of_ints 43 8)) then begin
     Stdlib.Printf.printf "FAIL: eval_at gave %s, expected 43/8\n"
       (Q.to_string got);
     Int.incr failures
   end);

  (* of_expr/to_expr round-trip *)
  (let p = poly_of "2*x^3 - x/2 + 7" in
   let back =
     Calculator.Polynomial.of_expr (Calculator.Polynomial.to_expr p ~var:"x") ~var:"x"
   in
   if not (poly_equal p back) then begin
     Stdlib.Printf.printf "FAIL: of_expr/to_expr round-trip changed %s\n"
       (show_poly p);
     Int.incr failures
   end);

  check_not_a_polynomial ~input:"sin(x)";
  check_not_a_polynomial ~input:"y + 1" (* wrong variable *);
  check_not_a_polynomial ~input:"x^(1/2)";
  check_not_a_polynomial ~input:"2^x";

  if !failures > 0 then begin
    Stdlib.Printf.printf "%d expand/factor test(s) failed\n" !failures;
    Stdlib.exit 1
  end
  else Stdlib.print_endline "All expand/factor tests passed"
