open Base
open Calculator.Ast

let failures = ref 0

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

(* --- Substitution check: every produced root must satisfy the equation --- *)

let solve_parts (input : string) : expr * expr * string =
  match Calculator.Parser.parse (Calculator.Lexer.tokenize input) with
  | Solve (left, right, variable) -> (left, right, variable)
  | _ -> failwith ("test input is not a solve statement: " ^ input)

let float_at (tree : expr) ~(var : string) ~(value : float) : float =
  let env = Map.singleton (module String) var (Calculator.Value.Approx value) in
  Calculator.Value.to_float (Calculator.Eval.eval ~env tree)

(* Rational roots are checked exactly; symbolic roots (sqrt forms) are
   checked numerically within a tight tolerance. *)
let check_roots_satisfy ~(input : string) : unit =
  let left, right, variable = solve_parts input in
  let equation = Sub (left, right) in
  let check_root (root : expr) : unit =
    match root with
    | Num q ->
        let env =
          Map.singleton (module String) variable (Calculator.Value.Exact q)
        in
        let value =
          Calculator.Value.to_string (Calculator.Eval.eval ~env equation)
        in
        if not (String.equal value "0") then begin
          Stdlib.Printf.printf "FAIL: %s: root %s gives %s, expected 0\n" input
            (Q.to_string q) value;
          Int.incr failures
        end
    | symbolic ->
        let root_value =
          Calculator.Value.to_float
            (Calculator.Eval.eval ~env:(Map.empty (module String)) symbolic)
        in
        let value = float_at equation ~var:variable ~value:root_value in
        if Float.(abs value > 1e-9) then begin
          Stdlib.Printf.printf "FAIL: %s: root %s gives %g, expected 0\n" input
            (Calculator.Printer.to_string symbolic)
            value;
          Int.incr failures
        end
  in
  match Calculator.Solve.solve ~left ~right ~var:variable with
  | Solutions roots | Partial (roots, _) -> List.iter roots ~f:check_root
  | No_solution | No_real_solution | All_reals ->
      Stdlib.Printf.printf "FAIL: %s produced no roots to check\n" input;
      Int.incr failures

(* --- Property test: build (x - r1)*...*(x - rk), solve, get the roots
   back. This closes the loop over exact arithmetic, simplification, the
   polynomial toolkit, factoring, and solving in one test. --- *)

let check_reconstruction (roots : Q.t list) : unit =
  let poly =
    List.fold roots ~init:[| Q.one |] ~f:(fun acc r ->
        Calculator.Polynomial.mul acc [| Q.neg r; Q.one |])
  in
  let tree = Calculator.Polynomial.to_expr poly ~var:"x" in
  let expected = List.dedup_and_sort roots ~compare:Q.compare in
  let label =
    String.concat ~sep:", " (List.map roots ~f:Q.to_string)
  in
  match Calculator.Solve.solve ~left:tree ~right:(Num Q.zero) ~var:"x" with
  | Solutions found ->
      let found_rationals =
        List.map found ~f:(fun root ->
            match root with
            | Num q -> q
            | other ->
                failwith
                  ("non-rational root: " ^ Calculator.Printer.to_string other))
      in
      if not (List.equal Q.equal found_rationals expected) then begin
        Stdlib.Printf.printf "FAIL: roots {%s} came back as {%s}\n" label
          (String.concat ~sep:", "
             (List.map found_rationals ~f:Q.to_string));
        Int.incr failures
      end
  | _ ->
      Stdlib.Printf.printf "FAIL: roots {%s}: solver found no solutions\n"
        label;
      Int.incr failures

let () =
  (* the PRD table, end to end *)
  check_output ~inputs:[ "solve(2*x + 3 = 7, x)" ] ~expected:"x = 2";
  check_output ~inputs:[ "solve(x + 1 = x + 1, x)" ]
    ~expected:"all real numbers";
  check_output ~inputs:[ "solve(x + 1 = x + 2, x)" ] ~expected:"no solution";
  check_output ~inputs:[ "solve(x^2 = 4, x)" ] ~expected:"x = -2, x = 2";
  check_output ~inputs:[ "solve(x^2 + 2*x + 1 = 0, x)" ] ~expected:"x = -1";
  check_output ~inputs:[ "solve(x^2 = 2, x)" ]
    ~expected:"x = -sqrt(2), x = sqrt(2)";
  check_output ~inputs:[ "solve(x^2 + 1 = 0, x)" ]
    ~expected:"no real solutions";
  check_output ~inputs:[ "solve(x^3 - x, x)" ]
    ~expected:"x = -1, x = 0, x = 1" (* implicit = 0 *);
  check_output ~inputs:[ "solve(x^3 - 2*x^2 - x + 2 = 0, x)" ]
    ~expected:"x = -1, x = 1, x = 2";
  check_output ~inputs:[ "solve((x - 1)*(x + 3) = 0, x)" ]
    ~expected:"x = -3, x = 1";
  check_output ~inputs:[ "solve(x/2 = 3, x)" ] ~expected:"x = 6";
  check_output ~inputs:[ "solve(a*x + b = 0, x)" ] ~expected:"x = -b/a";
  check_output ~inputs:[ "solve(y + 1 = 0, x)" ] ~expected:"no solution";
  check_output ~inputs:[ "solve(x^5 - x - 1 = 0, x)" ]
    ~expected:"unsolved: roots of x^5 - x - 1";
  check_output ~inputs:[ "let a = 2"; "solve(a*x = 4, x)" ] ~expected:"x = 2";

  (* more coverage *)
  check_output ~inputs:[ "solve(x^2 - x - 1 = 0, x)" ]
    ~expected:"x = 1/2*(1 - sqrt(5)), x = 1/2*(sqrt(5) + 1)";
  check_output ~inputs:[ "solve(x*y = 0, x)" ] ~expected:"x = 0";
  check_output ~inputs:[ "solve(x^4 - 1 = 0, x)" ] ~expected:"x = -1, x = 1";
  check_output ~inputs:[ "solve(2*t = 5, t)" ] ~expected:"t = 5/2";
  check_output ~inputs:[ "solve(diff(x^2, x) = 4, x)" ] ~expected:"x = 2";
  check_output ~inputs:[ "solve(0 = 0, x)" ] ~expected:"all real numbers";
  (* a bound solve variable is shadowed, like diff *)
  check_output ~inputs:[ "let x = 5"; "solve(x^2 = 4, x)" ]
    ~expected:"x = -2, x = 2";
  (* partial answers keep the rational roots they found:
     x^4 - x^3 - x^2 + 1 = (x - 1)*(x^3 - x - 1) *)
  check_output ~inputs:[ "solve(x^4 - x^3 - x^2 + 1 = 0, x)" ]
    ~expected:"x = 1; unsolved: roots of x^3 - x - 1";

  (* errors *)
  check_error ~input:"solve(sin(x) = 0, x)"
    ~expected_error:(Calculator.Calc_error.Cannot_solve "x");
  check_error ~input:"solve(2^x = 8, x)"
    ~expected_error:(Calculator.Calc_error.Cannot_solve "x");
  check_error ~input:"solve(a*x^2 + x = 0, x)"
    ~expected_error:(Calculator.Calc_error.Cannot_solve "x")
    (* symbolic quadratics are out of scope *);
  check_error ~input:"solve(x^2 = 4)"
    ~expected_error:Calculator.Calc_error.Expected_comma;
  check_error ~input:"solve(x = 4, 3)"
    ~expected_error:Calculator.Calc_error.Expected_variable_name;
  check_error ~input:"1 + solve(x = 1, x)"
    ~expected_error:(Calculator.Calc_error.Unexpected_token "SOLVE");

  (* substitution check on everything that produces roots *)
  List.iter ~f:(fun input -> check_roots_satisfy ~input)
    [ "solve(2*x + 3 = 7, x)";
      "solve(x^2 = 4, x)";
      "solve(x^2 + 2*x + 1 = 0, x)";
      "solve(x^2 = 2, x)";
      "solve(x^3 - x, x)";
      "solve(x^3 - 2*x^2 - x + 2 = 0, x)";
      "solve((x - 1)*(x + 3) = 0, x)";
      "solve(x/2 = 3, x)";
      "solve(x^2 - x - 1 = 0, x)";
      "solve(x^4 - 1 = 0, x)" ];

  (* property test: chosen roots must come back exactly *)
  List.iter ~f:check_reconstruction
    [ [ Q.of_int 2; Q.of_int 3 ];
      [ Q.of_ints 1 2; Q.of_int (-3) ];
      [ Q.zero; Q.zero; Q.of_int 5 ];
      [ Q.of_int (-1); Q.of_int (-1); Q.of_int 2; Q.of_ints 1 3 ];
      [ Q.of_ints (-7) 2 ];
      [ Q.of_int 1; Q.of_int 2; Q.of_int 3; Q.of_int 4; Q.of_int 5 ] ];

  if !failures > 0 then begin
    Stdlib.Printf.printf "%d solve test(s) failed\n" !failures;
    Stdlib.exit 1
  end
  else Stdlib.print_endline "All solve tests passed"
