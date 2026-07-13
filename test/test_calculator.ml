open Base

let failures = ref 0

(* Check that [input] evaluates to [expected]. The tolerance is loose because
   [evaluate] renders results with %g, which keeps 6 significant digits. *)
let check ~input ~expected =
  let env = Map.empty (module String) in
  match Calculator.Eval.evaluate ~env ~input with
  | Ok (text, _env) ->
      let value = Float.of_string text in
      if Float.(abs (value - expected) > 1e-4) then begin
        Stdlib.Printf.printf "FAIL: %s = %s, expected %g\n" input text expected;
        Int.incr failures
      end
  | Error _ ->
      Stdlib.Printf.printf "FAIL: %s returned an error, expected %g\n" input
        expected;
      Int.incr failures

(* Evaluate [inputs] in order, threading the environment between them, and
   check that the last one evaluates to [expected]. *)
let check_session ~inputs ~expected =
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
      let value = Float.of_string text in
      if Float.(abs (value - expected) > 1e-4) then begin
        Stdlib.Printf.printf "FAIL: %s = %s, expected %g\n" label text expected;
        Int.incr failures
      end
  | Error _ ->
      Stdlib.Printf.printf "FAIL: %s returned an error, expected %g\n" label
        expected;
      Int.incr failures

(* Check that [input] renders exactly as [expected] — used for exact
   arithmetic, where the output string itself (no float noise) is the point. *)
let check_exact ~input ~expected =
  let env = Map.empty (module String) in
  match Calculator.Eval.evaluate ~env ~input with
  | Ok (text, _env) ->
      if not (String.equal text expected) then begin
        Stdlib.Printf.printf "FAIL: %s = %s, expected %s\n" input text expected;
        Int.incr failures
      end
  | Error err ->
      Stdlib.Printf.printf "FAIL: %s returned error %s, expected %s\n" input
        (Calculator.Calc_error.to_string err)
        expected;
      Int.incr failures

(* Evaluate [inputs] in order, threading the environment, and check that the
   last one renders exactly as [expected]. *)
let check_session_exact ~inputs ~expected =
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

(* Check that [input] fails with exactly [expected_error]. *)
let check_error ~input ~expected_error =
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

let () =
  (* existing behavior *)
  check ~input:"1 + 2 * 3" ~expected:7.0;
  check ~input:"sin 0" ~expected:0.0;
  check ~input:"cos 0" ~expected:1.0;
  check ~input:"tan 0" ~expected:0.0;
  check ~input:"log 100" ~expected:2.0;
  check ~input:"ln 1" ~expected:0.0;
  check ~input:"abs(-5)" ~expected:5.0;
  check ~input:"abs 5" ~expected:5.0;
  check ~input:"floor 2.7" ~expected:2.0;
  check ~input:"sqrt 9" ~expected:3.0;
  check ~input:"-2 ^ 2" ~expected:(-4.0);

  (* new functions *)
  check ~input:"exp 0" ~expected:1.0;
  check ~input:"exp 1" ~expected:(Float.exp 1.0);
  check ~input:"ln(exp 1)" ~expected:1.0;
  check ~input:"log10 100" ~expected:2.0;
  check ~input:"log2 8" ~expected:3.0;
  check ~input:"ceil 2.1" ~expected:3.0;
  check ~input:"round 2.5" ~expected:3.0;
  check ~input:"round 2.4" ~expected:2.0;
  check ~input:"asin 1" ~expected:(Float.pi /. 2.0);
  check ~input:"acos 1" ~expected:0.0;
  check ~input:"atan 1" ~expected:(Float.pi /. 4.0);
  check ~input:"sinh 0" ~expected:0.0;
  check ~input:"cosh 0" ~expected:1.0;
  check ~input:"tanh 0" ~expected:0.0;

  (* let bindings *)
  check_session ~inputs:[ "let x = 5"; "x + 1" ] ~expected:6.0;
  check_session ~inputs:[ "let x = 2"; "let y = x ^ 3"; "y - x" ] ~expected:6.0;
  check_session ~inputs:[ "let x = 1"; "let x = 10"; "x" ] ~expected:10.0;

  (* symbolic evaluation: unbound variables stay symbolic and the result is
     simplified and pretty-printed instead of erroring *)
  check_exact ~input:"x + 1" ~expected:"1 + x";
  check_exact ~input:"x + x" ~expected:"2*x";
  check_exact ~input:"x - x" ~expected:"0";
  check_exact ~input:"x * 0" ~expected:"0";
  check_exact ~input:"x/x" ~expected:"1";
  check_exact ~input:"sin(x) + sin(x)" ~expected:"2*sin(x)";
  check_exact ~input:"2*x + 3*x + 1" ~expected:"1 + 5*x";
  (* bound variables still substitute before simplifying *)
  check_session_exact ~inputs:[ "let x = 5"; "x + y" ] ~expected:"5 + y";
  (* approximate bindings still evaluate numerically *)
  check_session ~inputs:[ "let a = sin 1"; "a + 1" ]
    ~expected:(Float.sin 1.0 +. 1.0);
  (* a diff variable stays symbolic even when bound, also in a larger
     expression *)
  check_session_exact ~inputs:[ "let x = 5"; "diff(x^2, x) + x" ]
    ~expected:"5 + 2*x";
  (* let bindings must produce a number, so unbound variables there are
     still an error *)
  check_error ~input:"let y = q + 1"
    ~expected_error:(Calculator.Calc_error.Unbound_variable "q");

  (* exact arithmetic (PRD 2 edge-case table) *)
  check_exact ~input:"1/3 + 1/6" ~expected:"1/2";
  check_exact ~input:"0.1 + 0.2" ~expected:"3/10";
  check_exact ~input:"2^100" ~expected:"1267650600228229401496703205376";
  check_exact ~input:"2^-2" ~expected:"1/4";
  check_exact ~input:"(1/2)^3" ~expected:"1/8";
  check_exact ~input:"10/2" ~expected:"5";
  check_exact ~input:"floor(7/2)" ~expected:"3";
  check_exact ~input:"100000000000000000000 + 1"
    ~expected:"100000000000000000001";
  check_exact ~input:"0.25" ~expected:"1/4";
  check_exact ~input:"sqrt 9" ~expected:"3";
  check_exact ~input:"sqrt(9/4)" ~expected:"3/2";
  check_exact ~input:"1/3 * 3" ~expected:"1";
  check_error ~input:"1/0"
    ~expected_error:Calculator.Calc_error.Division_by_zero;
  check_error ~input:"1 / sin 0"
    ~expected_error:Calculator.Calc_error.Division_by_zero;
  check_error ~input:"1.2.3"
    ~expected_error:(Calculator.Calc_error.Invalid_number "1.2.3");
  check ~input:"sin(1) + 1" ~expected:(Float.sin 1.0 +. 1.0);
  check ~input:"2^0.5" ~expected:(Float.sqrt 2.0);
  check ~input:"sqrt(9) + cos(0)" ~expected:4.0;

  (* exact values flow through let bindings *)
  check_session_exact ~inputs:[ "let x = 1/3"; "x * 3" ] ~expected:"1";
  check_session_exact ~inputs:[ "let x = 1/3"; "x + x" ] ~expected:"2/3";

  if !failures > 0 then begin
    Stdlib.Printf.printf "%d test(s) failed\n" !failures;
    Stdlib.exit 1
  end
  else Stdlib.print_endline "all tests passed"
