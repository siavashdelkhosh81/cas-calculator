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

(* Check that [input] fails with [Unbound_variable] on an empty environment. *)
let check_unbound ~input =
  let env = Map.empty (module String) in
  match Calculator.Eval.evaluate ~env ~input with
  | Error (Calculator.Calc_error.Unbound_variable _) -> ()
  | Ok (text, _) ->
      Stdlib.Printf.printf "FAIL: %s = %s, expected unbound-variable error\n"
        input text;
      Int.incr failures
  | Error _ ->
      Stdlib.Printf.printf
        "FAIL: %s returned the wrong error, expected unbound-variable\n" input;
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
  check_unbound ~input:"x + 1";

  if !failures > 0 then begin
    Stdlib.Printf.printf "%d test(s) failed\n" !failures;
    Stdlib.exit 1
  end
  else Stdlib.print_endline "all tests passed"
