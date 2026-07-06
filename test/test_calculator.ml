open Base

let failures = ref 0

(* Check that [input] evaluates to [expected]. The tolerance is loose because
   [evaluate] renders results with %g, which keeps 6 significant digits. *)
let check ~input ~expected =
  match Calculator.Eval.evaluate input with
  | Ok text ->
      let value = Float.of_string text in
      if Float.(abs (value - expected) > 1e-4) then begin
        Stdlib.Printf.printf "FAIL: %s = %s, expected %g\n" input text expected;
        Int.incr failures
      end
  | Error _ ->
      Stdlib.Printf.printf "FAIL: %s returned an error, expected %g\n" input
        expected;
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

  if !failures > 0 then begin
    Stdlib.Printf.printf "%d test(s) failed\n" !failures;
    Stdlib.exit 1
  end
  else Stdlib.print_endline "all tests passed"
