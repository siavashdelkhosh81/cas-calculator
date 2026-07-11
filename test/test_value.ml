open Base
module Value = Calculator.Value

let failures = ref 0

let fail label message =
  Stdlib.Printf.printf "FAIL: %s: %s\n" label message;
  Int.incr failures

(* Run one test case, catching unexpected exceptions so a broken case does
   not abort the whole suite. *)
let run label (f : unit -> unit) =
  try f () with
  | Calculator.Calc_error.Calc_error err ->
      fail label
        ("unexpected calc error: " ^ Calculator.Calc_error.to_string err)
  | exn -> fail label ("unexpected exception: " ^ Exn.to_string exn)

(* Check that [make ()] is [Exact] and equals the fraction in [expected]
   (a string like "1/2" or "4", parsed with Q.of_string). *)
let expect_exact ~label (make : unit -> Value.t) ~expected =
  run label (fun () ->
      match make () with
      | Value.Exact q ->
          if not (Q.equal q (Q.of_string expected)) then
            fail label
              (Stdlib.Printf.sprintf "got exact %s, expected exact %s"
                 (Q.to_string q) expected)
      | Value.Approx f ->
          fail label
            (Stdlib.Printf.sprintf "got approx %g, expected exact %s" f
               expected))

(* Check that [make ()] is [Approx] and close to [expected]. *)
let expect_approx ~label (make : unit -> Value.t) ~expected =
  run label (fun () ->
      match make () with
      | Value.Approx f ->
          if Float.(abs (f - expected) > 1e-6 *. (1.0 +. abs expected)) then
            fail label
              (Stdlib.Printf.sprintf "got approx %.10g, expected approx %.10g"
                 f expected)
      | Value.Exact q ->
          fail label
            (Stdlib.Printf.sprintf "got exact %s, expected approx %g"
               (Q.to_string q) expected))

(* Check that [make ()] is [Approx infinity] (overflow fallback path). *)
let expect_approx_inf ~label (make : unit -> Value.t) =
  run label (fun () ->
      match make () with
      | Value.Approx f when Float.is_inf f -> ()
      | Value.Approx f ->
          fail label (Stdlib.Printf.sprintf "got approx %g, expected inf" f)
      | Value.Exact q ->
          fail label
            (Stdlib.Printf.sprintf "got exact %s, expected approx inf"
               (Q.to_string q)))

(* Check that [make ()] raises the given calculator error. *)
let expect_error ~label (make : unit -> Value.t) ~expected_error =
  run label (fun () ->
      match make () with
      | exception Calculator.Calc_error.Calc_error err ->
          if not (Poly.equal err expected_error) then
            fail label
              (Stdlib.Printf.sprintf "got error %s, expected %s"
                 (Calculator.Calc_error.to_string err)
                 (Calculator.Calc_error.to_string expected_error))
      | value ->
          fail label
            ("got value " ^ Value.to_string value ^ ", expected an error"))

(* Check [Value.to_string] output verbatim. *)
let expect_string ~label (make : unit -> Value.t) ~expected =
  run label (fun () ->
      let text = Value.to_string (make ()) in
      if not (String.equal text expected) then
        fail label
          (Stdlib.Printf.sprintf "printed %s, expected %s" text expected))

let q text = Q.of_string text
let exact text = Value.Exact (q text)

let () =
  (* exact arithmetic *)
  expect_exact ~label:"1/3 + 1/6"
    (fun () -> Value.add (exact "1/3") (exact "1/6"))
    ~expected:"1/2";
  expect_exact ~label:"1/2 - 1/3"
    (fun () -> Value.sub (exact "1/2") (exact "1/3"))
    ~expected:"1/6";
  expect_exact ~label:"2/3 * 3/4"
    (fun () -> Value.mul (exact "2/3") (exact "3/4"))
    ~expected:"1/2";
  expect_exact ~label:"(1/2) / (1/3)"
    (fun () -> Value.div (exact "1/2") (exact "1/3"))
    ~expected:"3/2";
  expect_exact ~label:"1/10 + 2/10"
    (fun () -> Value.add (exact "1/10") (exact "2/10"))
    ~expected:"3/10";
  expect_exact ~label:"bignum + 1"
    (fun () -> Value.add (exact "100000000000000000000") (exact "1"))
    ~expected:"100000000000000000001";
  expect_exact ~label:"neg 1/2"
    (fun () -> Value.neg (exact "1/2"))
    ~expected:"-1/2";
  expect_approx ~label:"neg approx"
    (fun () -> Value.neg (Value.Approx 1.5))
    ~expected:(-1.5);

  (* contagion: any Approx operand makes the result Approx *)
  expect_approx ~label:"exact + approx"
    (fun () -> Value.add (exact "1") (Value.Approx 1.0))
    ~expected:2.0;
  expect_approx ~label:"approx * exact"
    (fun () -> Value.mul (Value.Approx 2.0) (exact "1/2"))
    ~expected:1.0;
  expect_approx ~label:"approx / exact"
    (fun () -> Value.div (Value.Approx 1.0) (exact "4"))
    ~expected:0.25;

  (* division by zero: any zero denominator is an error *)
  expect_error ~label:"1 / exact 0"
    (fun () -> Value.div (exact "1") (exact "0"))
    ~expected_error:Calculator.Calc_error.Division_by_zero;
  expect_error ~label:"1 / approx 0"
    (fun () -> Value.div (exact "1") (Value.Approx 0.0))
    ~expected_error:Calculator.Calc_error.Division_by_zero;
  expect_error ~label:"approx 1 / exact 0"
    (fun () -> Value.div (Value.Approx 1.0) (exact "0"))
    ~expected_error:Calculator.Calc_error.Division_by_zero;

  (* pow: exact base with integer exact exponent stays exact *)
  expect_string ~label:"2^100"
    (fun () -> Value.pow (exact "2") (exact "100"))
    ~expected:"1267650600228229401496703205376";
  expect_exact ~label:"2^-2"
    (fun () -> Value.pow (exact "2") (exact "-2"))
    ~expected:"1/4";
  expect_exact ~label:"(1/2)^3"
    (fun () -> Value.pow (exact "1/2") (exact "3"))
    ~expected:"1/8";
  expect_exact ~label:"(1/2)^-3"
    (fun () -> Value.pow (exact "1/2") (exact "-3"))
    ~expected:"8";
  expect_exact ~label:"0^0"
    (fun () -> Value.pow (exact "0") (exact "0"))
    ~expected:"1";
  expect_approx ~label:"2^(1/2)"
    (fun () -> Value.pow (exact "2") (exact "1/2"))
    ~expected:(Float.sqrt 2.0);
  expect_approx ~label:"approx base pow"
    (fun () -> Value.pow (Value.Approx 2.0) (exact "2"))
    ~expected:4.0;
  expect_error ~label:"0^-2"
    (fun () -> Value.pow (exact "0") (exact "-2"))
    ~expected_error:Calculator.Calc_error.Division_by_zero;
  (* an exponent past the exactness cap falls back to floats instead of
     allocating a gigantic bignum *)
  expect_approx_inf ~label:"2^(2^30) overflows to float" (fun () ->
      Value.pow (exact "2") (exact "1073741824"));

  (* exact-preserving functions *)
  expect_exact ~label:"abs exact"
    (fun () -> Value.apply_float_fn "abs" (exact "-5"))
    ~expected:"5";
  expect_approx ~label:"abs approx"
    (fun () -> Value.apply_float_fn "abs" (Value.Approx (-5.0)))
    ~expected:5.0;
  expect_exact ~label:"floor 7/2"
    (fun () -> Value.apply_float_fn "floor" (exact "7/2"))
    ~expected:"3";
  expect_exact ~label:"floor -7/2"
    (fun () -> Value.apply_float_fn "floor" (exact "-7/2"))
    ~expected:"-4";
  expect_exact ~label:"ceil 7/2"
    (fun () -> Value.apply_float_fn "ceil" (exact "7/2"))
    ~expected:"4";
  expect_exact ~label:"round 5/2"
    (fun () -> Value.apply_float_fn "round" (exact "5/2"))
    ~expected:"3";
  expect_exact ~label:"round -5/2"
    (fun () -> Value.apply_float_fn "round" (exact "-5/2"))
    ~expected:"-3";
  expect_exact ~label:"round 7/3"
    (fun () -> Value.apply_float_fn "round" (exact "7/3"))
    ~expected:"2";
  expect_exact ~label:"sqrt 9"
    (fun () -> Value.apply_float_fn "sqrt" (exact "9"))
    ~expected:"3";
  expect_exact ~label:"sqrt 9/4"
    (fun () -> Value.apply_float_fn "sqrt" (exact "9/4"))
    ~expected:"3/2";
  expect_approx ~label:"sqrt 2 is approx"
    (fun () -> Value.apply_float_fn "sqrt" (exact "2"))
    ~expected:(Float.sqrt 2.0);
  expect_approx ~label:"sqrt approx stays approx"
    (fun () -> Value.apply_float_fn "sqrt" (Value.Approx 9.0))
    ~expected:3.0;

  (* float-only functions convert then wrap in Approx *)
  expect_approx ~label:"sin exact 1"
    (fun () -> Value.apply_float_fn "sin" (exact "1"))
    ~expected:(Float.sin 1.0);
  expect_approx ~label:"round approx 2.5"
    (fun () -> Value.apply_float_fn "round" (Value.Approx 2.5))
    ~expected:3.0;
  expect_error ~label:"unknown function"
    (fun () -> Value.apply_float_fn "frobnicate" (exact "1"))
    ~expected_error:(Calculator.Calc_error.Unknown_function "frobnicate");

  (* printing *)
  expect_string ~label:"print integer" (fun () -> exact "4") ~expected:"4";
  expect_string ~label:"print reduced integer"
    (fun () -> exact "10/2")
    ~expected:"5";
  expect_string ~label:"print fraction" (fun () -> exact "1/2") ~expected:"1/2";
  expect_string ~label:"print negative fraction"
    (fun () -> exact "-3/2")
    ~expected:"-3/2";
  expect_string ~label:"print approx"
    (fun () -> Value.Approx 0.5)
    ~expected:"0.5";
  expect_string ~label:"print approx %g"
    (fun () -> Value.Approx (Float.sin 1.0))
    ~expected:"0.841471";

  (* to_float *)
  run "to_float 1/2" (fun () ->
      if Float.(Value.to_float (exact "1/2") <> 0.5) then
        fail "to_float 1/2" "expected 0.5");

  if !failures > 0 then begin
    Stdlib.Printf.printf "%d test(s) failed\n" !failures;
    Stdlib.exit 1
  end
  else Stdlib.print_endline "all value tests passed"
