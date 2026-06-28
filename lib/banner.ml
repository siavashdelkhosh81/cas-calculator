(* ANSI escape helpers. Industrial calculator startup banner. *)

let esc = "\027["
let reset = esc ^ "0m"
let bold = esc ^ "1m"
let dim = esc ^ "2m"

(* 256-colour foreground. *)
let fg n = Printf.sprintf "%s38;5;%dm" esc n

(* Orange-amber gradient, light -> deep, for the logo rows. *)
let gradient = [| 222; 215; 214; 208; 202; 166 |]
let amber = fg 208
let rule = fg 166

(* Figlet-style block logo, one string per row. *)
let logo =
  [| " ██████╗ █████╗ ██╗      ██████╗██╗   ██╗██╗      █████╗ ████████╗ ██████╗ ██████╗ "
   ; "██╔════╝██╔══██╗██║     ██╔════╝██║   ██║██║     ██╔══██╗╚══██╔══╝██╔═══██╗██╔══██╗"
   ; "██║     ███████║██║     ██║     ██║   ██║██║     ███████║   ██║   ██║   ██║██████╔╝"
   ; "██║     ██╔══██║██║     ██║     ██║   ██║██║     ██╔══██║   ██║   ██║   ██║██╔══██╗"
   ; "╚██████╗██║  ██║███████╗╚██████╗╚██████╔╝███████╗██║  ██║   ██║   ╚██████╔╝██║  ██║"
   ; " ╚═════╝╚═╝  ╚═╝╚══════╝ ╚═════╝ ╚═════╝ ╚══════╝╚═╝  ╚═╝   ╚═╝    ╚═════╝ ╚═╝  ╚═╝"
  |]

(* Visible (display) width of a UTF-8 string: count code points, not bytes.
   Every glyph used here is single-width, so code points = columns. *)
let display_width s =
  let n = ref 0 in
  String.iter (fun c -> if Char.code c land 0xC0 <> 0x80 then incr n) s;
  !n

let inner = display_width logo.(0) + 2 (* one space padding each side *)

let bar ch = String.concat "" (List.init inner (fun _ -> ch))

(* Emit a content row: [rendered] is the coloured text, [visible] its column
   count. Pads on the right so the closing border lines up. *)
let row buf ~visible rendered =
  let pad = inner - visible in
  let pad = if pad < 0 then 0 else pad in
  Buffer.add_string buf
    (Printf.sprintf "%s║%s%s%s%s║%s\n" (bold ^ rule) reset rendered (String.make pad ' ')
       (bold ^ rule) reset)

let print () =
  let b = Buffer.create 1024 in
  let add = Buffer.add_string b in
  add "\n";
  add (Printf.sprintf "%s%s╔%s╗%s\n" bold rule (bar "═") reset);
  Array.iteri
    (fun i line ->
      let rendered = Printf.sprintf " %s%s %s" (fg gradient.(i)) line reset in
      row b ~visible:(display_width line + 2) rendered)
    logo;
  add (Printf.sprintf "%s%s╠%s╣%s\n" bold rule (bar "═") reset);
  (* Status lines. *)
  let l1_plain = "  INDUSTRIAL CALCULATOR · v1.0.0 · arbitrary precision core" in
  let l1 =
    Printf.sprintf "  %sINDUSTRIAL CALCULATOR%s %s· v1.0.0 · arbitrary precision core%s"
      (bold ^ amber) reset dim reset
  in
  row b ~visible:(display_width l1_plain) l1;
  let l2_plain = "  type an expression — /q to quit, /help for help" in
  let l2 =
    Printf.sprintf "  %stype an expression — %s/q%s%s to quit, %s/help%s%s for help%s" dim
      (amber ^ bold) reset dim (amber ^ bold) reset dim reset
  in
  row b ~visible:(display_width l2_plain) l2;
  add (Printf.sprintf "%s%s╚%s╝%s\n" bold rule (bar "═") reset);
  print_string (Buffer.contents b);
  flush stdout

let prompt () =
  Printf.printf "\n%s▸%s " (bold ^ amber) reset;
  flush stdout
