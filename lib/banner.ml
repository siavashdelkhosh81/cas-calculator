open Base

(* ANSI escape helpers. Industrial calculator startup banner. *)

let esc = "\027["
let reset = esc ^ "0m"
let bold = esc ^ "1m"
let dim = esc ^ "2m"

(* 256-colour foreground. *)
let fg n = Printf.sprintf "%s38;5;%dm" esc n

(* Electric-blue gradient, ice -> deep, for the logo rows. The frame fades
   with it: each border row is drawn in the same colour as its content. *)
let gradient = [| 51; 45; 39; 33; 27; 21 |]
let accent = fg 45
let deep = fg 27

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
  String.iter s ~f:(fun c -> if Char.to_int c land 0xC0 <> 0x80 then Int.incr n);
  !n

let inner = display_width logo.(0) + 2 (* one space padding each side *)

let bar ch = String.concat ~sep:"" (List.init inner ~f:(fun _ -> ch))

(* Emit a content row: [rendered] is the coloured text, [visible] its column
   count, [edge] the colour of the side walls. Pads on the right so the
   closing border lines up. *)
let row buf ~edge ~visible rendered =
  let pad = inner - visible in
  let pad = if pad < 0 then 0 else pad in
  Buffer.add_string buf
    (Printf.sprintf "%s║%s%s%s%s║%s\n" (bold ^ edge) reset rendered
       (String.make pad ' ') (bold ^ edge) reset)

let print () =
  let b = Buffer.create 1024 in
  let add = Buffer.add_string b in
  let last = Array.length gradient - 1 in
  add "\n";
  (* Top border in the brightest shade; it darkens row by row from here. *)
  add (Printf.sprintf "%s%s╔%s╗%s\n" bold (fg gradient.(0)) (bar "═") reset);
  Array.iteri logo ~f:(fun i line ->
      let shade = fg gradient.(i) in
      let rendered = Printf.sprintf " %s%s %s" shade line reset in
      row b ~edge:shade ~visible:(display_width line + 2) rendered);
  add (Printf.sprintf "%s%s╠%s╣%s\n" bold deep (bar "═") reset);
  (* Status lines. *)
  let l1_plain = "  INDUSTRIAL CALCULATOR · v1.0.0 · arbitrary precision core" in
  let l1 =
    Printf.sprintf "  %sINDUSTRIAL CALCULATOR%s %s· v1.0.0 · arbitrary precision core%s"
      (bold ^ accent) reset dim reset
  in
  row b ~edge:deep ~visible:(display_width l1_plain) l1;
  let l2_plain = "  type an expression — /q to quit, /help for help" in
  let l2 =
    Printf.sprintf "  %stype an expression — %s/q%s%s to quit, %s/help%s%s for help%s" dim
      (accent ^ bold) reset dim (accent ^ bold) reset dim reset
  in
  row b ~edge:deep ~visible:(display_width l2_plain) l2;
  add (Printf.sprintf "%s%s╚%s╝%s\n" bold (fg gradient.(last)) (bar "═") reset);
  Stdio.print_string (Buffer.contents b);
  Stdio.Out_channel.flush Stdio.stdout

let prompt () =
  Stdio.printf "\n%s▸%s " (bold ^ accent) reset;
  Stdio.Out_channel.flush Stdio.stdout
