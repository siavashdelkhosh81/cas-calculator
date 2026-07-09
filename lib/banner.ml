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

(* Render the full banner with the gradient rotated by [offset] rows.
   Offset 0 is the resting state; other offsets are animation frames. *)
let render ~offset =
  let b = Buffer.create 1024 in
  let add = Buffer.add_string b in
  let last = Array.length gradient - 1 in
  let shade i = fg gradient.((i + offset) % Array.length gradient) in
  add "\n";
  (* Top border in the brightest shade; it darkens row by row from here. *)
  add (Printf.sprintf "%s%s╔%s╗%s\n" bold (shade 0) (bar "═") reset);
  Array.iteri logo ~f:(fun i line ->
      let shade = shade i in
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
  add (Printf.sprintf "%s%s╚%s╝%s\n" bold (shade last) (bar "═") reset);
  Buffer.contents b

let print_flush s =
  Stdio.print_string s;
  Stdio.Out_channel.flush Stdio.stdout

let print () = print_flush (render ~offset:0)

(* Shimmer: redraw the banner with the gradient rotated one step per frame,
   walking the offset down to 0 so the last frame is the resting banner.
   Skipped when stdout is not a tty (piped output gets the static banner). *)
let animate () =
  if not (Unix.isatty Unix.stdout) then print ()
  else begin
    let hide_cursor = esc ^ "?25l" and show_cursor = esc ^ "?25h" in
    let steps = Array.length gradient in
    let frames = 3 * steps in
    (* Make Ctrl-C raise so [protect] can restore the cursor first. *)
    Stdlib.Sys.catch_break true;
    Exn.protect
      ~f:(fun () ->
        print_flush hide_cursor;
        for frame = frames downto 0 do
          let s = render ~offset:(frame % steps) in
          print_flush s;
          if frame > 0 then begin
            Unix.sleepf 0.06;
            (* Cursor back to the top of the banner to draw the next frame. *)
            let lines = String.count s ~f:(Char.equal '\n') in
            print_flush (Printf.sprintf "%s%dA" esc lines)
          end
        done)
      ~finally:(fun () ->
        print_flush show_cursor;
        Stdlib.Sys.catch_break false)
  end

let prompt () =
  Stdio.printf "\n%s▸%s " (bold ^ accent) reset;
  Stdio.Out_channel.flush Stdio.stdout
