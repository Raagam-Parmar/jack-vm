open Lib

(** [parse s] parses [s] into an AST *)
let parse (s : string) : string Ast.program = 
    let lexbuf = Lexing.from_string s in
    let ast = Parser.main Lexer.read lexbuf in
    ast
    

(** [make_in_channel fp] returns in-channel for [fp] if it exists, otherwise defaults
        to a file called `input.asm' in the dune directory and returns its in-channel. *)
let make_in_channel (file_path : string) : in_channel option = 
    try Some (open_in file_path)
    with Sys_error _ ->
        Printf.eprintf "Error opening file.\n";
        None


(** [read_file ic] reads contents of in-channel [ic] as a string and closes it. *)
let read_file (ic : in_channel) : string =
    let buffer = Buffer.create 4096 in
    try
        while true do
        Buffer.add_channel buffer ic 4096
        done;
        ""
    with End_of_file ->
        close_in_noerr ic;
        Buffer.contents buffer


(** [write_line oc s] writes the string [s] onto out-channel [oc]. *)
let write_line (oc : out_channel) (s : string) : unit = 
    Printf.fprintf oc "%s\n" s


(** [write_lines oc sl] writes [sl] string list onto out-channel [oc]. *)
let write_lines (oc : out_channel) (sl : string list) : unit = 
    List.iter (write_line oc) sl;
    flush oc


(** [change_extension f e] replaces extension of file [f] with [e]. *)
let change_extension (filename : string) (extension : string) : string = 
    let dot_index = String.index filename '.' in
    let basename = String.sub filename 0 dot_index in
    basename ^ "." ^ extension


(* main program *)
let () =
    for i = 1 to Array.length Sys.argv - 1 do
        let input_file = Sys.argv.(i) in
        let output_file = change_extension input_file "hack" in

        match make_in_channel input_file with
        | Some in_ch -> let out_channel = open_out output_file in
                        in_ch |> read_file |> parse |> Machine.Program.v |> write_lines out_channel;
        | None       -> ()
    done