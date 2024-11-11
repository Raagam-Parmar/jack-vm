open Lib

(** [parse s] parses [s] into an AST *)
let parse (s : string) : string Ast.program = 
        let lexbuf = Lexing.from_string s in
        let ast = Parser.main Lexer.read lexbuf in
        ast


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


(** [write_line oc s] writes the string [s] into output-buffer of our-channel [oc] 
and flushes the buffer, but doesn't close it does not close it to enable further 
write actions *)
let write_line (oc : out_channel) (s : string) : unit = 
        Printf.fprintf oc "%s\n" s;
        flush oc


(** [make_in_channel fp] returns in-channel for [fp] if it exists, otherwise defaults
        to a file called `input.asm' in the dune directory and returns its in-channel. *)
let make_in_channel (file_path : string) : in_channel = 
        try open_in file_path
        with Sys_error x ->
                (
                print_endline x;
                print_endline "Error opening file. Defaulting to: `input.asm'\n";
                open_in "input.asm"
                )


let () = 
        print_endline "Full path to your file (default is `input.asm'): ";
        let input_file = read_line () in 
        let in_ch = make_in_channel input_file in 

        let program_string = read_file in_ch in
        let program_ast = parse program_string in

        let h = Hashtbl.create 10 in
        Machine.Program.populate h program_ast;

        let binary_list = Machine.Program.encode_pretty_string h program_ast in
        let output_file = "output.hack" in
        let out_ch = open_out output_file in
        List.iter (write_line out_ch) binary_list;

        close_out_noerr out_ch
        