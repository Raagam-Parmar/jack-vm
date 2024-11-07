open Lib
(* open Lib.Ast *)

(** [parse s] parses [s] into an AST *)
let parse (s : string) : string Ast.program = 
        let lexbuf = Lexing.from_string s in
        let ast = Parser.main Lexer.read lexbuf in
        ast


(** [read_file ic] returns a string with contents of the [ic] in channel and closes it *)
let read_file (ic : in_channel) : string =
        let rec aux acc =
                try
                let ch = input_char ic in
                aux (acc ^ String.make 1 ch)
                with End_of_file -> 
                        close_in_noerr ic;
                        acc
        in
        aux ""


(** [write_line oc s] writes the string [s] into output buffer [oc] 
and flushes the buffer, it does not close the out_channel to enable further 
write actions *)
let write_line (oc : out_channel) (s : string) : unit = 
        Printf.fprintf oc "%s\n" s;
        flush oc


(**  [make_in_channel file_path] takes a full path [file_path] 
to the input file and returns an in_channel for the path if the
path exists, otherwise returns an in_channel for the default 
file named `input.asm' in the dune project directory *)
let make_in_channel (file_path : string) : in_channel = 
        let in_ch = 
        try
                open_in file_path
        with Sys_error x ->
                (
                        print_endline x;
                        print_endline "input file set to: `input.asm'\n";
                        open_in "input.asm"
                )
        in in_ch


(** The following functions create an in_channel [in_ch] to read form *)
let () = print_endline "Full path to your file\n(default is `input.asm' in dune project directory:)\n"
let input_file = read_line ()
let in_ch = make_in_channel input_file


(**  The following functions parse the input_file form the in_channel [in_ic]
and return the AST [program_ast] for the file *)
let program_string = read_file in_ch
let program_ast = parse program_string


(**  The following functions create the symbol table [sym_tbl]
and label table [lbl_tbl] for the AST [program_ast] *)
let h = Hashtbl.create 10

let () = Machine.Program.populate h program_ast

(** The following function creates the machine instructions for as a list [binary_list] *)
let binary_list = Machine.Program.encode_pretty_string h program_ast


(** The following functions write each string in [binary_list]
into the [output_file] and closes the out_channel [out_ch] *)
let output_file = "output.txt"
let out_ch = open_out output_file
let () = List.iter (write_line out_ch) binary_list
let () = close_out_noerr out_ch