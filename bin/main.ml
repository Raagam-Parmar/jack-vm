open Lib
open Jackvm

exception VMError of string

(** [parse s] parses [s] into an AST *)
let parse (s : string) : (string, string) Vmast.Function.t list = 
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


(** [write_line oc s] writes the string [s] onto out-channel [oc]. *)
let write_line (oc : out_channel) (s : string) : unit = 
    Printf.fprintf oc "%s\n" s


(** [write_lines oc sl] writes [sl] string list onto out-channel [oc]. *)
let write_lines (oc : out_channel) (sl : string list) : unit = 
    List.iter (write_line oc) sl;
    flush oc


(** [basename f] returns name of file [f] without the file extension. *)
let basename (filename : string) : string = 
    let dot_index = String.index filename '.' in
    String.sub filename 0 dot_index


(** [change_extension f e] replaces extension of file [f] with [e]. *)
let change_extension (filename : string) (extension : string) : string = 
    basename filename ^ "." ^ extension


(* this function checks if all arguments are valid file paths. *)
let () = 
    for i = 1 to Array.length Sys.argv - 1 do
        let input_file = Sys.argv.(i) in
        if not( Sys.file_exists input_file) then
            raise (VMError input_file)
    done

(** [file_list ()] returns a string which contains all the input arguments. *)
let rec file_list ?(argi=Array.length Sys.argv - 1) () : string list =
    if argi = 0 then [] else
    let input_file = Sys.argv.(argi) in
    input_file :: file_list ~argi:(argi - 1) ()


(** [get_vmast files] returns a list of tuples, where second element is a function, and first element
is the file in which the function is declared. *)
let rec get_vmast (files : string list) : (string * (string, string) Vmast.Function.t) list = 
    match files with
    | [] -> []
    | head :: tail ->
        let in_ch = open_in head in 
        let functions = in_ch |> read_file |> parse in
        List.map ((fun s f -> (s, f)) head) functions @ get_vmast tail


(** [fun_encode f] takes a list from [get_vmast] and returns the ASM instructions by combining the
output of the individual ASM instructions from each function. *)
let rec func_encode (s : (string * (string, string) Vmast.Function.t) list) : string Ast.instruction list =
    match s with
    | [] -> []
    | head :: tail -> Encode.Function.encode (fst head) (snd head) @ func_encode tail


(** [main_encode s] calls the [func_encode f] and adds Bootstrap code in the beginning. *)
let main_encode (s : (string * (string, string) Vmast.Function.t) list) : string Ast.instruction list =
    Encode.Bootstrap.v @ 
    func_encode s


(* main program *)
let () = 
    let output_file = change_extension Sys.argv.(1) "hack" in
    let out_ch = open_out output_file in
    file_list () |> get_vmast |> main_encode |> Machine.Program.v |> write_lines out_ch
