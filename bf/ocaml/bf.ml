open Str;;

exception Help;;

let out_start = "extern int putchar(int);\nextern char getchar();\n\nchar array[30000] = {0}; char *ptr = array;\nint main (int argc, char *argv[]) {";;

let (mod) x y = ((x mod y) + y) mod y;;

let read_file filename =
        let ch = open_in filename in
        let s  = really_input_string ch (in_channel_length ch) in
        close_in ch;
        s;;

let equiv = function
        '>' -> "++ptr;"
        | '<' -> "--ptr;"
        | '+' -> "++*ptr;"
        | '-' -> "--*ptr;"
        | '.' -> "putchar(*ptr);"
        | ',' -> "*ptr = getchar();"
        | '[' -> "while (*ptr) {"
        | ']' -> "}"
        | _ -> "";;

let translate ch =
        String.iter (fun x ->
                match equiv x with "" -> ()
                | s -> Printf.fprintf ch "      %s\n" s)
        ;;

let transpile input_file output_file =
        let oc = open_out output_file in
        Printf.fprintf oc "%s\n" out_start;
        read_file input_file |> translate oc;
        Printf.fprintf oc "%s" "}";
        close_out oc;;

let compile input_file =
        let output_file = Filename.remove_extension input_file
        in let temp_file = Filename.temp_file output_file ".c"
        in let command = Filename.quote_command "tcc" [temp_file; "-o"; output_file] in
        transpile input_file temp_file;
        (match Sys.command command with 0 -> () | n -> Printf.printf "Error code %d" n);
        Sys.remove temp_file;;

let interpret input_file =
        let content = read_file input_file
        and arr = Array.make 30_000 0
        and ptr = ref 0 and i = ref 0
        in let len = String.length content - 1
        in let rec aux () =
                if !i>=len then () else begin
                incr i;
                match content.[!i] with
                  '>' -> (incr ptr; aux())
                | '<' -> (decr ptr; aux())
                | '+' -> (arr.(!ptr) <- arr.(!ptr)+1; aux())
                | '-' -> (arr.(!ptr) <- arr.(!ptr)-1; aux())
                | '.' -> (arr.(!ptr) |> char_of_int |> print_char; aux())
                | ',' -> (arr.(!ptr) <- (input_char stdin |> int_of_char); aux())
                | '[' ->
                        if arr.(!ptr) = 0 then
                                let loop = ref 1 in
                                while (!loop > 0) do
                                        incr i;
                                        if content.[!i] = ']' then decr loop
                                        else if content.[!i] = '[' then incr loop
                                        else ();
                                done
                        else ();
                        aux ()
                | ']' ->
                        if arr.(!ptr) <> 0 then
                                let loop = ref 1 in
                                while (!loop > 0) do
                                        decr i;
                                        if content.[!i] = ']' then incr loop
                                        else if content.[!i] = '[' then decr loop
                                        else ();
                                done
                        else ();
                        aux ()
                | _ -> aux ()
                end
        in aux ();;

let () =
        let interpreter = ref false
        and transpiler = ref false in
        try
                for i = 1 to Array.length Sys.argv - 1 do
                        match Sys.argv.(i) with
                        "-i" | "--interpreter" -> interpreter := true
                        | "-t" | "--transpiler" -> transpiler := true
                        | "-h" | "--help" ->
                                        print_endline "usage: bf [-hit] file1 [file2..]\n  -i: interpret\n  -t: transpile (to C)"; raise Help
                        | file -> begin
                                        if !interpreter then (interpreter := false;
                                                interpret file;
                                                Printf.printf "\n%s - intepreted\n" file)
                                        else if !transpiler then (transpiler := false;
                                                Filename.remove_extension file ^ ".c" |>
                                                transpile file;
                                                Printf.printf "%s - transpiled\n" file)
                                        else (compile file; Printf.printf "%s - compiled\n" file)
                                end
                done
        with Help -> ()
;;
