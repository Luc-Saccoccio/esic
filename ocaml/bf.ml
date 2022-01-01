open Str;;

let out_start = "extern int putchar(int);\nextern char getchar();\n\nchar array[30000] = {0}; char *ptr = array;\nint main (int argc, char *argv[]) {"

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
        and c = ref 0 and i = ref 0
        and correct x = x := !x mod 30_000
        in let len = String.length content - 1
        in let find_next s =
                let rec aux j co =
                        if j = len then failwith "Could not find matching ]"
                        else if content.[j] = '[' then aux (j+1) (co+1)
                        else if content.[j] = ']'
                                then if co = 0 then i := j else aux (j+1) (co-1)
                        else aux (j+1) co
                in aux s 0
        and find_prev s =
                let rec aux j co =
                        if j = 0 then failwith "Could not find matching ["
                        else if content.[j] = ']' then aux (j-1) (co+1)
                        else if content.[j] = '['
                                then if co = 0 then i := j else aux (j-1) (co-1)
                        else aux (j-1) co
                in aux s 0
        in let rec aux () =
                if !i>=len then () else begin
                incr i;
                match content.[!i] with
                  '>' -> (incr c; correct c; aux())
                | '<' -> (decr c; correct c; aux())
                | '+' -> (arr.(!c) <- arr.(!c)+1; aux())
                | '-' -> (arr.(!c) <- arr.(!c)-1; aux())
                | '.' -> (arr.(!c) mod 255 |> char_of_int |> print_char; aux())
                | ',' -> (arr.(!c) <- (input_char stdin |> int_of_char); aux())
                | '[' ->
                        if arr.(!c) = 0 then
                                find_next (!i-1)
                        else ();
                        aux ()
                | ']' ->
                        if arr.(!c) <> 0 then
                                find_prev (!i-1)
                        else ();
                        aux ()
                | _ -> aux ()
                end
        in aux ();;

let () =
        let interpreter = ref false
        and transpiler = ref false in
        for i = 1 to Array.length Sys.argv - 1 do
                match Sys.argv.(i) with
                "-i" | "--interpreter" -> interpreter := true
                | "-t" | "--transpiler" -> transpiler := true
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
        done;;
