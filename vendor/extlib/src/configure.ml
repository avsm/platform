let show_bytes s =
  if Sys.command (Printf.sprintf "ocamlfind query -format %s bytes" (Filename.quote s)) <> 0 then
    print_endline "WITH_DUMMY"

let define v =
  print_endline "-D";
  print_endline (v ^ " ")

let () =
  match Sys.argv with
  | [|_;"-cppo-args"|] ->
    if Sys.ocaml_version >= "4.00.0" then define "OCAML4";
    if Sys.ocaml_version >= "4.02.0" then define "OCAML4_02";
    if Sys.ocaml_version >= "4.03.0" then define "OCAML4_03";
    if Sys.ocaml_version >= "4.04.0" then define "OCAML4_04";
    if Sys.ocaml_version >= "4.05.0" then define "OCAML4_05";
    if Sys.ocaml_version >= "4.06.0" then define "OCAML4_06";
    print_endline "-D";
    show_bytes "WITH_BYTES";
    exit 0
  | [|_;"-compile-args"|] ->
    if Sys.ocaml_version >= "4.00.0" then print_endline "-bin-annot";
    show_bytes "-package bytes";
    exit 0
  | _ -> failwith "not gonna happen"
