let locales =
  Sys.readdir "."
  |> Array.to_list
  |> List.filter (fun f -> Filename.check_suffix f ".txt")
  |> List.map Filename.chop_extension
  |> List.sort compare

let file locale = Printf.sprintf "(%s.mar as locales/%s.mar)" locale locale

let rule locale = Printf.sprintf {|
(rule
 ((targets (%s.mar))
  (deps    (%s.txt (alias database)))
  (action  (chdir .. (run tools/camomilelocaledef.exe --file ${<} locales)))))
|} locale locale

let () = Printf.printf {|

(install
 ((section share)
  (files (
    %s
    ))))

(alias
 ((name database)
  (deps ((glob_files ../database/*.mar)))))

%s
|} (String.concat "\n" (List.map file locales))
    (String.concat "\n" (List.map rule locales))
