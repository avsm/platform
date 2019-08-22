let ask f =
  App_log.question (fun l -> f (fun ?header ?tags fmt -> l ?header ?tags (fmt ^^ " [Y/n]")))

let confirm ~question ~yes =
  let rec loop () =
    ask question;
    match String.lowercase_ascii (read_line ()) with
    | "" | "y" | "yes" -> true
    | "n" | "no" -> false
    | _ ->
        App_log.unhappy
          (fun l ->
             l "Please answer with \"y\" for yes, \"n\" for no or just hit enter for the default");
        loop ()
  in
  if yes then true else loop ()

let confirm_or_abort ~question ~yes =
  if confirm ~question ~yes then
    Ok ()
  else
    Error (`Msg "Aborting on user demand")
