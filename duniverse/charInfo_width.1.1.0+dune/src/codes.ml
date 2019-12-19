open CamomileLibraryDefault.Camomile

include USet

let add_ranges l s=
  List.fold_left (fun s (start, stop)-> add_range start stop s) s l

let tuple_to_range (start, stop)=
  let start= UChar.of_int start
  and stop= UChar.of_int stop in
  start, stop

let of_tuple_list l=
  let ranges= List.map tuple_to_range l in
  add_ranges ranges empty

