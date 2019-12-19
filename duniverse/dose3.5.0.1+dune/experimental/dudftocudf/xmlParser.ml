
module L = Xml.LazyList

type stack =
  | Element of (Xml.xml * stack)
  | Start of (string * (string * string) list * stack)
  | String of (string * t * stack)
  | Empty
and t =
  | PCData
  | CData

let stack = ref Empty

type buf =
  { mutable buffer : string;
    mutable pos : int;
    mutable cdata : bool ;
    mutable length : int }

let txt = { buffer = String.create 1024; pos = 0; cdata = false ; length = 1024 }

let resize txt n  =
  let new_len = txt.length * 2 + n in
  let new_buf = String.create new_len in
  String.unsafe_blit txt.buffer 0 new_buf 0 txt.pos;
  txt.buffer <- new_buf;
  txt.length <- new_len
;;

let add_string txt s =
  let len = String.length s in
  let new_pos = txt.pos + len in
  if new_pos > txt.length then resize txt len;
  String.unsafe_blit s 0 txt.buffer txt.pos len;
  txt.pos <- new_pos
;;

let rec only_ws s i =
  (i = 0) ||
  (let i = pred i in match (String.unsafe_get s i) with
     | ' ' | '\t' | '\n' | '\r' -> only_ws s i
     | _ -> false)
;;

let pcdata buff = (Xml.PCData buff)
let cdata buff = (Xml.CData buff)

let rec create_elt acc = function
  | String (s,CData, st) -> create_elt (L.push (cdata s) acc) st
  | String (s,PCData, st) -> create_elt (L.push (pcdata s) acc) st
  | Element (x,st) -> create_elt (L.push x acc) st
  | Start (tag,attrlst,st) -> stack := Element(Xml.Element(tag,attrlst,acc),st)
  | Empty -> assert false

let start_cdata_handler () = txt.cdata <- true ;;

let start_element_handler tag attrlst =
  if not (only_ws txt.buffer txt.pos) then begin
    let str = String.sub txt.buffer 0 txt.pos in
    if txt.cdata then
      stack := String (str, CData, !stack)
    else
      stack := String (str, PCData, !stack)
  end
  ;
  txt.pos <- 0;
  txt.cdata <- false;
  stack := Start (tag,attrlst,!stack)
;;

let end_element_handler _ =
  let acc =
    if only_ws txt.buffer txt.pos then L.empty
    else
      let str = String.sub txt.buffer 0 txt.pos in
      if txt.cdata then L.one (cdata str)
      else L.one (pcdata str)
  in
  txt.pos <- 0;
  txt.cdata <- false;
  create_elt acc !stack
;;

let character_data_handler = add_string txt ;;

let parser_aux f =
  let p = Expat.parser_create None in
  Expat.set_start_element_handler p start_element_handler ;
  Expat.set_end_element_handler p end_element_handler ;

  Expat.set_start_cdata_handler p start_cdata_handler ;

  Expat.set_character_data_handler p character_data_handler ;
  ignore (Expat.set_param_entity_parsing p Expat.ALWAYS);
  f p;
  match !stack with
  |Element (x,Empty) -> (stack := Empty; x)
  | _ -> assert false
;;

let bar = Common.Util.Progress.create "Dudftocidf.XmlParser"

let parse_str str =
  let f p = 
    Expat.parse p str;
    Expat.final p;
  in
  parser_aux f
;;

let parse_ch ch =
  let f p =
    try while true do 
      Expat.parse p (IO.nread ch 10240)
    done with IO.No_more_input -> Expat.final p ;
  in
  let r = parser_aux f in
  Common.Util.Progress.reset bar;
  r
;;
