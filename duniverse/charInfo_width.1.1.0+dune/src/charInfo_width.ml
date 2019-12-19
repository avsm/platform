open CamomileLibraryDefault.Camomile
open Result

module Cfg = Cfg

let width ?(cfg: Cfg.t option= None) uchar=
  let ucs= UChar.int_of uchar in
  if ucs >= 0x20 && ucs < 0x7f then
    1 (* ascii printing char *)
  else if ucs = 0 then
    0
  else if ucs < 0x20 || ucs >= 0x7f && ucs < 0xa0 then
    -1 (* control characters *)
  else if Combining.(Codes.mem uchar set) then
    0
  else if Fullwidth.is_fullwidth ucs then
    2
  else
    match cfg with
    | Some widthTable-> 
      if Codes.mem uchar widthTable.unprintable then
        -1
      else if Codes.mem uchar widthTable.combining then
        0
      else if Codes.mem uchar widthTable.w2 then
        2
      else if Codes.mem uchar widthTable.w3 then
        3
      else if Codes.mem uchar widthTable.w4 then
        4
      else if Codes.mem uchar widthTable.w5 then
        5
      else if Codes.mem uchar widthTable.w6 then
        6
      else
        1
    | None-> 1

let width_exn ?(cfg: Cfg.t option= None) uchar=
  let w= width ~cfg uchar in
  if w = -1 then
    raise (Failure "unprintable character")
  else
    w

module type UnicodeString_mini = sig
  type t
  val get : t -> int -> UChar.t
  val length : t -> int
end

module String(US:UnicodeString_mini) = struct
  let width ?(cfg: Cfg.t option= None) (us: US.t)=
    let length= US.length us in
    let rec aux ws i=
      if i < length then
        let wc= width ~cfg (US.get us i) in
        if wc = -1 then
          Error i
        else
          aux (ws+wc) (i+1)
      else
        Ok ws
    in
    aux 0 0
end

