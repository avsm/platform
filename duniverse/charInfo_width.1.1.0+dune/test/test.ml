open CamomileLibraryDefault
open Result
open Printf

let msg= "a͜b͡c字符宽度"

let%expect_test "width"=
  let length= Camomile.UTF8.length msg in
  for i= 0 to length - 1 do
    let c= Camomile.UTF8.get msg i in
    let len= CharInfo_width.width c in
    printf " %d" len
  done;
  [%expect "1 0 1 0 1 2 2 2 2"]

module UTF8 = CharInfo_width.String(Camomile.UTF8)

let%test "string: width"=
  UTF8.width msg = Ok (1 + 0 + 1 + 0 + 1 + 2 + 2 + 2 + 2)

let%test "string: error position"=
  UTF8.width "ab\ncd" = Error 2

