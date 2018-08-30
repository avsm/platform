(* TEST
   * flat-float-array
   ** expect
*)

let rec x = [| x |]; 1.;;
[%%expect{|
Line _, characters 12-19:
  let rec x = [| x |]; 1.;;
              ^^^^^^^
Warning 10: this expression should have type unit.
Line _, characters 12-23:
  let rec x = [| x |]; 1.;;
              ^^^^^^^^^^^
Error: This kind of expression is not allowed as right-hand side of `let rec'
|}];;

let rec x = let u = [|y|] in 10. and y = 1.;;
[%%expect{|
Line _, characters 12-32:
  let rec x = let u = [|y|] in 10. and y = 1.;;
              ^^^^^^^^^^^^^^^^^^^^
Error: This kind of expression is not allowed as right-hand side of `let rec'
|}];;
