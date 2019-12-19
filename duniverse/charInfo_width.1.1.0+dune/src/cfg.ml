open Result

module MiniParsec = struct
  open Printf

  type pos= {
    cnum: int;
    line: int;
    bol: int;
  }

  type state= {
    data: string;
    maxlen: int;
    pos: pos
  }

  let initState data= {
    data;
    maxlen= String.length data;
    pos= {
      cnum= 0;
      line= 1;
      bol= 0;
      };
  }

  type error= pos * string

  type 'a reply= (('a * state), error) result

  type 'a parser= state -> 'a reply
  type 'a t= 'a parser


  let string_of_pos pos= sprintf "line %d, characters %d"
    pos.line (pos.cnum - pos.bol)

  let string_of_pos_full pos= sprintf "offset %d, line %d, characters %d"
    pos.cnum pos.line (pos.cnum - pos.bol)

  (* parser generator *)

  let any= fun state->
    let pos= state.pos in
    if pos.cnum < state.maxlen then
      let found= String.get state.data state.pos.cnum in
      let pos= { pos with cnum= pos.cnum + 1 } in
      (Ok (found, { state with pos }))
    else
      (Error (state.pos, "out of bounds"))

  let char c= fun state->
    let pos= state.pos in
    if pos.cnum < state.maxlen then
      let found= String.get state.data pos.cnum in
      if found = c then
        let pos= { pos with cnum= pos.cnum + 1 } in
        (Ok (found, { state with pos }))
      else
        Error (
          state.pos,
          sprintf "\"%c\" expected but \"%c\" found" c found)
    else
      (Error (state.pos, "out of bounds"))

  let string str= fun state->
    let pos= state.pos in
    let len= String.length str in
    if state.maxlen - pos.cnum >= len then
      let found= String.sub state.data pos.cnum len in
      if found = str then
        let pos= { pos with cnum= pos.cnum + len } in
        (Ok (found, { state with pos }))
      else
        Error (
          state.pos,
          sprintf "\"%s\" expected but \"%s\" found" str found)
    else
      (Error (state.pos, "out of bounds"))


  let satisfy test= fun state->
    let pos= state.pos in
    if pos.cnum < state.maxlen then
      let found= String.get state.data pos.cnum in
      if test found then
        let pos= { pos with cnum= pos.cnum + 1 } in
        (Ok (found, { state with pos }))
      else
        Error (
          state.pos,
          sprintf "\"%c\" isn't satisfied" found)
    else
      (Error (state.pos, "out of bounds"))

  (* combinator *)
  let fail msg= fun state-> Error (state.pos, msg)

  let return v= fun state-> Ok (v, state)

  let bind (p: 'a parser) (f: 'a -> 'b parser)= fun state->
    let result= p state in
    match result with
    | Error e-> Error e
    | Ok (v,state)-> f v state

  let (>>=)= bind
  let (>>) p1 p2= p1 >>= fun _ -> p2
  let (<<) p1 p2= p1 >>= fun x-> p2 >> return x
  let (|>>) p f= p >>= fun v-> return (f v)
  let (>>$) p v= p >> return v

  let (<|>) (p1:'a parser) (p2:'a parser)= fun state->
    let result= p1 state in
    match result with
    | Error _-> p2 state
    | Ok _-> result

  let between left right p= left >> p << right

  let many p=
    let rec parser s=
      (((p |>> fun v-> Some v) <|> return None) >>= (function
        | Some v-> parser |>> (fun r-> v :: r)
        | None-> return []))
        s
    in parser

  let many1 p=
    p >>= fun v-> many p |>> fun l-> v :: l

  let rec times num p s=
    if num > 0 then
      (p >>= (fun v-> times (num-1) p |>> (fun r-> v::r))) s
    else
      (return []) s

  let sepBy1 sep p=
    p >>= fun head->
    many (sep >> p) >>= fun body->
    return (head :: body)

  let sepBy sep p= sepBy1 sep p <|> return []

  let sepEndBy sep p= many (p << sep)

  let sepEndBy1 sep p= many1 (p << sep)

  let opt default p=
    p <|> return default

  let option p= p |>> (fun v-> Some v) <|> return None

  let lookAhead p= fun state->
    let reply= p state in
    match reply with
    | Ok (r, newState)-> Ok (r, state)
    | Error _-> reply
    [@@ocaml.warning "-27"]

  let followedBy p msg= fun state->
    let reply= p state in
    match reply with
    | Ok _-> Ok ((), state)
    | Error _-> Error (state.pos, msg)

  let notFollowedBy p msg= fun state->
    let reply= p state in
    match reply with
    | Ok _-> Error (state.pos, msg)
    | Error _-> Ok ((), state)

  (* parser *)
  let eof state=
    if state.pos.cnum >= state.maxlen
    then Ok ((), state)
    else Error (state.pos, "not eof")

  let newline_lf state=
    let pos= state.pos in
    if pos.cnum < state.maxlen then
      let found= String.get state.data pos.cnum in
      if found = '\n' then
        let cnum= pos.cnum + 1
        and line= pos.line + 1 in
        let bol= cnum in
        let pos= { cnum; line; bol } in
        (Ok (String.make 1 found, { state with pos }))
      else
        Error (
          state.pos,
          sprintf "newline-lf expected but \"%c\" found" found)
    else
      (Error (state.pos, "out of bounds"))

  let newline_cr state=
    let pos= state.pos in
    if pos.cnum < state.maxlen then
      let found= String.get state.data pos.cnum in
      if found = '\r' then
        let cnum= pos.cnum + 1
        and line= pos.line + 1 in
        let bol= cnum in
        let pos= { cnum; line; bol } in
        (Ok (String.make 1 found, { state with pos }))
      else
        Error (
          state.pos,
          sprintf "newline-cr expected but \"%c\" found" found)
    else
      (Error (state.pos, "out of bounds"))

  let newline_crlf state=
    let pos= state.pos in
    if pos.cnum + 2 <= state.maxlen then
      let found= String.sub state.data pos.cnum 2 in
      if found = "\r\n" then
        let cnum= pos.cnum + 1
        and line= pos.line + 1 in
        let bol= cnum in
        let pos= { cnum; line; bol } in
        (Ok (found, { state with pos }))
      else
        Error (
          state.pos,
          sprintf "newline-crlf expected but \"%s\" found" found)
    else
      (Error (state.pos, "out of bounds"))

  let newline_lfcr state=
    let pos= state.pos in
    if pos.cnum + 2 <= state.maxlen then
      let found= String.sub state.data pos.cnum 2 in
      if found = "\n\r" then
        let cnum= pos.cnum + 1
        and line= pos.line + 1 in
        let bol= cnum in
        let pos= { cnum; line; bol } in
        (Ok (found, { state with pos }))
      else
        Error (
          state.pos,
          sprintf "newline-lfcr expected but \"%s\" found" found)
    else
      (Error (state.pos, "out of bounds"))

  let newline= newline_crlf <|> newline_lfcr
    <|> newline_lf <|> newline_cr 


  let int8= any |>> int_of_char

  let int16= any >>= fun l-> any
    |>> fun h-> int_of_char h lsl 8 + int_of_char l
  let int16_net= any >>= fun h-> any
    |>> fun l-> int_of_char h lsl 8 + int_of_char l

  let int32= int16 >>= fun l-> int16
    |>> fun h-> Int32.(add (shift_left (of_int h) 16) (of_int l))
  let int32_net= int16_net >>= fun h-> int16_net
    |>> fun l-> Int32.(add (shift_left (of_int h) 16) (of_int l))

  let int64= int32 >>= fun l-> int32
    |>> fun h-> Int64.(add (shift_left (of_int32 h) 32) (of_int32 l))
  let int64_net= int32_net >>= fun h-> int32_net
    |>> fun l-> Int64.(add (shift_left (of_int32 h) 32) (of_int32 l))

  let num_dec= satisfy (fun c->
    '0' <= c && c <= '9')

  let num_bin= satisfy (fun c->
    c = '0' || c = '1')

  let num_oct= satisfy (fun c->
    '0' <= c && c <= '7')

  let num_hex= satisfy (fun c->
    '0' <= c && c <= '9'
    || 'a' <= c && c <= 'f'
    || 'A' <= c && c <= 'F')

  let lowercase= satisfy (fun c->
    'a' <= c && c <= 'z')

  let uppercase= satisfy (fun c->
    'A' <= c && c <= 'Z')

  (* start parsing *)
  let parse_string parser str= parser (initState str)
end [@@ocaml.warning "-32-34"]

module Parser = struct
  open MiniParsec

  let string_of_cl cl= String.concat "" (List.map (String.make 1) cl)

  (* OCaml comments. Nested comments are handled correctly. *)
  let rec p_comment state=
    (string "(*" >>
    many
      ((p_comment |>> String.concat "")
      <|>
      (((newline |>> fun _-> '\n')
        <|> char '*' << notFollowedBy (char ')') ""
        <|> satisfy ((<>) '*'))
        |>> String.make 1))
    << string "*)") state

  let p_space= char ' ' <|> char '\t'
    <|> (newline |>> (fun _-> '\n'))
    <|> (p_comment |>> (fun _-> ' '))
  let p_spaces= many p_space
  let p_spaces1= many1 p_space

  let p_ocaml_num_dec= many1 num_dec

  let p_ocaml_num_bin= (string "0b" <|> string "0B") >>
    many1 num_bin >>= fun v->
    return ('0'::'b'::v)

  let p_ocaml_num_oct= (string "0o" <|> string "0O") >>
    many1 num_oct >>= fun v->
    return ('0'::'o'::v)

  let p_ocaml_num_hex= (string "0x" <|> string "0X") >>
    many1 num_hex >>= fun v->
    return ('0'::'x'::v)

  let p_ocaml_num=
    p_ocaml_num_bin
    <|> p_ocaml_num_oct
    <|> p_ocaml_num_hex
    <|> p_ocaml_num_dec

  let p_code= p_ocaml_num |>> string_of_cl |>> int_of_string

  let p_tuple= char '(' >> p_spaces >>
    p_code >>= fun start->
    p_spaces >> char ',' >> p_spaces >>
    p_code >>= fun stop->
    p_spaces >> char ')' >>
    return (start, stop)

  let p_tuples=
    let p_tuples_tl=
      many (p_spaces >> char ';' >> p_spaces >> p_tuple)
      << option (p_spaces >> char ';')
    in
    p_tuple >>= fun hd->
    p_tuples_tl >>= fun tl->
    return (hd::tl)

  let p_list=
    (p_spaces >> char '[' >> p_spaces)
    >> opt [] p_tuples <<
    (p_spaces >> char ']' >> p_spaces1)

  let p_assgin name= p_spaces >>
    string "let" >> p_spaces >>
    string name >> p_spaces >>
    char '=' >> p_spaces >>
    p_list


  let p_unprintable= p_assgin "unprintable" |>> fun s-> `Unprintable s
  let p_combining= p_assgin "combining" |>> fun s-> `Combining s
  let p_w2= p_assgin "w2" |>> fun s-> `W2 s
  let p_w3= p_assgin "w3" |>> fun s-> `W3 s
  let p_w4= p_assgin "w4" |>> fun s-> `W4 s
  let p_w5= p_assgin "w5" |>> fun s-> `W5 s
  let p_w6= p_assgin "w6" |>> fun s-> `W6 s

  let p_set= p_unprintable <|> p_combining
    <|> p_w2 <|> p_w3 <|> p_w4 <|> p_w5 <|> p_w6
    <|> (eof |>> fun _-> `Eof)

  let p_cfg=
    let unprintable= ref []
    and combining= ref []
    and w2= ref []
    and w3= ref []
    and w4= ref []
    and w5= ref []
    and w6= ref [] in
    let rec p_cfg_aux state=
      let result= p_set state in
      match result with
      | Error e-> Error e
      | Ok (`Eof, state)-> Ok ((), state)
      | Ok (`Unprintable set, state)-> unprintable:= set; p_cfg_aux state
      | Ok (`Combining set, state)-> combining:= set; p_cfg_aux state
      | Ok (`W2 set, state)-> w2:= set; p_cfg_aux state
      | Ok (`W3 set, state)-> w3:= set; p_cfg_aux state
      | Ok (`W4 set, state)-> w4:= set; p_cfg_aux state
      | Ok (`W5 set, state)-> w5:= set; p_cfg_aux state
      | Ok (`W6 set, state)-> w6:= set; p_cfg_aux state
    in
    p_cfg_aux
      |>> fun ()-> (!unprintable, !combining, !w2, !w3, !w4, !w5, !w6)
end

type widthTable= {
  unprintable: Codes.t;
  combining: Codes.t;
  w2: Codes.t;
  w3: Codes.t;
  w4: Codes.t;
  w5: Codes.t;
  w6: Codes.t;
}

type t= widthTable

let load_from_string cfg=
  match MiniParsec.parse_string Parser.p_cfg cfg with
  | Ok ((unprintable, combining, w2, w3, w4, w5, w6), _)->
    let unprintable= Codes.of_tuple_list unprintable
    and combining= Codes.of_tuple_list combining
    and w2= Codes.of_tuple_list w2
    and w3= Codes.of_tuple_list w3
    and w4= Codes.of_tuple_list w4
    and w5= Codes.of_tuple_list w5
    and w6= Codes.of_tuple_list w6 in
    Ok { unprintable; combining; w2; w3; w4; w5; w6 }
  | Error (pos, _)-> Error pos.cnum

let load_from_path path=
  let ic= open_in path in
  let length= in_channel_length ic in
  let cfg= really_input_string ic length in
  load_from_string cfg

let union cfg1 cfg2=
  let unprintable= Codes.union cfg1.unprintable cfg2.unprintable
  and combining= Codes.union cfg1.combining cfg2.combining
  and w2= Codes.union cfg1.w2 cfg2.w2
  and w3= Codes.union cfg1.w3 cfg2.w3
  and w4= Codes.union cfg1.w4 cfg2.w4
  and w5= Codes.union cfg1.w5 cfg2.w5
  and w6= Codes.union cfg1.w6 cfg2.w6 in
  { unprintable; combining; w2; w3; w4; w5; w6 }

