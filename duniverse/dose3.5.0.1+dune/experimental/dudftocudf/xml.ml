
module LazyList = struct
(* code from  http://www.cs.utoronto.ca/~trebla/LazyList.ml *)

  open Lazy

  type 'a list = 'a listrep Lazy.t
  and 'a listrep =
    | Nil
    | Cons of 'a * 'a list

  let hd s = match force s with
  | Nil -> failwith "hd"
  | Cons (a,_) -> a

  let tl s = match force s with
  | Nil -> s
  | Cons (_,t) -> t

  let rec fold_right f s a = begin
    match force s with
    | Nil -> force a
    | Cons (x,xs) -> f x (lazy (fold_right f xs a))
  end

  let rec fold_right' f s a = begin
    match force s with
    | Nil -> a
    | Cons (x,xs) -> f x (fold_right' f xs a)
  end

  let rec fold_left f a s = begin
    match force s with
    | Nil -> a
    | Cons (x,t) -> fold_left f (f a x) t
  end

  let map f s = lazy (fold_right (fun x s -> Cons(f x, s)) s (lazy_from_val Nil))

  let iter p s = fold_left (fun () x -> p x) () s

  let one x = lazy_from_val (Cons(x, lazy_from_val Nil))

  let empty = lazy_from_val Nil

  let append s0 s1 = lazy (fold_right (fun x s -> Cons(x, s)) s0 s1)

  let (@@) = append

  let push e l = append (one e) l

  let concat s = lazy (fold_right (fun s0 s1 -> force (s0 @@ s1)) s (lazy_from_val Nil))

  let length s = fold_left (fun n _ -> n+1) 0 s

  let to_list s = fold_right (fun a s -> a :: force s) s (lazy_from_val [])

  let rec of_list = function
    | [] -> lazy_from_val Nil
    | x::xs -> lazy (Cons (x, of_list xs))

end

(* code from xml-ligth *)

type xml =
  | Element of (tag * attribute list * children)
  | PCData of string
  | CData of string
  | Empty
and children = xml LazyList.list
and tag = string
and attribute = (string * string)

exception Not_element of xml
exception Not_pcdata of xml
exception Not_cdata of xml
exception Not_attribute of string

let empty = Empty

let tag = function
  | Element (tag,_,_) -> tag
  | x -> raise (Not_element x)

let pcdata = function
  | PCData text -> text
  | x -> raise (Not_pcdata x)

let cdata = function
  | CData text -> text
  | x -> raise (Not_cdata x)

let attribs = function
  | Element (_,attr,_) -> attr
  | x -> raise (Not_element x)

let attrib x att =
  match x with
  | Element (_,attr,_) ->
      (try
      let att = String.lowercase att in
      snd (List.find (fun (n,_) -> String.lowercase n = att) attr)
      with Not_found -> raise (Not_attribute att))
  | x -> raise (Not_element x)

let children = function
  | Element (_,_,clist) -> LazyList.to_list clist
  | _ -> []

let iter f = function
  | Element (_,_,clist) -> LazyList.iter f clist
  | x -> raise (Not_element x)

let map f = function
  | Element (_,_,clist) -> LazyList.map f clist
  | x -> raise (Not_element x)

let fold f v = function
  | Element (_,_,clist) -> LazyList.fold_left f v clist
  | x -> raise (Not_element x)

let tmp = Buffer.create 1024

let buffer_cdata text = Buffer.add_string tmp text

let buffer_pcdata text =
  let l = String.length text in
  for p = 0 to l-1 do
    match text.[p] with
    | '>' -> Buffer.add_string tmp "&gt;"
    | '<' -> Buffer.add_string tmp "&lt;"
    | '&' ->
      if p < l-1 && text.[p+1] = '#' then
        Buffer.add_char tmp '&'
      else
        Buffer.add_string tmp "&amp;"
    | '\'' -> Buffer.add_string tmp "&apos;"
    | '"' -> Buffer.add_string tmp "&quot;"
    | c -> Buffer.add_char tmp c
  done

let buffer_attr (n,v) =
  Buffer.add_char tmp ' ';
  Buffer.add_string tmp n;
  Buffer.add_string tmp "=\"";
  let l = String.length v in
  for p = 0 to l-1 do
    match v.[p] with
    | '\\' -> Buffer.add_string tmp "\\\\"
    | '"' -> Buffer.add_string tmp "\\\""
    | c -> Buffer.add_char tmp c
  done;
  Buffer.add_char tmp '"'

let to_string x =
  let pcdata = ref false in
  let rec loop = function
    | Element (tag,alist,l) ->
        Buffer.add_char tmp '<';
        Buffer.add_string tmp tag;
        List.iter buffer_attr alist;
        Buffer.add_char tmp '>';
        pcdata := false;
        LazyList.iter loop l;
        Buffer.add_string tmp "</";
        Buffer.add_string tmp tag;
        Buffer.add_char tmp '>';
        pcdata := false;
    | PCData text ->
        if !pcdata then Buffer.add_char tmp ' ';
        buffer_pcdata text;
        pcdata := true;
    | CData text ->
        buffer_cdata text;
        pcdata := false;
    | Empty -> ()
  in
  Buffer.reset tmp; 
  loop x;
  let s = Buffer.contents tmp in
  Buffer.reset tmp; 
  s 
;;

let decode str =
  let amp_re = Pcre.regexp "&amp;" in
  let lt_re = Pcre.regexp "&lt;" in
  let gt_re = Pcre.regexp "&gt;" in
  let quot_re = Pcre.regexp "&quot;" in
  let str = Pcre.replace ~rex:amp_re ~templ:"&" str in
  let str = Pcre.replace ~rex:lt_re ~templ:"<" str in
  let str = Pcre.replace ~rex:gt_re ~templ:">" str in
  let str = Pcre.replace ~rex:quot_re ~templ:"\"" str in
  str
;;

