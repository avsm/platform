let time_shift = ref 0.0

let timed_lazy r x =
  lazy (
    let start = Misc.time_spent () in
    let time_shift0 = !time_shift in
    let update () =
      let delta = Misc.time_spent () -. start in
      let shift = !time_shift -. time_shift0 in
      time_shift := time_shift0 +. delta;
      r := !r +. delta -. shift;
    in
    match Lazy.force x with
    | x -> update (); x
    | exception exn -> update (); Std.reraise exn
  )

module Typer = struct
  type t = {
    errors : exn list lazy_t;
    result : Mtyper.result;
  }
end

module Ppx = struct
  type t = {
    config : Mconfig.t;
    errors : exn list;
    parsetree : Mreader.parsetree;
  }
end

type t = {
  trace  : Trace.t;
  config : Mconfig.t;
  raw_source : Msource.t;
  source : Msource.t lazy_t;
  reader : (Mreader.result * Mconfig.t) lazy_t;
  ppx    : Ppx.t lazy_t;
  typer  : Typer.t lazy_t;

  pp_time     : float ref;
  reader_time : float ref;
  ppx_time    : float ref;
  typer_time  : float ref;
  error_time  : float ref;
}

let get_trace t = t.trace

let raw_source t = t.raw_source

let input_config t = t.config
let input_source t = Lazy.force t.source

let get_lexing_pos t pos =
  Msource.get_lexing_pos
    t.trace (input_source t) ~filename:(Mconfig.filename t.config) pos

let with_reader t f =
  Mreader.with_ambient_reader t.trace t.config (input_source t) f

let reader t = Lazy.force t.reader

let ppx    t = Lazy.force t.ppx
let typer  t = Lazy.force t.typer

let reader_config    t = (snd (reader t))
let reader_parsetree t = (fst (reader t)).Mreader.parsetree
let reader_comments  t = (fst (reader t)).Mreader.comments
let reader_lexer_errors  t = (fst (reader t)).Mreader.lexer_errors
let reader_parser_errors t = (fst (reader t)).Mreader.parser_errors
let reader_no_labels_for_completion t =
  (fst (reader t)).Mreader.no_labels_for_completion

let ppx_parsetree t = (ppx t).Ppx.parsetree
let ppx_errors    t = (ppx t).Ppx.errors

let final_config  t = (ppx t).Ppx.config

let typer_result t = (typer t).Typer.result
let typer_errors t = Lazy.force (typer t).Typer.errors

let process trace
    ?(pp_time=ref 0.0)
    ?(reader_time=ref 0.0)
    ?(ppx_time=ref 0.0)
    ?(typer_time=ref 0.0)
    ?(error_time=ref 0.0)
    ?for_completion
    config raw_source =
  let source = timed_lazy pp_time (lazy (
      match Mconfig.(config.ocaml.pp) with
      | None -> raw_source
      | Some { workdir; workval } ->
        let source = Msource.text raw_source in
        let source =
          Pparse.apply_pp
            ~workdir ~filename:Mconfig.(config.query.filename)
            ~source ~pp:workval
        in
        Msource.make trace source
    )) in
  let reader = timed_lazy reader_time (lazy (
      let lazy source = source in
      let result = Mreader.parse trace ?for_completion config source in
      let config = result.Mreader.config in
      let config = Mreader.apply_directives config result.Mreader.parsetree in
      let config = Mconfig.normalize trace config in
      result, config
    )) in
  let ppx = timed_lazy ppx_time (lazy (
      let lazy ({Mreader.parsetree; _}, config) = reader in
      let caught = ref [] in
      Msupport.catch_errors Mconfig.(config.ocaml.warnings) caught @@ fun () ->
      let config, parsetree = Mppx.rewrite trace config parsetree in
      { Ppx. config; parsetree; errors = !caught }
    )) in
  let typer = timed_lazy typer_time (lazy (
      let lazy source = source in
      let lazy { Ppx. config; parsetree; _ } = ppx in
      let result = Mtyper.run trace config source parsetree in
      let errors = timed_lazy error_time (lazy (Mtyper.get_errors result)) in
      { Typer. errors; result }
    )) in
  { trace; config; raw_source; source; reader; ppx; typer;
    pp_time; reader_time; ppx_time; typer_time; error_time }

let make tr config source =
  process tr (Mconfig.normalize tr config) source

let for_completion position
    {trace; config; raw_source;
     pp_time; reader_time; ppx_time; typer_time; error_time; _} =
  process trace config raw_source ~for_completion:position
    ~pp_time ~reader_time ~ppx_time ~typer_time ~error_time

let timing_information t = [
  "pp"     , !(t.pp_time);
  "reader" , !(t.reader_time);
  "ppx"    , !(t.ppx_time);
  "typer"  , !(t.typer_time);
  "error"  , !(t.error_time);
]
