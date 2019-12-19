
open Ocamlbuild_plugin;;

Options.use_ocamlfind := true ;;
#if OCAML_VERSION > (4, 1, 0)
Ocamlbuild_pack.Flags.mark_tag_used "use_" ;;
Ocamlbuild_pack.Flags.mark_tag_used "pkg_" ;;
Ocamlbuild_pack.Flags.mark_tag_used "link_" ;;
#endif

let modules_dirs = [
  "common"; "versioning"; "pef"; "opam"; "npm"; "deb"; "opencsw"; "rpm"; "algo";
  "doseparse"; "doseparseNoRpm"; "applications"; "experimental"; "libcudf";
  "experimental/dudftocudf"
];;

let libraries = [
  "cudf";"common";"versioning"; "pef"; "opam"; "npm"; "debian"; "csw";"rpm";"algo";
  "doseparse";"doseparseNoRpm"
];;

let doselibs = "doselibs" ;;

(* everybody can see the library dir *)
List.iter (fun d -> Pathname.define_context d [doselibs]) modules_dirs ;;

let _ = dispatch begin function
   | After_rules ->
       List.iter (fun lib ->
         flag ["ocaml"; "link"; "use_"^lib; "program"; "native"] & 
         S[A("doselibs/"^lib^".cmxa")];
         
         flag ["ocaml"; "link"; "use_"^lib; "program"; "byte"] & 
         S[A("doselibs/"^lib^".cma")];
       ) libraries
       ;

       (* add add the directory rpm to find the dllrpm?_stubs at
        * link time . we need this only with bytecode *)
       flag ["ocaml"; "link"; "use_rpm"; "program"; "byte"] & S[
         A"-I"; A"rpm"
       ];

       (* add compilation flags for rpm *)
       flag ["c"; "use_rpm"; "compile"] & S[ 
         A"-ccopt"; A"-I/usr/include/rpm"; 
       ];

       flag ["c"; "use_rpm"; "ocamlmklib"] & S[ 
         A"-lrpm";
         A"-lrpmio";
       ];

       (* Disable Warning 24: bad source file name *)
       flag ["ocaml"; "compile"] & S[A"-w"; A"-24"];

       (* optimization to ocaml code *)
       flag ["ocaml"; "compile"] & S[A"-ccopt"; A"-O9"];

       dep ["ocaml"; "use_rpm5"; "compile"] ["rpm/dllrpm5_stubs.so"];
       dep ["ocaml"; "use_rpm4"; "compile"] ["rpm/dllrpm4_stubs.so"];

       dep ["link"; "ocaml"; "link_rpm4"] ["rpm/librpm4_stubs.a"];
       dep ["link"; "ocaml"; "link_rpm5"] ["rpm/librpm5_stubs.a"];

       (* add rpm libraries *)
       flag ["ocaml"; "use_rpm"; "link"] & S[
         A"-cclib"; A"-lrpm";
         A"-cclib"; A"-lrpmio";
         A"-ccopt"; A"-Lrpm";
       ];

       flag ["ocaml"; "use_rpm"; "link"; "program"; "byte"] & S[A"-I"; A"rpm"];

       flag ["link"; "ocaml"; "link_rpm4"] & S[A"rpm/librpm4_stubs.a"];
       flag ["link"; "ocaml"; "link_rpm5"] & S[A"rpm/librpm5_stubs.a"];

       flag ["ocaml"; "use_rpm5"; "link"; "library"; "byte"] & S[
         (* A"-dllib"; A"-lrpm5_stubs"; *)
         A"-custom"; A"rpm/librpm5_stubs.a"
       ];

       flag ["ocaml"; "use_rpm4"; "link"; "library"; "byte"] & S[
         (* A"-dllib"; A"-lrpm4_stubs"; *)
         A"-custom"; A"rpm/librpm4_stubs.a"
       ];

       flag ["ocaml"; "use_rpm5"; "link"; "library"; "native"] & S[
         A"-cclib"; A"-lrpm5_stubs";
       ];
       flag ["ocaml"; "use_rpm4"; "link"; "library"; "native"] & S[
         A"-cclib"; A"-lrpm4_stubs";
       ];
       
       flag ["ocaml"; "pkg_threads"; "compile"] (S[A "-thread"]);
       flag ["ocaml"; "pkg_threads"; "link"] (S[A "-thread"]);
   | _ -> ()
end
