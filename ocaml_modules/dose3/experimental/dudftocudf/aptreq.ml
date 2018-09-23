  let request =
    let mapver = function
      |`Pkg p -> (p,None)
      |`PkgVer (p,v) -> begin
          try (p,Some(`Eq,Rpm.Rpmcudf.get_cudf_version tables (p,v)))
          with Not_found -> failwith (Printf.sprintf "There is no version %s of package %s" p v)
      end
      |`PkgDst (p,d) ->
            failwith (Printf.sprintf "There is no package %s in release %s " p d)
    in
    let request_id =
      if OptParse.Opt.is_set Options.problemid then OptParse.Opt.get Options.problemid
      else if uid <> "" then uid
      else (string_of_int (Random.bits ()))
    in
    let parsed_action =
      match dudfdoc.metaInstaller.name with
      |"apt" -> Debian.Apt.parse_request_apt action
      |s -> failwith("Unsupported meta installer "^s)
    in
    match parsed_action with
    |Debian.Apt.Upgrade (Some (suite))
    |Debian.Apt.DistUpgrade (Some (suite)) ->
        let il = Rpm.Packages.Set.fold (fun pkg acc -> `PkgDst (pkg.Rpm.Packages.name,suite) :: acc) installed_packages [] in
        let l = List.map mapver il in
        { Cudf.request_id = request_id ; install = l ; remove = [] ; upgrade = [] ; req_extra = [] ; }
    |Debian.Apt.Install l ->
        let l = List.map mapver l in
        { Cudf.request_id = request_id ; install = l ; remove = [] ; upgrade = [] ; req_extra = [] ; }
    |Debian.Apt.Remove l ->
        let l = List.map (fun (`Pkg p) -> (p,None) ) l in
        { Cudf.request_id = request_id ; install = [] ; remove = l ; upgrade = [] ; req_extra = [] ;}
    |Debian.Apt.Upgrade None ->
        { Cudf.request_id = request_id ; install = [] ; remove = [] ; upgrade = [] ; req_extra = [] ; }
    |Debian.Apt.DistUpgrade None ->
        { Cudf.request_id = request_id ; install = [] ; remove = [] ; upgrade = [] ; req_extra = [] ; }
  in

