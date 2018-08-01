{ pkgs ? import /home/elliot/obsidian/xplat-nixpkgs {} }:
let
  ocamlLib = rec {
    compareVersions = a: b:
      if a == b then 0 else 
      if a == "transition" then 1 else
      if b == "transition" then -1 else
      builtins.compareVersions a b;
    versionAtLeast = a: b: compareVersions a b >= 0;
  };
in pkgs.ocaml-ng.mkOcamlPackages pkgs.ocaml-ng.ocamlPackages_4_06.ocaml (super: self: {
  tezos = self.callPackage ./tezos.nix { inherit ocamlLib; };

  opam = self.callPackage handy/opam2.nix {};
  afl-persistent = self.callPackage nix/ocaml/afl-persistent {};
  alcotest = self.callPackage nix/ocaml/alcotest {};
  alcotest-lwt = self.callPackage nix/ocaml/alcotest-lwt {};
  asn1-combinators = self.callPackage nix/ocaml/asn1-combinators {};
  astring = self.callPackage nix/ocaml/astring {};
  base = self.callPackage nix/ocaml/base {};
  base-bigarray = self.callPackage nix/ocaml/base-bigarray {};
  base-bytes = self.callPackage nix/ocaml/base-bytes {};
  base-threads = self.callPackage nix/ocaml/base-threads {};
  base-unix = self.callPackage nix/ocaml/base-unix {};
  base64 = self.callPackage nix/ocaml/base64 {};
  bigstring = self.callPackage nix/ocaml/bigstring {};
  biniou = self.callPackage nix/ocaml/biniou {};
  calendar = self.callPackage nix/ocaml/calendar {};
  cmdliner = self.callPackage nix/ocaml/cmdliner {};
  cohttp = self.callPackage nix/ocaml/cohttp {};
  cohttp-lwt = self.callPackage nix/ocaml/cohttp-lwt {};
  cohttp-lwt-unix = self.callPackage nix/ocaml/cohttp-lwt-unix {};
  conduit = self.callPackage nix/ocaml/conduit {};
  conduit-lwt = self.callPackage nix/ocaml/conduit-lwt {};
  conduit-lwt-unix = self.callPackage nix/ocaml/conduit-lwt-unix {};
  conf-gmp = self.callPackage nix/ocaml/conf-gmp {};
  conf-hidapi = self.callPackage nix/ocaml/conf-hidapi {};
  conf-libev = self.callPackage nix/ocaml/conf-libev {};
  conf-m4 = self.callPackage nix/ocaml/conf-m4 {};
  conf-perl = self.callPackage nix/ocaml/conf-perl {};
  conf-pkg-config = self.callPackage nix/ocaml/conf-pkg-config {};
  conf-which = self.callPackage nix/ocaml/conf-which {};
  configurator = self.callPackage nix/ocaml/configurator {};
  cppo = self.callPackage nix/ocaml/cppo {};
  cppo_ocamlbuild = self.callPackage nix/ocaml/cppo_ocamlbuild {};
  cpuid = self.callPackage nix/ocaml/cpuid {};
  crowbar = self.callPackage nix/ocaml/crowbar {};
  cstruct = self.callPackage nix/ocaml/cstruct {};
  cstruct-lwt = self.callPackage nix/ocaml/cstruct-lwt {};
  easy-format = self.callPackage nix/ocaml/easy-format {};
  dune = self.callPackage nix/ocaml/dune {};
  ezjsonm = self.callPackage nix/ocaml/ezjsonm {};
  fieldslib = self.callPackage nix/ocaml/fieldslib {};
  fmt = self.callPackage nix/ocaml/fmt {};
  hex = self.callPackage nix/ocaml/hex {};
  hidapi = self.callPackage nix/ocaml/hidapi {};
  ipaddr = self.callPackage nix/ocaml/ipaddr {};
  irmin = self.callPackage nix/ocaml/irmin {};
  #jbuilder = self.callPackage nix/ocaml/jbuilder {};
  jbuilder = self.callPackage nix/ocaml/dune {};  # fake it out and make it dune instead.
  js_of_ocaml = self.callPackage nix/ocaml/js_of_ocaml {};
  js_of_ocaml-compiler = self.callPackage nix/ocaml/js_of_ocaml-compiler {};
  jsonm = self.callPackage nix/ocaml/jsonm {};
  logs = self.callPackage nix/ocaml/logs {};
  lwt = self.callPackage nix/ocaml/lwt {};
  magic-mime = self.callPackage nix/ocaml/magic-mime {};
  mirage-no-solo5 = self.callPackage nix/ocaml/mirage-no-solo5 {};
  mirage-no-xen = self.callPackage nix/ocaml/mirage-no-xen {};
  mtime = self.callPackage nix/ocaml/mtime {};
  num = self.callPackage nix/ocaml/num {};
  ocaml-compiler-libs = self.callPackage nix/ocaml/ocaml-compiler-libs {};
  ocaml-migrate-parsetree = self.callPackage nix/ocaml/ocaml-migrate-parsetree {};
  ocamlbuild = self.callPackage nix/ocaml/ocamlbuild {};
  ocamlgraph = self.callPackage nix/ocaml/ocamlgraph {};
  ocb-stubblr = self.callPackage nix/ocaml/ocb-stubblr {};
  ocp-build = self.callPackage nix/ocaml/ocp-build {};
  ocp-indent = self.callPackage nix/ocaml/ocp-indent {};
  ocp-ocamlres = self.callPackage nix/ocaml/ocp-ocamlres {};
  ocplib-endian = self.callPackage nix/ocaml/ocplib-endian {};
  pprint = self.callPackage nix/ocaml/pprint {};
  ppx_ast = self.callPackage nix/ocaml/ppx_ast {};
  ppx_core = self.callPackage nix/ocaml/ppx_core {};
  ppx_cstruct = self.callPackage nix/ocaml/ppx_cstruct {};
  ppx_derivers = self.callPackage nix/ocaml/ppx_derivers {};
  ppx_deriving = self.callPackage nix/ocaml/ppx_deriving {};
  ppx_driver = self.callPackage nix/ocaml/ppx_driver {};
  ppx_fields_conv = self.callPackage nix/ocaml/ppx_fields_conv {};
  ppx_metaquot = self.callPackage nix/ocaml/ppx_metaquot {};
  ppx_optcomp = self.callPackage nix/ocaml/ppx_optcomp {};
  ppx_sexp_conv = self.callPackage nix/ocaml/ppx_sexp_conv {};
  ppx_tools = self.callPackage nix/ocaml/ppx_tools {};
  ppx_tools_versioned = self.callPackage nix/ocaml/ppx_tools_versioned {};
  ppx_traverse_builtins = self.callPackage nix/ocaml/ppx_traverse_builtins {};
  ppx_type_conv = self.callPackage nix/ocaml/ppx_type_conv {};
  ptime = self.callPackage nix/ocaml/ptime {};
  re = self.callPackage nix/ocaml/re {};
  result = self.callPackage nix/ocaml/result {};
  rresult = self.callPackage nix/ocaml/rresult {};
  sexplib = self.callPackage nix/ocaml/sexplib {};
  stdio = self.callPackage nix/ocaml/stdio {};
  stringext = self.callPackage nix/ocaml/stringext {};
  tls = self.callPackage nix/ocaml/tls {};
  topkg = self.callPackage nix/ocaml/topkg {};
  uchar = self.callPackage nix/ocaml/uchar {};
  uri = self.callPackage nix/ocaml/uri {};
  uutf = self.callPackage nix/ocaml/uutf {};
  x509 = self.callPackage nix/ocaml/x509 {};
  yojson = self.callPackage nix/ocaml/yojson {};
  zarith = self.callPackage nix/ocaml/zarith {};
})
