{ stdenv, fetchurl, bash, ocaml, findlib, jbuilder
, lambdaTerm, ocaml_lwt, react, lwt_react, camomile, cppo, makeWrapper
}:

if !stdenv.lib.versionAtLeast ocaml.version "4.02.3"
then throw "utop is not available for OCaml ${ocaml.version}"
else

stdenv.mkDerivation rec {
  version = "2.1.0";
  name = "utop-${version}";

  src = fetchurl {
    url = "https://github.com/diml/utop/releases/download/2.1.0/utop-2.1.0.tbz";
    sha256 = "1d5jfz2ini2g8hsvw2vf5jv5avz574yf18612l0zfa0ri518n06w";
  };

  nativeBuildInputs = [ makeWrapper ];
  buildInputs = [ ocaml findlib jbuilder ];

  unpackPhase = "tar xjf ${src}; cd ${name}";

  inherit (jbuilder) installPhase;

  propagatedBuildInputs = [ lambdaTerm ocaml_lwt cppo react lwt_react camomile ];

  createFindlibDestdir = true;

  buildPhase = ''
    jbuilder build -p utop
    '';

  dontStrip = true;

  postFixup =
   let
     path = "etc/utop/env";

     # derivation of just runtime deps so env vars created by
     # setup-hooks can be saved for use at runtime
     runtime = stdenv.mkDerivation rec {
       name = "utop-runtime-env-${version}";

       buildInputs = [ findlib ] ++ propagatedBuildInputs;

       phases = [ "installPhase" ];

       installPhase = ''
         mkdir -p "$out"/${path}
         for e in OCAMLPATH CAML_LD_LIBRARY_PATH; do
           printf %s "''${!e}" > "$out"/${path}/$e
         done
       '';
     };

     get = key: ''$(cat "${runtime}/${path}/${key}")'';
   in ''
   for prog in "$out"/bin/*
   do

    # Note: wrapProgram by default calls 'exec -a $0 ...', but this
    # breaks utop on Linux with OCaml 4.04, and is disabled with
    # '--argv0 ""' flag. See https://github.com/NixOS/nixpkgs/issues/24496
    wrapProgram "$prog" \
      --argv0 "" \
      --prefix CAML_LD_LIBRARY_PATH ":" "${get "CAML_LD_LIBRARY_PATH"}" \
      --prefix OCAMLPATH ":" "${get "OCAMLPATH"}" \
      --prefix OCAMLPATH ":" $(unset OCAMLPATH; addOCamlPath "$out"; printf %s "$OCAMLPATH") \
      --add-flags "-I ${findlib}/lib/ocaml/${stdenv.lib.getVersion ocaml}/site-lib"
   done
   '';

  meta = {
    description = "Universal toplevel for OCaml";
    longDescription = ''
    utop is an improved toplevel for OCaml. It can run in a terminal or in Emacs. It supports line edition, history, real-time and context sensitive completion, colors, and more.

    It integrates with the tuareg mode in Emacs.
    '';
    homepage = https://github.com/diml/utop;
    license = stdenv.lib.licenses.bsd3;
    platforms = ocaml.meta.platforms or [];
    maintainers = [
      stdenv.lib.maintainers.gal_bolle
    ];
  };
}
