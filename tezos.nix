{ lib, stdenv, ocaml, findlib, jbuilder, bigstring, cstruct, hex, ocplib-endian, re, zarith, calendar, cmdliner, ocamlLib }:

assert ocamlLib.versionAtLeast (lib.getVersion bigstring) "0.1.1";
assert ocamlLib.versionAtLeast (lib.getVersion cstruct) "3.2.1";
assert ocamlLib.versionAtLeast (lib.getVersion ocplib-endian) "1.0";
assert ocamlLib.versionAtLeast (lib.getVersion re) "1.7.2";
assert ocamlLib.versionAtLeast (lib.getVersion zarith) "1.7";

stdenv.mkDerivation rec {
  name = "tezos-${version}";
  version = "0.0.0";
  srcs = [ tezos/src tezos/vendors tezos/scripts tezos/docs ];
  postUnpack = ''
    cp ${tezos/jbuild} jbuild
    cp ${tezos/Makefile} Makefile
  '';
  sourceRoot = ".";
  buildInputs = [ ocaml findlib jbuilder bigstring cstruct hex ocplib-endian re zarith calendar cmdliner ];

  # installPhase = "$opam/bin/opam-installer -i --prefix=$out --libdir=$OCAMLFIND_DESTDIR";

  meta = {
    # homepage = https://github.com/janestreet/jbuilder;
    description = "Tezos blockchain reference implementation.";
    # maintainers = [ stdenv.lib.maintainers.vbgl ];
    # license = stdenv.lib.licenses.asl20;
    inherit (ocaml.meta) platforms;
  };
}
