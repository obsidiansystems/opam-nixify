{ stdenv, lib, fetchgit, fetchurl, makeWrapper,
  ocaml, unzip, ncurses, curl, aspcud
}:

assert lib.versionAtLeast ocaml.version "4.02.3";

let
  srcs = {
    cmdliner = fetchurl {
      url = "http://erratique.ch/software/cmdliner/releases/cmdliner-1.0.2.tbz";
      sha256 = "18jqphjiifljlh9jg8zpl6310p3iwyaqphdkmf89acyaix0s4kj1";
    };
    cppo = fetchurl {
      url = "https://github.com/mjambon/cppo/archive/v1.6.4.tar.gz";
      sha256 = "0jdb7d21lfa3ck4k59mrqs5pljzq5rb504jq57nnrc6klljm42j7";
    };
    cudf = fetchurl {
      url = "https://gforge.inria.fr/frs/download.php/36602/cudf-0.9.tar.gz";
      sha256 = "0771lwljqwwn3cryl0plny5a5dyyrj4z6bw66ha5n8yfbpcy8clr";
    };
    dose3 = fetchurl {
      url = "https://gforge.inria.fr/frs/download.php/file/36063/dose3-5.0.1.tar.gz";
      sha256 = "00yvyfm4j423zqndvgc1ycnmiffaa2l9ab40cyg23pf51qmzk2jm";
    };
    extlib = fetchurl {
      url = "http://ygrek.org.ua/p/release/ocaml-extlib/extlib-1.7.4.tar.gz";
      sha256 = "18jb4rvkk6p3mqnkamwb41x8q49shgn43h020bs4cp4vac7nrhnr";
    };
    jbuilder = fetchurl {
      url = "https://github.com/ocaml/dune/releases/download/1.0+beta20/jbuilder-1.0.beta20.tbz";
      sha256 = "07hl9as5llffgd6hbw41rs76i1ibgn3n9r0dba5h0mdlkapcwb10";
    };
    mccs = fetchurl {
      url = "https://github.com/AltGr/ocaml-mccs/archive/1.1+8.tar.gz";
      sha256 = "0xavfvxfrcf3lmry8ymma1yzy0hw3ijbx94c9zq3pzlwnylrapa4";
    };
    ocamlgraph = fetchurl {
      url = "http://ocamlgraph.lri.fr/download/ocamlgraph-1.8.8.tar.gz";
      sha256 = "0m9g16wrrr86gw4fz2fazrh8nkqms0n863w7ndcvrmyafgxvxsnr";
    };
    opam-file-format = fetchurl {
      url = "https://github.com/ocaml/opam-file-format/archive/2.0.0-rc2.tar.gz";
      sha256 = "1mgk08msp7hxn0hs0m82vky3yv6hcq4pw5402b3vhx4c49431jsb";
    };
    re = fetchurl {
      url = "https://github.com/ocaml/ocaml-re/releases/download/1.7.3/re-1.7.3.tbz";
      sha256 = "0nv933qfl8y9i19cqvhsalwzif3dkm28vg478rpnr4hgfqjlfryr";
    };
    result = fetchurl {
      url = "https://github.com/janestreet/result/releases/download/1.3/result-1.3.tbz";
      sha256 = "1lrnbxdq80gbhnp85mqp1kfk0bkh6q1c93sfz2qgnq2qyz60w4sk";
    };
    opam = fetchurl {
      url = "https://github.com/ocaml/opam/archive/2.0.0-rc3.zip";
      sha256 = "0swjyvg9b3c4h92bdafa95yk75s5pacqpi33aycav19xm0s087rf";
    };
  };
in stdenv.mkDerivation rec {
  name = "opam-${version}";
  version = "2.0.0-rc3";

  buildInputs = [ unzip curl ncurses ocaml makeWrapper];

  src = srcs.opam;

  postUnpack = ''
    ln -sv ${srcs.cmdliner} $sourceRoot/src_ext/cmdliner.tbz
    ln -sv ${srcs.cppo} $sourceRoot/src_ext/cppo.tar.gz
    ln -sv ${srcs.cudf} $sourceRoot/src_ext/cudf.tar.gz
    ln -sv ${srcs.dose3} $sourceRoot/src_ext/dose3.tar.gz
    ln -sv ${srcs.extlib} $sourceRoot/src_ext/extlib.tar.gz
    ln -sv ${srcs.jbuilder} $sourceRoot/src_ext/jbuilder.tbz
    ln -sv ${srcs.mccs} $sourceRoot/src_ext/mccs.tar.gz
    ln -sv ${srcs.ocamlgraph} $sourceRoot/src_ext/ocamlgraph.tar.gz
    ln -sv ${srcs.opam-file-format} $sourceRoot/src_ext/opam-file-format.tar.gz
    ln -sv ${srcs.re} $sourceRoot/src_ext/re.tbz
    ln -sv ${srcs.result} $sourceRoot/src_ext/result.tbz
  '';

  preConfigure = ''
    substituteInPlace ./src_ext/Makefile --replace "%.stamp: %.download" "%.stamp:"
  '';

  postConfigure = "make lib-ext";

  # Dirty, but apparently ocp-build requires a TERM
  makeFlags = ["TERM=screen"];

  outputs = [ "out" "installer" ];
  setOutputFlags = false;

  # change argv0 to "opam" as a workaround for
  # https://github.com/ocaml/opam/issues/2142
  postInstall = ''
    mv $out/bin/opam $out/bin/.opam-wrapped
    makeWrapper $out/bin/.opam-wrapped $out/bin/opam \
      --argv0 "opam" \
      --suffix PATH : ${aspcud}/bin
    $out/bin/opam-installer --prefix=$installer opam-installer.install
  '';

  doCheck = false;

  meta = with stdenv.lib; {
    description = "A package manager for OCaml";
    homepage = http://opam.ocamlpro.com/;
    maintainers = [ maintainers.henrytill ];
    platforms = platforms.all;
  };
}
