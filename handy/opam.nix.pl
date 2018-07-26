#!/usr/bin/env perl

use strict;
use warnings qw<all>;

my $OPAM_RELEASE = "2.0.0-rc3";
my $OPAM_RELEASE_SHA256 = `nix-prefetch-url https://github.com/ocaml/opam/archive/$OPAM_RELEASE.zip`;
chomp $OPAM_RELEASE_SHA256;

my $OPAM_BASE_URL = "https://raw.githubusercontent.com/ocaml/opam/$OPAM_RELEASE";
my $OPAM_OPAM = `curl -L --url \Q$OPAM_BASE_URL\E/opam-devel.opam`;
my($OCAML_MIN_VERSION) = $OPAM_OPAM =~ /^available: ocaml-version >= "(.*)"$/m
  or die "could not parse ocaml version bound\n";

print <<"EOF";
{ stdenv, lib, fetchgit, fetchurl, makeWrapper,
  ocaml, unzip, ncurses, curl, aspcud
}:

assert lib.versionAtLeast ocaml.version "$OCAML_MIN_VERSION";

let
  srcs = {
EOF

my %urls = ();
my %md5s = ();

open(SOURCES, "-|", "curl", "-L", "--url", "$OPAM_BASE_URL/src_ext/Makefile.sources");
while (<SOURCES>) {
  if (/^URL_(?!PKG_)([-\w]+)\s*=\s*(\S+)$/) {
    $urls{$1} = $2;
  } elsif (/^MD5_(?!PKG_)([-\w]+)\s*=\s*(\S+)$/) {
    $md5s{$1} = $2;
  }
}
for my $src (sort keys %urls) {
  my ($sha256,$store_path) = split /\n/, `nix-prefetch-url --print-path \Q$urls{$src}\E`;
  system "echo \Q$md5s{$src}\E' *'\Q$store_path\E | md5sum -c 1>&2";
  die "md5 check failed for $urls{$src}\n" if $?;
  print <<"EOF";
    $src = fetchurl {
      url = "$urls{$src}";
      sha256 = "$sha256";
    };
EOF
}

print <<"EOF";
    opam = fetchurl {
      url = "https://github.com/ocaml/opam/archive/$OPAM_RELEASE.zip";
      sha256 = "$OPAM_RELEASE_SHA256";
    };
  };
in stdenv.mkDerivation rec {
  name = "opam-\${version}";
  version = "$OPAM_RELEASE";

  buildInputs = [ unzip curl ncurses ocaml makeWrapper];

  src = srcs.opam;

  postUnpack = ''
EOF
for my $src (sort keys %urls) {
  my($ext) = $urls{$src} =~ /(\.(?:t(?:ar\.|)|)(?:gz|bz2?))$/
    or die "could not find extension for $urls{$src}\n";
  print <<"EOF";
    ln -sv \${srcs.$src} \$sourceRoot/src_ext/$src$ext
EOF
}
print <<'EOF';
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
EOF
