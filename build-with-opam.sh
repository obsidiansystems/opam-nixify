#!/usr/bin/env bash

set -e

if [ ! -d nixpkgs-tbxz ]; then
  wget https://github.com/xplat/nixpkgs/archive/tbxz.zip
  unzip tbxz.zip
  rm -f tbxz.zip
fi

if [ ! -d tezos ]; then
  git clone git@gitlab.com:tezos/tezos.git tezos
fi

export NIX_PATH="nixpkgs=$PWD/nixpkgs-tbxz:$NIX_PATH"
nix-shell -p 'ocaml-ng.ocamlPackages_4_06.callPackage ./handy/opam2.nix {}' -p 'ocaml-ng.ocamlPackages_4_06.callPackage ./handy/jbuilder.nix {}' -p 'ocaml-ng.ocamlPackages_4_06.ocaml' -p bubblewrap -p m4 -p perl -p pkgconfig -p which -p libev -p hidapi -p gmp --run "$(cat <<'EOF'
  export NIX_PATH="nixpkgs=$PWD/nixpkgs-tbxz:$NIX_PATH"
  if [ ! -d ~/.opam ]; then
    opam init --no-setup
  fi

  cd tezos
  make build-deps
  eval $(opam env)
  make
EOF
)"
