Opam-Nixify
===========


Build
-----

Install with `opam`:

    opam pin add opam-nixify . -kgit

Or just run

    make

which builds `_build/install/default/bin/opam-nixify`

Code Formatting
---------------

Using the dev version of `ocamlformat`

     opam pin add ocamlformat --dev

then `make fmt`.


Tezos Hacking
-------------

First run `./build-with-opam.sh` if you haven't already.

Open `nix-shell` like this

```
nix-shell -p 'ocaml-ng.ocamlPackages_4_06.callPackage ./handy/opam2.nix {}' -p 'ocaml-ng.ocamlPackages_4_06.callPackage ./handy/jbuilder.nix {}' -p 'ocaml-ng.ocamlPackages_4_06.ocaml' -p bubblewrap -p m4 -p perl -p pkgconfig -p which -p libev -p hidapi -p gmp
```

If the first time, run `opam install opam-client`.

Once the shell is open you want to continue using the nixpkgs fork:

```
export NIX_PATH="nixpkgs=$PWD/nixpkgs-tbxz:$NIX_PATH"
```

and set up opam env:

```
eval $(opam env)
```

When you want to regenerate the nix derivations for opam dependencies run

```
make && (for i in tezos/_opam/.opam-switch/packages/[^no]* tezos/_opam/.opam-switch/packages/n[^o]* tezos/_opam/.opam-switch/packages/o[^c]* tezos/_opam/.opam-switch/packages/oc[^a]* tezos/_opam/.opam-switch/packages/ocaml[^-.]* tezos/_opam/.opam-switch/packages/ocaml-[^b]*; do j=nix/ocaml/"$(basename "${i%%.*([^/])}")"; mkdir -p "$j" && _build/install/default/bin/opam-nixify "$i" >"$j"/default.nix && (shopt -s nullglob; for k in "$i"/files/*; do cp -at "$j" "$k"; done); done)`
```


To build tezos (or some other package in `ocamlworld.nix`) use:

```
nix-build -E '(import ./ocamlworld.nix {}).tezos'
```
