nix-shell -p opam 'ocaml-ng.ocamlPackages_4_06.callPackage(import handy/utop.nix) {}' 'ocaml-ng.ocamlPackages_4_06.callPackage(import handy/jbuilder.nix) {}' 'ocaml-ng.ocamlPackages_4_06.ocaml'
