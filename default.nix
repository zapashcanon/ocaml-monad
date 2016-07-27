{ pkgs ? (import <nixpkgs> {}).pkgs
}:

let
  myemacs =
    with pkgs.emacsPackages; with pkgs.emacsPackagesNg; pkgs.emacsWithPackages
      [ haskellMode magit emacsw3m tuaregMode ];
in with pkgs; stdenv.mkDerivation {
  name = "monadlib";
  buildInputs = [
    ocamlPackages.findlib ocamlPackages.ocaml_batteries ocamlPackages.merlin
    ocamlPackages.ocaml_oasis ocamlPackages.ocaml
    opam myemacs ];
  findlib = "${ocamlPackages.findlib}/lib/ocaml/4.01.0/site-lib/";
  merlin  = "${ocamlPackages.merlin}";
}
