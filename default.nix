{ nixpkgs ? import <nixpkgs> {} }:
with nixpkgs;
nixpkgs.stdenv.mkDerivation {
  name = "env";
  buildInputs = [ 
    nixpkgs.nodejs-12_x
    nixpkgs.yarn
    nixpkgs.purescript
  ];
}
