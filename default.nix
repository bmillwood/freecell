{ nixpkgs ? <nixpkgs>
, config ? {}
}:

let
  pkgs = import nixpkgs config;
  inherit (pkgs) stdenv;
  elm = pkgs.elmPackages.elm;
in
stdenv.mkDerivation {
  name = "freecell";
  src = ./.;
  buildInputs = [
    elm
    pkgs.nodejs
  ];
  configurePhase = pkgs.elmPackages.fetchElmDeps {
    elmPackages = import ./elm-srcs.nix;
    elmVersion = elm.version;
    registryDat = ./registry.dat;
  };
  buildPhase = ''
    mkdir "$out"
    cp index.html "$out/"
    elm make --output="$out/elm.js" src/Main.elm
  '';
  doCheck = true;
  checkPhase = ''
    elm make --output=test/check.js test/Check.elm
    node test/run.js
  '';
}
