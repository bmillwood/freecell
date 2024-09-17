{ nixpkgs ? <nixpkgs>
, config ? {}
}:

let
  pkgs = import nixpkgs config;
  inherit (pkgs) stdenv;
in
stdenv.mkDerivation {
  name = "freecell";
  src = ./.;
  buildInputs = [
    pkgs.elmPackages.elm
  ];
  configurePhase = pkgs.elmPackages.fetchElmDeps {
    elmPackages = import ./elm-srcs.nix;
    elmVersion = "0.19.1";
    registryDat = ~/.elm/0.19.1/packages/registry.dat;
  };
  buildPhase = ''
    wc -c $ELM_HOME/0.19.1/packages/registry.dat
    mkdir "$out"
    cp index.html "$out/"
    elm make --output="$out/elm.js" src/Main.elm
  '';
}
