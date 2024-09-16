let
  pkgs = import <nixpkgs> { };
  inherit (pkgs) stdenv;
in
stdenv.mkDerivation {
  name = "freecell";
  src = ./.;
  buildInputs = [
    pkgs.elmPackages.elm
  ];
  configurePhase = pkgs.elmPackages.fetchElmDeps {
    elmPackages = {
      "elm/browser" = {
        sha256 = "sha256-I/QUkdMlr8cmSdUSdB+4FzclAUyT5ILSW6szJVVaT1k=";
        version = "1.0.2";
      };
      "elm/core" = {
        sha256 = "sha256-bjexHIjImmjRnQx2JfHvOe1wxZ5EPe+V5N6Y1nSMgKc=";
        version = "1.0.5";
      };
      "elm/html" = {
        sha256 = "1n3gpzmpqqdsldys4ipgyl1zacn0kbpc3g4v3hdpiyfjlgh8bf3k";
        version = "1.0.0";
      };
      "elm/json" = {
        sha256 = "sha256-0GNfMxN+StP8Mj+WuigORdxBr6UQdsU9nwT9ksLPXE4=";
        version = "1.1.3";
      };
      "elm/random" = {
        sha256 = "sha256-tLncmdWgZLxgdoTdFYGZIIvOUcBSG36KUVw2XgoRFo0=";
        version = "1.0.0";
      };
      "elm/time" = {
        sha256 = "0vch7i86vn0x8b850w1p69vplll1bnbkp8s383z7pinyg94cm2z1";
        version = "1.0.0";
      };
      "elm/url" = {
        sha256 = "0av8x5syid40sgpl5vd7pry2rq0q4pga28b4yykn9gd9v12rs3l4";
        version = "1.0.0";
      };
      "elm/virtual-dom" = {
        sha256 = "sha256-m2AuTws+xlJGAX26dnz6c4cGSwZYPIVOaV2YL/pFa/s=";
        version = "1.0.3";
      };
    };
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
