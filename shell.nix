let
  pkgs     = import <nixpkgs> { inherit config overlays; };
  config   = {};
  overlays = [];
  #
  haskp = pkgs.haskellPackages.ghcWithPackages (hp: with hp; [
    ghc-prim
    transformers
    ghc-paths
    ghc-exactprint
  ]);
in
pkgs.mkShell {
  buildInputs = [ haskp ];
}

