{
  description = "hspec";
  inputs = {
    # To find a suitable nixpkgs hash with cache, pick one from https://status.nixos.org/
    nixpkgs.url = "github:nixos/nixpkgs/4d60081494259c0785f7e228518fee74e0792c1b";
    flake-utils.url = "github:numtide/flake-utils";
    flake-utils.inputs.nixpkgs.follows = "nixpkgs";
  };
  outputs = inputs@{ self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          # Because: https://zimbatm.com/notes/1000-instances-of-nixpkgs
          pkgs = nixpkgs.legacyPackages.${system};
        in
        {
          devShell = pkgs.mkShell {
            buildInputs = [
              pkgs.ghc
              pkgs.cabal-install
              pkgs.haskellPackages.haskell-language-server
            ];
          };
        }
      );
}
