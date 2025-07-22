# flake.nix
{
  description = "A Haskell project with Free Monads";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        haskellPackages = pkgs.haskell.packages.ghc984;
      in
      {
        devShells.default = haskellPackages.developPackage {
          root = ./.;
          modifier = drv: drv.overrideAttrs (old: {

            nativeBuildInputs = old.nativeBuildInputs ++ (with pkgs; [
              haskell-language-server
              stack
              git
              pkg-config
            ]);
          });
        };
      });
}