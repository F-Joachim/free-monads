{
  description = "A Haskell project with Free Monads";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        ghcVersion = "ghc984";
        pkgs = nixpkgs.legacyPackages.${system};
        haskellPackages = pkgs.haskell.packages.${ghcVersion};
      in
      {
        devShells.default = haskellPackages.developPackage {
          root = ./.;
          modifier = drv: pkgs.haskell.lib.addBuildTools drv [
            haskellPackages.haskell-language-server
            haskellPackages.stack
          ];
        };
      }
    );
}