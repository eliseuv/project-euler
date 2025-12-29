{
  description = "Haskell Dev Environment";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      ...
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };

        systemLibs = with pkgs; [
          zlib
          zlib.dev
          ncurses
          gmp
        ];

        hpkgs = pkgs.haskell.packages.ghc96;
        ghcPackages = with hpkgs; [
          ghc
          haskell-language-server
        ];

      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs =
            # Libraries needed for dynamic linking
            systemLibs
            # Main GHC packages
            ++ ghcPackages
            # Haskell tools
            ++ (with pkgs; [
              cabal-install
              haskellPackages.implicit-hie
              ghcid
              ghciwatch
              ormolu
            ]);

          # Fix for common linking errors in C libraries
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath systemLibs;

          shellHook = ''
            # Generate hie.yaml for LSP support
            ${pkgs.lib.getExe pkgs.haskellPackages.implicit-hie} > hie.yaml
          '';
        };
      }
    );
}
