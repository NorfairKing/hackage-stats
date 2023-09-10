{
  description = "hackage-stats-collector";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-23.05";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
  };

  outputs =
    { self
    , nixpkgs
    , pre-commit-hooks
    }:
    let
      system = "x86_64-linux";
      pkgsFor = nixpkgs: import nixpkgs {
        inherit system;
        overlays = [ (import ./nix/overlay.nix) ];
      };
      pkgs = pkgsFor nixpkgs;

      indexTarGz = builtins.fetchurl "hackage.haskell.org/packages/index.tar.gz";
      collector = pkgs.hackage-stats-collector;
      csv = pkgs.runCommand "hackage-stats.csv" { } ''
        ${collector}/bin/hackage-stats-collector ${indexTarGz} > $out
      '';
      pythonEnv = pkgs.python3.withPackages (ps: with ps; [ csv matplotlib ]);
      packagesPerYearPlot = pkgs.stdenv.mkDerivation {
        name = "packages-per-year.svg";
        src = ./packages-per-year.py;
        buildInputs = [ pythonEnv ];
        buildCommand = ''
          python $src ${csv} > $out
        '';
      };
    in
    {
      packages.${system}.default = packagesPerYearPlot;
      checks.${system} = {
        pre-commit = pre-commit-hooks.lib.${system}.run {
          src = ./.;
          hooks = {
            hlint.enable = true;
            hpack.enable = true;
            ormolu.enable = true;
            nixpkgs-fmt.enable = true;
            nixpkgs-fmt.excludes = [ ".*/default.nix" ];
            cabal2nix.enable = true;
          };
        };
      };
      devShells.${system}.default = pkgs.haskellPackages.shellFor {
        name = "hackage-stats-collector-shell";
        packages = p: [ p.hackage-stats-collector ];
        withHoogle = true;
        doBenchmark = true;
        buildInputs = (with pkgs; [
          zlib
          pythonEnv
        ]) ++ (with pre-commit-hooks.packages.${system};
          [
            hlint
            hpack
            nixpkgs-fmt
            ormolu
            cabal2nix
          ]);
        shellHook = self.checks.${system}.pre-commit.shellHook;
      };
    };
}
