{
  description = "A Template for Haskell Packages";
  inputs.flake-parts.url = "github:hercules-ci/flake-parts";
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  inputs.nix-hs-utils.url = "github:tbidne/nix-hs-utils";
  outputs =
    inputs@{
      flake-parts,
      nixpkgs,
      nix-hs-utils,
      self,
    }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      perSystem =
        { pkgs, ... }:
        let
          hlib = pkgs.haskell.lib;
          ghc-version = "ghc982";
          compiler = pkgs.haskell.packages."${ghc-version}";
          compilerPkgs = {
            inherit compiler pkgs;
          };

          mkPkg =
            returnShellEnv:
            nix-hs-utils.mkHaskellPkg {
              inherit compiler pkgs returnShellEnv;
              name = "todo";
              root = ./.;
            };

          stack-wrapped = pkgs.symlinkJoin {
            name = "stack";
            paths = [ pkgs.stack ];
            buildInputs = [ pkgs.makeWrapper ];
            postBuild = ''
              wrapProgram $out/bin/stack --add-flags "--no-nix --system-ghc"
            '';
          };
        in
        {
          packages.default = mkPkg false;
          devShells = {
            default = mkPkg true;

            stack = pkgs.mkShell {
              buildInputs = [
                compiler.ghc
                pkgs.zlib
                stack-wrapped
              ];
            };
          };

          apps = {
            format = nix-hs-utils.format compilerPkgs;
            lint = nix-hs-utils.lint compilerPkgs;
            lintRefactor = nix-hs-utils.lintRefactor compilerPkgs;
          };
        };
      systems = [
        "aarch64-darwin"
        "aarch64-linux"
        "x86_64-darwin"
        "x86_64-linux"
      ];
    };
}
