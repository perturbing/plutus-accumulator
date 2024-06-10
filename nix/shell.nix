{ repoRoot, inputs, pkgs, system, lib }:

cabalProject:

{
  name = "plutus-accumulator";

  packages = lib.traceSeq inputs.CHaP [
    pkgs.cargo
    pkgs.rustc
    pkgs.rustfmt
    pkgs.jq
    pkgs.m4
  ];

  preCommit = {
    cabal-fmt.enable = true;
    cabal-fmt.extraOptions = "--no-tabular";
    nixpkgs-fmt.enable = true;
    shellcheck.enable = true;
    fourmolu.enable = true;
    fourmolu.extraOptions = "-o -XCPP";
    hlint.enable = true;
  };

  tools = {
    haskell-language-server =
      let
        hlsProject = pkgs.haskell-nix.cabalProject' {
          name = "haskell-language-server";
          src = inputs.iogx.inputs.haskell-nix.inputs."hls-2.6";
          configureArgs = "--disable-benchmarks --disable-tests";
          compiler-nix-name = lib.mkDefault "ghc96";
          modules = [ ];
        };
      in
      hlsProject.hsPkgs.haskell-language-server.components.exes.haskell-language-server;
  };

  scripts = {
    rustfmt-repo = {
      description = "rustfmt the repo";
      group = "general";
      exec = ''
        set -e
        set -u
        set -o pipefail

        cd $(git rev-parse --show-toplevel)
        find . -name "*.rs" | xargs rustfmt
      '';
    };

  };

  shellHook = ''
  '';
}
