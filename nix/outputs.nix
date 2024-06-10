{ repoRoot, inputs, pkgs, system, lib }:
let
  haskell-project = repoRoot.nix.haskell-project;
  rust-project = {
    packages = repoRoot.nix.rust-project;
  };

in
[
  (haskell-project.flake)
  (rust-project)
]
