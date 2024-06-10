{ repoRoot, inputs, pkgs, lib, system }:

let
  naersk' = pkgs.callPackage inputs.naersk { };
  cargoProject = naersk'.buildPackage {
    src = ../accumulator-proof-server;
    nativeBuildInputs = [ pkgs.m4 pkgs.pkg-config ];
    PKG_CONFIG_PATH = "${pkgs.openssl.dev}/lib/pkgconfig";
  };

  outputs = {
    accumulator-proof-server = cargoProject // {
      meta = {
        type = "app";
      };
    };
  };

in

outputs
