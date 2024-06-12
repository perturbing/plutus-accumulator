{ repoRoot, inputs, pkgs, lib, system }:

let
  naersk' = pkgs.callPackage inputs.naersk { };
  cargoProject = naersk'.buildPackage {
    src = ../veritas;
    nativeBuildInputs = [ pkgs.m4 pkgs.pkg-config ];
    PKG_CONFIG_PATH = "${pkgs.openssl.dev}/lib/pkgconfig";
  };

  outputs = {
    veritas = cargoProject // {
      meta = {
        type = "app";
      };
    };
  };

in

outputs
