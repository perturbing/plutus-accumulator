{ repoRoot, inputs, pkgs, lib, system }:

let
  toolchain = with inputs.fenix.packages.${system};
    combine [
      latest.rustc
      latest.cargo
    ];

  naersk' = pkgs.callPackage inputs.naersk {
    cargo = toolchain;
    rustc = toolchain;
  };
  cargoProject = naersk'.buildPackage {
    src = ../veritas;
    nativeBuildInputs = with pkgs; [ m4 pkg-config ];
    release = true;
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
