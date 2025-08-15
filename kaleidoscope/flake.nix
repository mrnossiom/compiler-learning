{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";

    rust-overlay.url = "github:oxalica/rust-overlay";
    rust-overlay.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, rust-overlay }:
    let
      inherit (nixpkgs.lib) genAttrs;

      forAllSystems = genAttrs [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" ];
      forAllPkgs = function: forAllSystems (system: function pkgs.${system});

      pkgs = forAllSystems (system: (import nixpkgs {
        inherit system;
        overlays = [ (import rust-overlay) ];
      }));
    in
    {
      formatter = forAllPkgs (pkgs: pkgs.nixpkgs-fmt);

      devShells = forAllPkgs (pkgs:
        with pkgs.lib;
        let
          file-rust-toolchain = pkgs.rust-bin.fromRustupToolchainFile ./rust-toolchain.toml;
          rust-toolchain = file-rust-toolchain.override { extensions = [ "rust-analyzer" ]; };
        in
        {
          default = pkgs.mkShell rec {
            nativeBuildInputs = with pkgs; [
              pkg-config
              rust-toolchain
              act

              llvmPackages.llvm.dev

              cargo-insta
            ];

            buildInputs = with pkgs; [
              libffi
              libxml2
              libz
              ncurses
              stdenv.cc.cc
            ];

            RUST_SRC_PATH = pkgs.rustPlatform.rustLibSrc;
            LD_LIBRARY_PATH = makeLibraryPath buildInputs;

            LLVM_SYS_181_PREFIX = pkgs.llvmPackages_18.llvm.dev;
            RUST_LOG = "debug";
            RUST_BACKTRACE = "1";
          };
        });
    };
}
