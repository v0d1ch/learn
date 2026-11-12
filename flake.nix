{
  description = "A development environment with Rust, Go and Haskell toolchains";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";

    # The best and most up-to-date Rust overlay
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
  };

  outputs = { self, nixpkgs, flake-utils, rust-overlay }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ rust-overlay.overlays.default ];
        };

        # You can pin a specific version or just use the latest stable
        rustToolchain = pkgs.rust-bin.stable.latest.default.override {
          extensions = [ "rust-analyzer" "rust-src" ];
        };

      in {
        devShells.default = pkgs.mkShell {
          name = "rust-go-haskell-dev";

          # Make the Rust, Go and Haskell toolchains available in the shell.
          nativeBuildInputs = [
            rustToolchain
            pkgs.go
            pkgs.gopls
            pkgs.haskell.compiler.ghc967
            pkgs.cabal-install
            pkgs.gitMinimal
          ];

          # Supporting libraries and tools for native builds.
          buildInputs = with pkgs; [
            pkg-config
            gmp
            zlib
          ];

          # Makes SSL certificates work for cargo crates that fetch git deps, etc.
          shellHook = ''
            export PKG_CONFIG_PATH="${pkgs.openssl.dev}/lib/pkgconfig:$PKG_CONFIG_PATH"
          '';
        };

        # Optional: expose cargo as a default package (so `nix build .#cargo` works)
        packages.cargo = rustToolchain;

        # If you want `nix build` to just give you the toolchain itself
        packages.default = rustToolchain;
      });
}
