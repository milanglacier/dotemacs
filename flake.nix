{
  description = "Provides build scripts for Emacs packages that require dynamic modules.";

  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

  outputs =
    { self, nixpkgs }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];
      forAllSystems =
        f:
        builtins.listToAttrs (
          map (system: {
            name = system;
            value = f system;
          }) systems
        );
    in
    {
      apps = forAllSystems (
        system:
        let
          pkgs = import nixpkgs { inherit system; };
          lib = pkgs.lib;
          runtimeInputs =
            with pkgs;
            [
              cmake
              libtool
              libvterm-neovim
              gnumake
            ]
            ++ lib.optional stdenv.isLinux gcc
            ++ lib.optional stdenv.isDarwin clang;

          buildVterm = pkgs.writeShellApplication {
            name = "milanglacier-build-vterm";
            inherit runtimeInputs;
            text = ''
              set -euo pipefail

              # Ensure we are in the repo root; nix runs from CWD.
              if ! cd straight/build/vterm; then
              echo "Error: directory straight/builds/vterm not found relative to current directory." >&2
              exit 1
              fi

              # Create build directory (portable via CMake helper)
              cmake -E make_directory build
              cd build

              # Configure and build
              cmake ..
              make
            '';
          };

          build = pkgs.writeShellApplication {
            name = "milanglacier-build-all";
            text = ''
              set -euo pipefail
              "${buildVterm}/bin/milanglacier-build-vterm" "$@"
            '';
          };
        in
        {
          build-vterm = {
            type = "app";
            program = "${buildVterm}/bin/milanglacier-build-vterm";
          };

          build = {
            type = "app";
            program = "${build}/bin/milanglacier-build-all";
          };
        }
      );
    };
}
