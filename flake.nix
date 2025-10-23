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
              # We use the `libvterm-neovim` fork because the upstream
              # `libvterm` hardcodes certain GNU programs with a "g" prefix.
              # This assumes a Homebrew-style packaging convention and is not
              # suitable for nix.
              libvterm-neovim
              gnumake
            ]
            ++ lib.optional stdenv.isLinux gcc
            ++ lib.optional stdenv.isDarwin clang;
          cc = if pkgs.stdenv.isLinux then pkgs.gcc else pkgs.clang;

          buildVterm = pkgs.writeShellApplication {
            name = "milanglacier-build-vterm";
            inherit runtimeInputs;
            text = ''
              export CC="${cc}/bin/cc"

              export CMAKE_PREFIX_PATH="$${CMAKE_PREFIX_PATH:-${pkgs.cmake}}"

              export CMAKE_PREFIX_PATH="${pkgs.libvterm-neovim}:$CMAKE_PREFIX_PATH"

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

          buildPdfTools = pkgs.writeShellApplication {
            name = "milanglacier-build-pdftools";
            runtimeInputs = [ ];
            text = ''
              set -euo pipefail

              # Ensure we are in the repo root; nix runs from CWD.
              if ! cd straight/build/pdf-tools/build; then
              echo "Error: directory straight/build/pdf-tools/build not found relative to current directory." >&2
              exit 1
              fi

              # Check if ./server/autobuild is executable
              if [ ! -x "./server/autobuild" ]; then
                echo "Error: ./server/autobuild is not executable." >&2
                exit 1
              fi

              # Run autobuild
              ./server/autobuild -i "$(cd .. && pwd)" --os nixos
            '';
          };

          build = pkgs.writeShellApplication {
            name = "milanglacier-build-all";
            text = ''
              set -euo pipefail
              "${buildVterm}/bin/milanglacier-build-vterm" "$@"
              "${buildPdfTools}/bin/milanglacier-build-pdftools" "$@"
            '';
          };
        in
        {
          build-vterm = {
            type = "app";
            program = "${buildVterm}/bin/milanglacier-build-vterm";
          };

          build-pdftools = {
            type = "app";
            program = "${buildPdfTools}/bin/milanglacier-build-pdftools";
          };

          build = {
            type = "app";
            program = "${build}/bin/milanglacier-build-all";
          };
        }
      );
    };
}
