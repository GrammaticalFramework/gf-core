{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.05";
    systems.url = "github:nix-systems/default";
    devenv.url = "github:cachix/devenv";
  };

  nixConfig = {
    extra-trusted-public-keys =
      "devenv.cachix.org-1:w1cLUi8dv3hnoSPGAuibQv+f9TZLr6cv/Hm9XgU50cw=";
    extra-substituters = "https://devenv.cachix.org";
  };

  outputs = { self, nixpkgs, devenv, systems, ... }@inputs:
    let forEachSystem = nixpkgs.lib.genAttrs (import systems);
    in {
      packages = forEachSystem (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          haskellPackages = pkgs.haskell.packages.ghc925.override {
            overrides = self: _super: {
              cgi = pkgs.haskell.lib.unmarkBroken (pkgs.haskell.lib.dontCheck
                (self.callHackage "cgi" "3001.5.0.1" { }));
            };
          };

        in {
          devenv-up = self.devShells.${system}.default.config.procfileScript;
          gf = pkgs.haskell.lib.overrideCabal
            (haskellPackages.callCabal2nixWithOptions "gf" self "--flag=-server"
              { }) (_old: {
                # Fix utf8 encoding problems
                patches = [
                  # Already applied in master
                  # (
                  #   pkgs.fetchpatch {
                  #     url = "https://github.com/anka-213/gf-core/commit/6f1ca05fddbcbc860898ddf10a557b513dfafc18.patch";
                  #     sha256 = "17vn3hncxm1dwbgpfmrl6gk6wljz3r28j191lpv5zx741pmzgbnm";
                  #   }
                  # )
                  ./nix/expose-all.patch
                  ./nix/revert-new-cabal-madness.patch
                ];
                jailbreak = true;
                # executableSystemDepends = [
                #   (pkgs.ncurses.override { enableStatic = true; })
                # ];
                # executableHaskellDepends = [ ];
              });
        });

      devShells = forEachSystem (system:
        let pkgs = nixpkgs.legacyPackages.${system};

        in with pkgs.darwin.apple_sdk.frameworks;
        let
          darwinPkgs =

            if pkgs.stdenv.isDarwin then [ Cocoa pkgs.clang ] else [ ];
        in {

          defaultPackage = self.packages.${system}.${packageName};

          default = devenv.lib.mkShell {
            inherit inputs pkgs;
            modules = [{
              # https://devenv.sh/basics/
              env.GREET = "devenv";

              # https://devenv.sh/packages/
              packages = [ pkgs.git pkgs.llvm ] ++ darwinPkgs;

              # https://devenv.sh/scripts/
              scripts.hello.exec = "echo hello from $GREET";

              enterShell = ''
                hello
                git --version
              '';

              languages.haskell = {
                enable = true;
                # HLS fails to build on darwin and 9.2.5
                languageServer = null;
                package = pkgs.haskell.compiler.ghc925;
              };

              # https://devenv.sh/pre-commit-hooks/
              pre-commit.hooks = {
                # lint shell scripts
                shellcheck.enable = true;
                markdownlint.enable = true;
                # lint nix
                nixfmt.enable = true;
                deadnix.enable = true;
                nil.enable = true;
                # statix.enable = true;
                # format haskell
                # ormolu.enable = true;
                # cabal-fmt.enable = true;
                # lint haskell
                # hlint.enable = true;
              };
              # https://devenv.sh/processes/
              # processes.ping.exec = "ping example.com";

              # See full reference at https://devenv.sh/reference/options/
            }];
          };
        });
    };
}
