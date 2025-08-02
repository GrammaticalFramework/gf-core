{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.05";
    systems.url = "github:nix-systems/default";
  };

  nixConfig = {
    # extra-trusted-public-keys =
    #   "devenv.cachix.org-1:w1cLUi8dv3hnoSPGAuibQv+f9TZLr6cv/Hm9XgU50cw=";
    # extra-substituters = "https://devenv.cachix.org";
  };

  outputs = { self, nixpkgs, systems, ... }@inputs:
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
    };
}
