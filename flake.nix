{
  description = "Nix config for ldap-scim-bridge";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-24.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    { self
    , nixpkgs
    , flake-utils
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };

        haskellPackagesOverlay = {
          overrides = self: super: {
            hscim = super.callHackageDirect
              {
                pkg = "hscim";
                ver = "0.4.0.6";
                sha256 = "sha256-qGTyfu9nGQDLdUnqCZRfcCRTAgc2fFu6/IomKyh0fbU=";
              }
              { };
            tinylog = pkgs.haskell.lib.markUnbroken (
              pkgs.haskell.lib.overrideSrc super.tinylog {
                src = pkgs.fetchFromGitHub {
                  owner = "wireapp";
                  repo = "tinylog";
                  rev = "9609104263e8cd2a631417c1c3ef23e090de0d09";
                  sha256 = "sha256-htEIJY+LmIMACVZrflU60+X42/g14NxUyFM7VJs4E6w=";
                };
                version = "0.15";
              }
            );
            ldap-scim-bridge = super.developPackage {
              root = ./.;
            };
          };
        };

        haskellPackages = pkgs.haskellPackages.override haskellPackagesOverlay;

        formatters = [
          (pkgs.haskell.lib.justStaticExecutables pkgs.haskellPackages.ormolu)
          (pkgs.haskell.lib.justStaticExecutables pkgs.haskellPackages.cabal-fmt)
          pkgs.nixpkgs-fmt
          pkgs.treefmt
          pkgs.shellcheck
        ];

        treefmt-command = pkgs.writeShellApplication {
          name = "nix-fmt-treefmt";
          text = ''
            exec ${pkgs.treefmt}/bin/treefmt --config-file ./treefmt.toml "$@"
          '';
          runtimeInputs = formatters;
        };

        statix-command = pkgs.writeShellApplication {
          name = "statix-check";
          runtimeInputs = [ pkgs.statix ];
          text = ''
            statix check ${toString ./.} || exit 1
            echo "Statix check passed!"
          '';
        };

        # ldap-scim-bridge currently can only be built with GHC 9.6. However,
        # providing environments with other GHC versions is a first step to fix
        # the issues...
        ghcVersions = [ "ghc92" "ghc94" "ghc96" "ghc98" "ghc910" ];

        haskellPackagesFor = ghc_version: (pkgs.haskell.packages.${ghc_version}).override haskellPackagesOverlay;
        additionalDevShells = builtins.listToAttrs (map
          (ghc_version: {
            name = ghc_version;
            value = (haskellPackagesFor ghc_version).shellFor {
              packages = p: [ p.ldap-scim-bridge ];
              withHoogle = true;
              buildInputs = [
                pkgs.ghcid
                pkgs.statix
                statix-command
                # We need to be careful to not rebuild HLS, because that would be expensive.
                (pkgs.haskell-language-server.override {
                  supportedGhcVersions = [ (pkgs.lib.removePrefix "ghc" ghc_version) ];
                })
              ] ++ formatters;
            };
          })
          ghcVersions);
      in
      {
        packages.default = haskellPackages.ldap-scim-bridge;

        # Development shell with required dependencies
        devShells = {
          default = additionalDevShells.ghc96;
        } // additionalDevShells;

        formatter = treefmt-command;

        checks = {
          statix =
            pkgs.runCommand "statix-check"
              {
                buildInputs = [ statix-command ];
              }
              ''
                statix-check > $out
              '';
        };

        packages.dockerImage = pkgs.dockerTools.buildImage {
          name = "ldap-scim-bridge";
          # Don't use `latest` as tag: The image could easily be confused with
          # `latest` from the image repo.
          tag = "build";
          copyToRoot = pkgs.buildEnv {
            name = "image-root";
            paths = [ (pkgs.haskell.lib.justStaticExecutables haskellPackages.ldap-scim-bridge) ];
            pathsToLink = [ "/bin" ];
          };
          config = {
            Cmd = [ "/bin/ldap-scim-bridge" ];
          };
        };
      }
    );
}
