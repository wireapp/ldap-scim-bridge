{
  description = "Nix config for ldap-scim-bridge";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-24.11";
    # Recent workflow linters (actionlint, zizmor), independent of the build.
    nixpkgs-lint.url = "nixpkgs/nixos-26.05";
    flake-utils.url = "github:numtide/flake-utils";
    tom-bombadil.url = "github:wireapp/tom-bombadil";
  };

  outputs =
    { self
    , nixpkgs
    , nixpkgs-lint
    , flake-utils
    , tom-bombadil
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };
        lintPkgs = import nixpkgs-lint { inherit system; };

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

        # The checks of workflows-lint.yml, also run by `nix flake check`.
        lint-workflows = lintPkgs.writeShellApplication {
          name = "lint-workflows";
          runtimeInputs = [ lintPkgs.actionlint lintPkgs.shellcheck lintPkgs.zizmor ];
          text = ''
            config=()
            if [[ -f .github/actionlint.yaml ]]; then
              config=(-config-file .github/actionlint.yaml)
            fi
            actionlint "''${config[@]}" .github/workflows/*.yml
            zizmor --offline --min-severity medium --min-confidence medium .github/workflows
          '';
        };

        workflowsSrc = pkgs.lib.fileset.toSource {
          root = ./.;
          fileset = ./.github;
        };

        # ldap-scim-bridge currently can only be built with GHC 9.6. However,
        # providing environments with other GHC versions is a first step to fix
        # the issues...
        ghcVersions = [ "ghc92" "ghc94" "ghc96" "ghc98" "ghc910" ];

        haskellPackagesFor = ghc_version: pkgs.haskell.packages.${ghc_version}.override haskellPackagesOverlay;
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

        bomDependencies = tom-bombadil.lib.${system}.bomDependenciesDrv pkgs [ haskellPackages.ldap-scim-bridge ] haskellPackages;
      in
      {
        packages = {
          default = haskellPackages.ldap-scim-bridge;
          inherit bomDependencies;
        };

        # Development shell with required dependencies
        devShells = {
          default = additionalDevShells.ghc96;
          # The tools of the GitHub workflows: the linters, and skopeo and
          # crane for the image pushes (`nix develop .#ci --command ...`).
          ci = lintPkgs.mkShell {
            packages = [ lint-workflows lintPkgs.actionlint lintPkgs.shellcheck lintPkgs.zizmor lintPkgs.skopeo lintPkgs.crane ];
          };
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
          workflows =
            pkgs.runCommand "workflows-lint"
              {
                nativeBuildInputs = [ lint-workflows ];
              }
              ''
                export HOME=$TMPDIR
                cd ${workflowsSrc}
                lint-workflows
                touch $out
              '';
        };

        packages.dockerImage =
          let
            # CI passes the build version and, for dev builds, the Quay
            # expiration (`nix build --impure`). A pure build gets the
            # placeholders: `0.0.0-dev` and no expiration.
            imageVersion =
              let v = builtins.getEnv "LSB_IMAGE_VERSION"; in if v == "" then "0.0.0-dev" else v;
            expiresAfter = builtins.getEnv "LSB_IMAGE_EXPIRES_AFTER";
            # The commit date, not the build time: same commit, same labels.
            d = self.lastModifiedDate or "19700101000000";
            created = "${builtins.substring 0 4 d}-${builtins.substring 4 2 d}-${builtins.substring 6 2 d}T${builtins.substring 8 2 d}:${builtins.substring 10 2 d}:${builtins.substring 12 2 d}Z";
          in
          pkgs.dockerTools.buildImage {
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
              # OCI labels of ADR-0020 rule 8.
              Labels = {
                "org.opencontainers.image.title" = "ldap-scim-bridge";
                "org.opencontainers.image.description" = "Sync LDAP users to Wire through its SCIM API";
                "org.opencontainers.image.source" = "https://github.com/wireapp/ldap-scim-bridge";
                "org.opencontainers.image.url" = "https://github.com/wireapp/ldap-scim-bridge";
                "org.opencontainers.image.vendor" = "Wire GmbH";
                "org.opencontainers.image.licenses" = "AGPL-3.0-or-later";
                "org.opencontainers.image.version" = imageVersion;
                "org.opencontainers.image.revision" = self.rev or self.dirtyRev or "unknown";
                "org.opencontainers.image.created" = created;
              } // pkgs.lib.optionalAttrs (expiresAfter != "") {
                "quay.expires-after" = expiresAfter;
              };
            };
          };
      }
    );
}
