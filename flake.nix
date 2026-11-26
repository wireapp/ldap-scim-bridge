{
  description = "Nix config for ldap-scim-bridge";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-24.11";
    # Recent workflow linters (actionlint, zizmor), independent of the build.
    nixpkgs-lint.url = "nixpkgs/nixos-26.05";
    flake-utils.url = "github:numtide/flake-utils";
    tom-bombadil.url = "github:wireapp/tom-bombadil";
    # hscim and the wire-server libraries it needs (schema-profunctor, imports,
    # wai-utilities, types-common, ...) come from wire-server's Haskell package
    # set.  So do the third-party packages wire-server replaces with git forks
    # or fixed Hackage versions (nix/haskell-pins.nix there), e.g. tinylog, cql,
    # warp.  Pinned to master (2026-10-26).
    # Not a flake input: wire-server's flake requires its submodules, one of
    # which is private.  flake-compat evaluates it with wire-server's own
    # flake.lock, but without submodules.
    wire-server = {
      url = "github:wireapp/wire-server/5239cf4eef81bf02ca6ed18df39f8e86a93d2116";
      flake = false;
    };
    flake-compat = {
      # needed for wire-server (see above)
      url = "github:NixOS/flake-compat";
      flake = false;
    };
  };

  outputs =
    { self
    , nixpkgs
    , nixpkgs-lint
    , flake-utils
    , tom-bombadil
    , wire-server
    , flake-compat
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };
        lintPkgs = import nixpkgs-lint { inherit system; };

        wireServerPackages = (import flake-compat { src = wire-server; }).outputs.packages.${system};
        wireServer = wireServerPackages.wireServer;
        haskellPackages = wireServer.haskellPackages.extend (self: super: {
          ldap-scim-bridge = super.developPackage {
            root = ./.;
          };
        });

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

        # The GHC version is the one of wire-server's package set.
        devShell = haskellPackages.shellFor {
          packages = p: [ p.ldap-scim-bridge ];
          withHoogle = true;
          buildInputs = [
            pkgs.ghcid
            pkgs.statix
            statix-command
            wireServerPackages.pkgs.haskell-language-server
          ] ++ formatters;
        };

        # `cabal build` and `cabal test` in CI: no HLS, hoogle nor ghcid, which
        # are not in cache.nixos.org for wire-server's package set and take long
        # to build.  cabal-install is pinned here instead of coming from the
        # runner image.
        buildShell = haskellPackages.shellFor {
          packages = p: [ p.ldap-scim-bridge ];
          buildInputs = [ pkgs.cabal-install ];
        };

        # tom-bombadil evaluates every package of the set it gets, and some of
        # wire-server's don't evaluate (missing submodules), so pass it only the
        # Haskell closure of ldap-scim-bridge.
        bomHaskellPackages = builtins.listToAttrs (map
          ({ key, drv }: { name = key; value = drv; })
          (builtins.genericClosure {
            startSet = [{ key = "ldap-scim-bridge"; drv = haskellPackages.ldap-scim-bridge; }];
            operator = { drv, ... }: map
              (d: { key = d.pname; drv = d; })
              (wireServerPackages.pkgs.haskell.lib.getHaskellBuildInputs drv);
          }));
        bomDependencies = tom-bombadil.lib.${system}.bomDependenciesDrv wireServerPackages.pkgs [ haskellPackages.ldap-scim-bridge ] bomHaskellPackages;
      in
      {
        packages = {
          default = haskellPackages.ldap-scim-bridge;
          inherit bomDependencies;
        };

        # Development shell with required dependencies
        devShells = {
          default = devShell;
          build = buildShell;
          # The tools of the GitHub workflows: the linters, and skopeo and
          # crane for the image pushes (`nix develop .#ci --command ...`).
          ci = lintPkgs.mkShell {
            packages = [ lint-workflows lintPkgs.actionlint lintPkgs.shellcheck lintPkgs.zizmor lintPkgs.skopeo lintPkgs.crane ];
          };
        };

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
