## ldap-scim-bridge

[![GitHub CI](https://github.com/wireapp/ldap-scim-bridge/workflows/CI/badge.svg)](https://github.com/wireapp/ldap-scim-bridge/actions)
[![Hackage](https://img.shields.io/hackage/v/ldap-scim-bridge.svg?logo=haskell)](https://hackage.haskell.org/package/ldap-scim-bridge)
[![Stackage Lts](http://stackage.org/package/ldap-scim-bridge/badge/lts)](http://stackage.org/lts/package/ldap-scim-bridge)
[![Stackage Nightly](http://stackage.org/package/ldap-scim-bridge/badge/nightly)](http://stackage.org/nightly/package/ldap-scim-bridge)
[![AGPL-3.0-only license](https://img.shields.io/badge/license-AGPL--3.0--only-blue.svg)](LICENSE)

## intro

This is a small command line tool to pull data from an LDAP server and
push it to a SCIM peer.  It is currently used in production togethre
with [wire-server](https://github.com/wireapp/wire-server), but is
designed as a more general tool.

It supports fields `externalId`, `userName`, `displayName`, `emails`,
in the `User` schema.  It may not support any other fields, and it
does not support scim `Group`s.

If you extend this to other fields, groups, or other use cases and setups, we
would highly appreciate pull requests, tickets, or emails (no matter how half-baked).

There is a yaml config file that describes both how to reach the LDAP
server (including the desired directory object(s)) and the SCIM peer,
how to map attributes between the two worlds, and anything else that's
needed like log level.

Every communication is logged to stdout.  When called
without arguments, the tool will print out usage info:

```
*** Exception: bad number of arguments: []

usage: ldap-scim-bridge <config.yaml>
see https://github.com/wireapp/ldap-scim-bridge for a sample config.
```

See [ldif](./ldif/README.md) for a few sample user records to play with.
A working example can be found in `./examples/wire-server`.

## usage

If you have gotten here as a
[wire-server](https://github.com/wireapp/wire-server) administrator and want to
use this to populate your teams, you've got three options to run
`ldap-scim-bridge`:

1. Execute the plain executable.
1. Run the Docker (container) image.
1. Deploy it via the Helm chart.

The `ldap-scim-bridge` can be configured to use unencrypted connections (by
setting the configuration options `ldapSource.tls` or `scimTarget.tls` to
`false`.) Please only do so if the connection target (Wire or LDAP server) and
`ldap-scim-bridge` run in the same trusted network. This can be accomplished by
running the components e.g. in the same Kubernetes cluster or using a VPN.

In general, `ldapSource.tls` or `scimTarget.tls` should be set to true in a
production-level setup.

### execute plain executable

The easiest way is to leave building the executable and running it in the right
context to Nix; e.g.:

```sh
nix shell --command bash -c "ldap-scim-bridge examples/wire-server/conf1.yaml"
```

### docker

You can use the docker image we're building from this repo. As we do not
guarantee full backwards-compatibility (there may be breaking changes e.g.
regarding CLI options), please always provide the image's tag. 

```sh
docker pull quay.io/wire/ldap-scim-bridge:$IMAGE_TAG
```

Usually, `IMAGE_TAG` should be the latest release name without the `v`-prefix.
E.g. for `v0.10.3` the `IMAGE_TAG` is `0.10.3`.

You need to create a config file that contains your setup.  If in doubt, you can start with [this example](./examples/wire-server/conf1.yaml), or [this one](./examples/wire-server/conf2.yaml).  Name the file `config.yaml` and place it into `/config-path`.

```sh
docker run -it --network=host \
  --mount type=bind,src=/config-path,target=/mnt \
  quay.io/wire/ldap-scim-bridge:$IMAGE_TAG \
  ldap-scim-bridge /mnt/config.yaml
```

This should work fine for Windows if you make sure the file path under `src` points to the right place.  You may need to you `\` instead of `/`.

If you need to add trusted certificates for TLS-encrypted connections to the store in `/etc/ssl/certs/`, you can just mount it:

```sh
docker run -it --network=host \
  --mount type=bind,src=/config-path,target=/mnt \
  --mount type=bind,src=/etc/ssl/certs,target=/etc/ssl/certs \
  quay.io/wire/ldap-scim-bridge:$IMAGE_TAG \
  ldap-scim-bridge /mnt/config.yaml
```

### helm

There is a Helm chart to deploy `ldap-scim-bridge` to a Kubernetes cluster:
https://github.com/wireapp/wire-server/tree/master/charts/ldap-scim-bridge

Please refer to its documentation.

## developers

For running unit tests, try `cabal test`.

Integration tests is a bit more involved, since we need ldap and
wire-server running.  See `./examples/wire-server/run.sh`.

### Nix

`direnv` (`.envrc`) calls the Nix flake to provide a development environment.
Another way to get into this environment is to call:

```sh
nix develop
```

To develop with a specific GHC version call e.g.:
```sh
nix develop .\#ghc96
```

The Docker image can be build and loaded with:

```sh
nix build .\#dockerImage
docker load -i result
```

To use Treefmt to format the whole project (according to the `./treefmt.toml`)
run:

```sh
nix fmt
```

To validate the Nix code itself:

```sh
nix flake check
```

To update the Nix dependencies:
```sh
nix flake update
```

### CI

Tag your PR with the label `ghc-matrix` to build this package with many
different GHC versions. Most are unfortunately - shame on us! - expected to
fail.

## cutting a new release

- Make sure master is up to date
- `git tag v0.*`
- `git push --tags`

This will trigger a github workflow that builds and uploads the docker
images to quay.io.

- consider releasing to hackage (see `./admin/publish.sh`)
- bump version in cabal file to upcoming version

## future work

See https://github.com/wireapp/ldap-scim-bridge/issues

## further reading

- [https://devconnected.com/how-to-setup-openldap-server-on-debian-10/](https://devconnected.com/how-to-setup-openldap-server-on-debian-10/)
- [https://www.lepide.com/how-to/restore-deleted-objects-in-active-directory.html](https://www.lepide.com/how-to/restore-deleted-objects-in-active-directory.html)
