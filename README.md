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

## use via docker

If you have gotten here as a
[wire-server](https://github.com/wireapp/wire-server) administrator
and want to use this to populate your teams, you can use the docker
image we're building from this repo:

```sh
docker pull quay.io/wire/ldap-scim-bridge:0.5
```

You need to create a config file that contains your setup.  If in doubt, you can start with [this example](./examples/wire-server/conf1.yaml), or [this one](./examples/wire-server/conf2.yaml).  Name the file `config.yaml` and place it into `/config-path`.  Let's say you want to work on release 0.5.  (You can check if there is a `:latest`, but at the time of writing this paragraph, we still have to add that.)

```sh
docker run -it --network=host \
  --mount type=bind,src=/config-path,target=/mnt \
  quay.io/wire/ldap-scim-bridge:0.5 \
  ldap-scim-bridge /mnt/config.yaml
```

This should work fine for Windows if you make sure the file path under `src` points to the right place.  You may need to you `\` instead of `/`.

The connection to wire is not encrypted.  This tool is made for running inside the trusted network the backend is running in.  If you need to protect this connection you can use an off-the-shelf tls tunnel or vpn solution.

The connection to the LDAP source is TLS-encrypted.  If you need to add trusted certificates to the store in `/etc/ssl/certs/`, you can just mount it:

```sh
docker run -it --network=host \
  --mount type=bind,src=/config-path,target=/mnt \
  --mount type=bind,src=/etc/ssl/certs,target=/etc/ssl/certs \
  quay.io/wire/ldap-scim-bridge:0.5 \
  ldap-scim-bridge /mnt/config.yaml
```

## future work

See https://github.com/wireapp/ldap-scim-bridge/issues

## further reading

- [https://devconnected.com/how-to-setup-openldap-server-on-debian-10/](https://devconnected.com/how-to-setup-openldap-server-on-debian-10/)
- [https://www.lepide.com/how-to/restore-deleted-objects-in-active-directory.html](https://www.lepide.com/how-to/restore-deleted-objects-in-active-directory.html)
