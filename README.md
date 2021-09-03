## ldap-scim-bridge

[![GitHub CI](https://github.com/fisx/ldap-scim-bridge/workflows/CI/badge.svg)](https://github.com/fisx/ldap-scim-bridge/actions)
[![Hackage](https://img.shields.io/hackage/v/ldap-scim-bridge.svg?logo=haskell)](https://hackage.haskell.org/package/ldap-scim-bridge)
[![Stackage Lts](http://stackage.org/package/ldap-scim-bridge/badge/lts)](http://stackage.org/lts/package/ldap-scim-bridge)
[![Stackage Nightly](http://stackage.org/package/ldap-scim-bridge/badge/nightly)](http://stackage.org/nightly/package/ldap-scim-bridge)
[![AGPL-3.0-only license](https://img.shields.io/badge/license-AGPL--3.0--only-blue.svg)](LICENSE)

## this is work in progress.  use at your own risk?

## intro

This is a small command line tool to pull data from an LDAP server and
push it to a SCIM peer.  It supports only  fields `externalId`,
`userName`, `emails`, in the `User` schema and no `Group`s.

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

## future work

Support more SCIM attributes and extensions.
Specifically, for
[wire-server](https://github.com/wireapp/wire-server), we may want to
add rich profiles:

```
      { ...,
        "urn:wire:scim:schemas:profile:1.0": {
          "richInfo": {
            "version": 0,
            "fields": [
              {
                "value": "hair color",
                "type": "green"
              },
              {
                "value": "title",
                "type": "galactic overlord"
              }
            ]
          }
        },
        "urn:ietf:params:scim:schemas:extension:wire:1.0:User": {
          "hair color": "green",
          "title": "galactic overlord"
        },
        ...
      }
```

## further reading

- [https://devconnected.com/how-to-setup-openldap-server-on-debian-10/](https://devconnected.com/how-to-setup-openldap-server-on-debian-10/)
- [https://www.lepide.com/how-to/restore-deleted-objects-in-active-directory.html](https://www.lepide.com/how-to/restore-deleted-objects-in-active-directory.html)
