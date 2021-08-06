# ldap-scim-bridge

[![GitHub CI](https://github.com/fisx/ldap-scim-bridge/workflows/CI/badge.svg)](https://github.com/fisx/ldap-scim-bridge/actions)
[![Hackage](https://img.shields.io/hackage/v/ldap-scim-bridge.svg?logo=haskell)](https://hackage.haskell.org/package/ldap-scim-bridge)
[![Stackage Lts](http://stackage.org/package/ldap-scim-bridge/badge/lts)](http://stackage.org/lts/package/ldap-scim-bridge)
[![Stackage Nightly](http://stackage.org/package/ldap-scim-bridge/badge/nightly)](http://stackage.org/nightly/package/ldap-scim-bridge)
[![AGPL-3.0-only license](https://img.shields.io/badge/license-AGPL--3.0--only-blue.svg)](LICENSE)

# this is work in progress.  do not use!

# notes

https://devconnected.com/how-to-setup-openldap-server-on-debian-10/

IDEA: use csv team download to compute deletees.  do that outside of
this code base, but in the same repo under `/examples/wire.com/`, and
add a field to yaml that points to the downloaded csv file and the
column with the ID for deletion information for all scim peers that do
not implement "get all users" requests.

BETTER IDEA (thanks julia): Ad uses a deleted objects folder, so this
makes sense to use that.
https://www.lepide.com/how-to/restore-deleted-objects-in-active-directory.html.

do we need to change the `Mock` tag for `Scim.User.User` to something
more wire-like?  need to check!

we may have to specify the encoding of ldap values in the yaml config
as well (iso-latin or utf8 or whatever).  ldap-client gives us
bytestring, which means it's not trusing the encoding to be anything
in particular.


specific to wire, but we may want to add this:
  {- TODO:
      schemas:
        - "urn:wire:scim:schemas:profile:1.0"
        - "urn:ietf:params:scim:schemas:extension:wire:1.0:User"
      looking like this:
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
  -}


objectClass `extensibleObject` could be replaced by `iNetOrgPerson`.  both works.


next:
- post to wire
- update
- delete
- done!
