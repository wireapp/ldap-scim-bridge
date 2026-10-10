# Configuration

`ldap-scim-bridge` takes a single argument, the path of a YAML config file
(`src/LdapScimBridge.hs:536-540`). It reads **no environment variables**.

In the Helm chart, the file is the `config` value rendered into a Secret and
mounted at `/etc/ldap-scim-bridge/config.yaml`, or an operator-managed Secret
named by `existingSecret` (see `charts/ldap-scim-bridge/values.yaml`). The
chart fails to render when a required key is missing.

A complete example: [`sample-conf.yaml`](../sample-conf.yaml).

## Top level

| Key | Type | Required | Description |
|---|---|---|---|
| `logLevel` | string | yes | `Trace`, `Debug`, `Info`, `Warn`, `Error` or `Fatal` (least noisy) (`:162-171`) |
| `ldapSource` | object | yes | Where the users come from (below) |
| `scimTarget` | object | yes | Where they are pushed (below) |
| `mapping` | object | yes | Which LDAP attribute feeds which SCIM attribute (below) |

## `ldapSource` (`src/LdapScimBridge.hs:78-111`)

| Key | Type | Required | Description |
|---|---|---|---|
| `tls` | bool | yes | Connect with TLS (default TLS settings). Use `false` only inside a trusted network |
| `host` | string | yes | LDAP server host name |
| `port` | int | yes | Usually 389 (plain) or 636 (TLS) |
| `dn` | string | yes | Bind DN, e.g. `cn=admin,dc=nodomain` |
| `password` | string | yes | Bind password |
| `codec` | string | yes | `utf8` or `latin1` (`:96-99`) |
| `search` | object | yes | Search for the users to sync: see *Searches* |
| `deleteOnAttribute` | object | no | `key` and `value` (both required): matching entries are deleted in SCIM (`:113-117`) |
| `deleteFromDirectory` | object | no | A search (see *Searches*): entries found there are deleted in SCIM |

### Searches (`:119-130`)

| Key | Type | Required | Description |
|---|---|---|---|
| `base` | string | yes | Search base DN |
| `objectClass` | string | yes | Object class of the entries |
| any other key | string | no | Extra filter: only entries whose attribute `<key>` equals the value, e.g. `memberOf: "cn=wire,ou=Groups,dc=nodomain"` |

## `scimTarget` (`:141-148`)

| Key | Type | Required | Description |
|---|---|---|---|
| `tls` | bool | yes | Use HTTPS (`:384-389`). Use `false` only inside a trusted network |
| `host` | string | yes | SCIM peer host name (Wire: spar, or the public API host) |
| `port` | int | yes | SCIM peer port |
| `path` | string | yes | Path of the SCIM API, e.g. `/scim/v2` |
| `token` | string | yes | Authorization header value, e.g. `Bearer <token>` (token created in team settings) |

## `mapping` (`:232-238`)

Keys are SCIM attributes, values are LDAP attribute names.

| Key | Required | Description |
|---|---|---|
| `userName` | yes | Wire handle |
| `externalId` | yes | Stable identifier of the user |
| `displayName` | no | Name shown in Wire |
| `email` | no | E-mail address: zero or one value, which must be a valid address |
| `roles` | no | Wire role |

Every other mapped LDAP attribute must have exactly one value on each entry
(`:252-297`). A single entry breaking a rule makes the whole run fail, with an
error naming the entry (`:417`, `:433`).
