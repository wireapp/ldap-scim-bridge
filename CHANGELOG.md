# Changelog

Since 6.0.0, `ldap-scim-bridge` follows [Semantic Versioning][1] as the
Wire build standard defines it: GA `X.Y.Z`, pre-releases
`X.Y.Z-<alpha|beta|rc>.N`, git tags `vX.Y.Z`. The image, the Helm chart and its
`appVersion` all carry the same version. Earlier versions used [PVP][3]. The
per-commit changes are in the [GitHub releases][2].

The cabal file holds the base version of the next release (`6.0.0` for
`6.0.0-rc.1` too): cabal versions cannot carry a pre-release suffix.

## 6.0.0

The bridge itself is unchanged since 0.10.4; this release changes how it is
built, versioned and deployed. See
[publication/release-note.md](publication/release-note.md).

- Switch from PVP to Semantic Versioning.
- Move the Helm chart from wire-server to `charts/ldap-scim-bridge`, published
  as `oci://quay.io/wire/charts/stable/ldap-scim-bridge` with the bridge
  version instead of the wire-server version. The bridge goes from 0.10.4
  to 6.0.0, above the 5.x charts wire-server published under the same name.
- Chart: `apiVersion: v2`; the image tag defaults to the chart `appVersion`;
  the bridge configuration is required, or `existingSecret` names a Secret
  holding it; optional image pull secrets; Kubernetes 1.21 or later.
- Document the configuration file in `docs/configuration.md`.
- Build dev versions on every merge and release from GitHub Actions with the
  Wire release workflows; pushing a tag no longer publishes an image.

## 0.11.0

These changes shipped in the `0.10.1` to `0.10.4` images. No image, chart or
Hackage package was published as `0.11.0`. The annotated tag `v5.0.0` marks
the same commit as `v0.11.0`: it is the base of the SemVer versions, so that
6.0.0 sorts above the 5.x charts published from wire-server.

- Migrate project setup and CI to use Nix flake
- Remove old `Dockerfile` (build docker image with Nix)

## 0.10

- More helpful error messages on bad LDAP input records.

## 0.9

- Map ldap attribute to SCIM roles (#26)
- Bump dependencies (#25)

## 0.8

- Fix repo home url in cabal file.
- Tweak README.
- Maintainer script.
- Bump parent docker image.
- Tag docker image as `latest` on release.

## 0.7

- Fix TLS handshake to scim server (#23)

## 0.6

- Work on README (#20)

## 0.5

- Various small fixes (#15)

## 0.4

- Filter for arbitrary LDAP attributes (#16)

## 0.3

- Better error logging. (#11)
- Allow mapping the scim `displayName` attribute. (#14)

## 0.2

* Fix: add `./examples/wire-server/` to hackage distro.

## 0.1

* Working for one non-trivial use-case.  See `./examples/wire-server/`

## 0.0.0.0

* Initially created.

[1]: https://semver.org
[2]: https://github.com/wireapp/ldap-scim-bridge/releases
[3]: https://pvp.haskell.org
