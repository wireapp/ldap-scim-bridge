## Overview

The bridge itself is unchanged since 0.10.4: same code, same configuration
file. What changes is where its Helm chart comes from, how it is versioned, and
what the chart checks before it deploys anything.

- **The chart moved** from wire-server to this repository and is published at
  `oci://quay.io/wire/charts/stable/ldap-scim-bridge` (pre-releases under
  `oci://quay.io/wire/charts/dev/ldap-scim-bridge`). Until wire-builds reads OCI,
  it is also pushed to the S3 Helm repositories `charts` (GA) and
  `charts-develop` (pre-releases). wire-server no longer ships it.
- **One version for everything:** the chart version, its `appVersion` and the
  image tag are all `6.0.0`. Until now the chart carried the wire-server
  version (5.x) and pinned the image to `0.10.4` in its values.
- **Semantic Versioning** replaces PVP: `X.Y.Z`, pre-releases
  `X.Y.Z-rc.N`, tags `vX.Y.Z`.

## Before upgrading

> **Breaking changes in the chart**
>
> - The chart requires **Kubernetes 1.21** or later (CronJob `batch/v1`).
> - The render **fails** unless `config` holds the whole bridge configuration
>   (LDAP and SCIM endpoints, credentials, mapping, log level), or
>   `existingSecret` names a Secret with a `config.yaml` key. The old chart
>   rendered an empty configuration and the job failed at runtime instead.
> - The chart moves from the wire-server versions (5.x) to the bridge versions,
>   starting at 6.0.0: the bridge itself jumps from 0.10.4 to 6.0.0, with no
>   change to its code or configuration.

## Upgrading from the wire-server chart

1. Check your values against `charts/ldap-scim-bridge/values.yaml`. `config`
   keeps the same structure as before; every key listed in
   [docs/configuration.md](https://github.com/wireapp/ldap-scim-bridge/blob/dev/docs/configuration.md) as required must be set.
   Remove `image.tag: 0.10.4` if you set it: the tag now follows the chart.
2. Upgrade the release in place (same release name, same chart name):

   ```sh
   helm upgrade -n wire ldap-scim-bridge \
     oci://quay.io/wire/charts/stable/ldap-scim-bridge --version 6.0.0 \
     -f my-values.yaml
   ```

3. With an Active Directory CA, apply the certificate patch again after the
   upgrade (see the chart README): every `helm upgrade` reverts it.

To keep the LDAP password and the SCIM token out of Helm values, create a
Secret from your config file and pass `--set existingSecret=<name>` instead of
`config`.

## New in the chart

- `existingSecret`: mount an operator-managed configuration.
- `image.pullSecrets`: optional image pull secrets; none by default, for public
  registries and air-gapped mirrors.
- Every value is documented in `values.yaml`.

## Release process

Dev builds of every merge are published under computed versions such as
`5.0.1-20261028093000.gabcdef0` until 6.0.0, then `6.0.1-…` (charts under
`oci://quay.io/wire/charts/dev/ldap-scim-bridge`, kept 90 days). Releases are
prepared and published from GitHub Actions, approved by the release managers,
and declared to the Wire release manager.
