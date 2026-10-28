# ldap-scim-bridge

Helm chart running [ldap-scim-bridge](https://github.com/wireapp/ldap-scim-bridge)
as a Kubernetes CronJob: every `schedule` (default: every 5 minutes), a pod
reads users from LDAP, pushes them to Wire's SCIM API, then exits. Runs never
overlap.

The chart version, its `appVersion` and the image tag are the same build
version (`0.12.0`, `0.12.0-rc.1`, …). Until 5.x it was published from
wire-server under the wire-server version.

**Always pin `--version`.** The 5.x charts published by wire-server stay in
the same repositories under the same name, and sort above 0.12.0: without
`--version`, Helm picks the highest version, an old wire-server one.

## Install

Releases (GA):

```sh
helm upgrade --install -n wire ldap-scim-bridge \
  oci://quay.io/wire/charts/stable/ldap-scim-bridge --version <version> \
  -f my-values.yaml
```

Pre-releases (`X.Y.Z-rc.N`, …) and dev builds (which expire after 90 days)
are under `oci://quay.io/wire/charts/dev/ldap-scim-bridge`.

Until wire-builds reads OCI, releases are also pushed to the S3 Helm
repositories `https://s3-eu-west-1.amazonaws.com/public.wire.com/charts` (GA)
and `…/charts-develop` (pre-releases), with the same versions.

`my-values.yaml` must provide the bridge configuration under `config` (see
`values.yaml` for the reference and
[docs/configuration.md](../../docs/configuration.md) for every key): the
render fails when an LDAP or SCIM endpoint, a credential or a mapping key is
missing. To keep credentials out of Helm values, create a Secret with the whole
file under the key `config.yaml` and set `existingSecret` instead:

```sh
kubectl create secret generic -n wire ldap-scim-bridge-config \
  --from-file=config.yaml=./config.yaml
helm upgrade --install -n wire ldap-scim-bridge \
  oci://quay.io/wire/charts/stable/ldap-scim-bridge --version <version> \
  --set existingSecret=ldap-scim-bridge-config
```

Behind a registry mirror, set `image.repository` and, if the mirror needs
credentials, `image.pullSecrets`.

## Test deployment with openldap

The `openldap` chart in wire-server is a test instance with preloaded users;
the reference `config` in `values.yaml` matches it.

```sh
helm upgrade --install -n wire openldap <wire-server>/charts/openldap/
helm upgrade --install -n wire ldap-scim-bridge charts/ldap-scim-bridge -f my-values.yaml
```

Each run creates a `ldap-scim-bridge-XXXXXX` pod; read its logs with:

```sh
kubectl get pods -n wire
kubectl logs -n wire ldap-scim-bridge-XXXXXX
```

## Active Directory with a private CA

Put the CA certificate in a ConfigMap:

```sh
kubectl create configmap -n wire ca-ad-pemstore --from-file=ad-root.crt=./ad-root.crt
```

The chart has no extra-volume values yet, so mount it with a patch after each
`helm upgrade` (the next upgrade reverts it):

```sh
kubectl patch cronjob -n wire ldap-scim-bridge --patch-file add-ad-patch.yaml
```

with `add-ad-patch.yaml`:

```yaml
spec:
  jobTemplate:
    spec:
      template:
        spec:
          containers:
            - name: ldap-scim-bridge
              volumeMounts:
                - name: ca-ad-pemstore
                  mountPath: /etc/ssl/certs/ad-root.crt
                  subPath: ad-root.crt
                  readOnly: true
          volumes:
            - name: ca-ad-pemstore
              configMap:
                name: ca-ad-pemstore
```
