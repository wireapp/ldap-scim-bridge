{{/*
Fail the render when a key the bridge's config parser requires is missing from
`config` (src/LdapScimBridge.hs). Not called when `existingSecret` provides
the whole config file.
*/}}
{{- define "ldap-scim-bridge.validateConfig" -}}
{{- $config := .Values.config | default dict -}}
{{- $required := list
  "logLevel"
  "ldapSource.tls" "ldapSource.host" "ldapSource.port" "ldapSource.dn"
  "ldapSource.password" "ldapSource.codec"
  "ldapSource.search.base" "ldapSource.search.objectClass"
  "scimTarget.tls" "scimTarget.host" "scimTarget.port" "scimTarget.path"
  "scimTarget.token"
  "mapping.userName" "mapping.externalId" -}}
{{- range $path := $required -}}
{{- $value := $config -}}
{{- range $key := splitList "." $path -}}
{{- if kindIs "map" $value -}}
{{- $value = get $value $key -}}
{{- else -}}
{{- $value = "" -}}
{{- end -}}
{{- end -}}
{{- $_ := required (printf "config.%s is required (or set existingSecret)" $path) $value -}}
{{- end -}}
{{- end -}}
