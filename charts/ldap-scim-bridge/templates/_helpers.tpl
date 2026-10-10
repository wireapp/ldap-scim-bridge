{{/*
Fail the render when a value the bridge cannot run without is missing from
`config`. Not called when `existingSecret` provides the whole config file.
*/}}
{{- define "ldap-scim-bridge.validateConfig" -}}
{{- $config := .Values.config | default dict -}}
{{- $required := list
  "ldapSource.host" "ldapSource.port" "ldapSource.dn" "ldapSource.password"
  "ldapSource.search.base" "scimTarget.host" "scimTarget.port" "scimTarget.token"
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
