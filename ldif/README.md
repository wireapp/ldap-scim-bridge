Try this to set up some test data:

```
$ apt-get install ldapscripts ldap-utils slapd
$ ldapadd -D "cn=admin,dc=nodomain" -W -H ldapi:/// -f ./<...>.ldif
```

The objectClass `extensibleObject` could be replaced by
`inetOrgPerson` (both works).
