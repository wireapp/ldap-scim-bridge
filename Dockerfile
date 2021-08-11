FROM haskell:8.8.3-buster

WORKDIR /opt/ldap-scim-bridge

RUN cabal v2-update

# Add just the .cabal file to capture dependencies
COPY ./ldap-scim-bridge.cabal /opt/ldap-scim-bridge/ldap-scim-bridge.cabal
COPY ./stack.yaml /opt/ldap-scim-bridge/stack.yaml
COPY ./stack.yaml.lock /opt/ldap-scim-bridge/stack.yaml.lock

# Docker will cache this command as a layer, freeing us up to
# modify source code without re-installing dependencies
# (unless the .cabal file changes!)
RUN stack build --only-dependencies -j4

# Add and Install Application Code
COPY . /opt/ldap-scim-bridge
RUN stack install

CMD ["ldap-scim-bridge"]