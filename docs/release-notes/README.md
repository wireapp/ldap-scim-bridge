# Release notes

One Markdown file per GA version, `<X.Y.Z>.md` (or `<X.Y>.md` for a whole
minor line), written by hand and merged on the release's source branch before
the release is prepared. It is for the people who operate ldap-scim-bridge:
what changed for them, and what to do when upgrading.

`.github/workflows/prepare-release.yml` reads it:

- a GA without its note fails to prepare: the release body is written once and
  never rewritten;
- a pre-release (`X.Y.Z-rc.N`) uses the note of its GA when it exists, and
  says so in its body otherwise;
- when the release manager sends a note (`release_note` input), that note is
  used for the run instead, and is never committed.

The per-commit changelog is generated from the history; the note does not
repeat it.
