# OCaml Build Infrastructure

This repository contains the scripts, libraries and command-line tools to
access the opam2 bulk build infrastructure that checks on the health of the
[opam](https://opam.ocaml.org) package manager.

The main services and repositories associates with this infrastructure are:

- **Documentation:**
  - <https://github.com/ocaml/infrastructure/wiki>
  - <https://github.com/ocaml/infrastructure/wiki/Containers> is rebuilt automatically with the latest information
- **GitHub:** Git repositories
  - <https://github.com/ocaml/obi>: for the source code
  - <https://github.com/ocaml/obi-logs>: for the build logs
- **Docker Hub:** container images
  - <https://hub.docker.com/r/ocaml/opam2>: opam2 and OCaml compiler images
  - <https://hub.docker.com/r/ocaml/opam2-staging>: intermediate container images for bulk builds
- **Coordination:**
  - <https://buildkite.com/ocaml>: the coordination Hub (account required until [buildkite#137](https://github.com/buildkite/feedback/issues/137) is resolved)

## Getting Started

The main tool you will want to try out is `opam-ci`, which provides
CLI access to build results. You can get this by:

```
opam pin add -n obi https://github.com/ocaml/obi.git
opam pin add opam-ci https://github.com/ocaml/obi.git
```

and then try it out via:

```
opam-ci --help
opam-ci status --help
opam-ci logs --help
```

## Further Information

- [CHANGES.md](CHANGES.md) is the repository changelog for source code and
  Buildkite scripts.
- [METADATA.md](METADATA.md) contains a changelog for the Obi sexp format
  that is published on <https://github.com/ocaml/obi-logs/tree/index>.

While we are assembling the documentation, please contact @avsm for more information.
