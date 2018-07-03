#!/usr/bin/env bash
DISTRO=${DISTRO:-alpine-3.6}
VERSIONS=${OCAML_VERSIONS:-4.04.2 4.05.0 4.06.0}

set -ex
# TODO opam2 depext
case $DISTRO in
alpine-*) sudo apk add m4 ;;
debian-*) sudo apt -y install m4 pkg-config ;;
ubuntu-*) sudo apt -y install m4 pkg-config ;;
esac

sudo chown -R opam /home/opam/src
cd /home/opam/src
export OPAMYES=1
export OPAMJOBS=3
opam install --deps-only .
rm -f jbuild-workspace.dev
for v in $VERSIONS; do
  echo "(context ((switch $v)))" >> jbuild-workspace.dev
  opam install --deps-only -t --switch $v .
done

jbuilder build --workspace jbuild-workspace.dev
rm -f jbuild-workspace.dev
