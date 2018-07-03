#!/usr/bin/env bash
DISTRO=${DISTRO:-alpine-3.7}

set -ex
# TODO opam2 depext
case $DISTRO in
alpine-*) sudo apk add m4 ;;
debian-*) sudo apt update; sudo apt -y install m4 pkg-config ;;
ubuntu-*) sudo apt update; sudo apt -y install m4 pkg-config ;;
esac

sudo chown -R opam /home/opam/src
cd /home/opam/src
opam pin add -n jbuilder --dev
opam pin add -y dune https://github.com/ocaml/dune.git
cd /home/opam/src
make
