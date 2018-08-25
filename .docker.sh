#!/usr/bin/env bash
DISTRO=${DISTRO:-alpine-3.7}
OCAML_VERSION=${OCAML_VERSION:-4.07}

set -ex
# TODO opam2 depext
case $DISTRO in
alpine-*) sudo apk add m4 ;;
debian-*) sudo apt update; sudo apt -y install m4 pkg-config ;;
ubuntu-*) sudo apt update; sudo apt -y install m4 pkg-config ;;
fedora-*) sudo yum -y install m4 pkg-config m4 ;;
esac

sudo chown -R opam /home/opam/src
cd /home/opam/src
opam install -y dune ocamlfind 
cd /home/opam/src
make
