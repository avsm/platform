#!/usr/bin/env bash
DISTRO=${DISTRO:-alpine}
OCAML_VERSION=4.07.1
export OPAMYES=1
export OPAMJOBS=3
set -ex
git config --global user.name "Azure Pipelines CI"
git config --global user.email "bactrian@ocaml.org"
case $AGENT_OS in
Linux)
;; 
LinuxDocker) 
  sudo chown -R opam /home/opam/src
  cd /home/opam/src
  git -C /home/opam/opam-repository pull origin master
  opam update
  eval $(opam env)
  ;;
Windows_NT)
  cd ${BUILD_SOURCES_DIR}
  ;;
Darwin)
  brew install ocaml
  ;;
*)
  echo Unknown OS $AGENT_OS
  exit 1
esac

make
