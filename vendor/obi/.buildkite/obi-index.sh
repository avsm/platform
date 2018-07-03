#!/bin/bash -ex

echo --- Cloning opam repository
cd /home/opam
git -C opam-repository pull origin master
opam update
echo --- Cloning obi-logs
rm -rf obi-logs
git clone -b builds --depth=1 git://github.com/ocaml/obi-logs
echo --- Generating index
obi-buildkite index -i obi-logs -r opam-repository -vv > index.sxp
echo --- Uploading result
buildkite-agent artifact upload index.sxp
cd obi-logs && buildkite-agent artifact upload maintainers.sxp && buildkite-agent artifact upload tags.sxp
