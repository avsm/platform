#!/bin/sh -ex

mkdir -p ~/rpmbuild/SPECS ~/rpmbuild/SOURCES
git archive master --prefix=ocaml-platform-0.1/ --format tgz -o ~/rpmbuild/SOURCES/ocaml-platform.tar.gz
cp ocaml-platform.spec ~/rpmbuild/SPECS
rpmbuild -ba ~/rpmbuild/SPECS/ocaml-platform.spec
cp ~/rpmbuild/RPMS/*/* .



