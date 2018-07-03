#!/bin/bash -ex

tag=$1
arch=$2
ov=$3
distro=$4
rev=$5

rm -rf $tag
buildkite-agent artifact download "$tag/results/*" .
echo $arch > $tag/arch
echo $ov > $tag/ov
echo $distro > $tag/distro
echo $rev > $tag/rev
tar -jcvf results-$tag.tar.bz2 $tag
buildkite-agent artifact upload results-$tag.tar.bz2
if [ -d obi-logs ]; then
  git -C obi-logs clean -dxf
  git -C obi-logs fetch git@github.com:ocaml/obi-logs
  git -C obi-logs reset --hard origin/builds
else
  git clone -b builds --depth=1 git@github.com:ocaml/obi-logs
fi
docker pull ocaml/opam2-staging:obi-buildkite
docker run -it -v `pwd`/$tag:/home/opam/$tag -v `pwd`/obi-logs:/home/opam/obi-logs ocaml/opam2-staging:obi-buildkite obi-buildkite process -vv -i /home/opam/$tag -o /home/opam/obi-logs
ssh-add -D && ssh-add ~/.ssh/id_rsa.bulk && ssh-add -l
cd obi-logs && find . -type f && git add . && git pull --commit && git commit -m "update $tag" && git push -u origin builds
rm -rf obi-logs
