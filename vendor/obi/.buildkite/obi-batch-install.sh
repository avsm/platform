#!/bin/bash -ex
# Either build an artefact directly or spawn more if there are a lot of packages

hub=$1
tag=$2
arch=$3
pkg=$4

docker pull $hub:$tag
docker run --rm $hub:$tag opam list -s --all-versions --installable $pkg | sort > toinstall.txt
docker run --rm $hub:$tag opam list -s --all-versions $pkg | sort > allpossible.txt
ls -la *.txt
echo toinstall:
cat toinstall.txt
echo allpossible:
cat allpossible.txt
echo diffing:
comm -23 allpossible.txt toinstall.txt > uninstallable.txt

echo --- Checking uninstallable
wc -l uninstallable.txt
echo Uninstallable packages in this switch are:
cat uninstallable.txt

echo --- Checking installable
echo To install:
cat toinstall.txt

numlines=`wc -l < toinstall.txt`

# workaround: bap takes forever to build in serial
if [[ $pkg == bap-* ]]; then
  echo --- Special case for bap
  tospawn=`tail -n +2 toinstall.txt`
  tobuild=`head -1 toinstall.txt`
elif [ $numlines -gt 4 ]; then
  echo --- Splitting packages
  tospawn=`tail -n +5 toinstall.txt`
  tobuild=`head -4 toinstall.txt`
else
  tobuild=`cat toinstall.txt`
  tospawn=
fi

if [ ! -z "$tospawn" ]; then
  echo --- Spawning jobs for excess packages
  echo steps: > tobuild.yml
  for i in $tospawn; do
    cat <<EOL >> tobuild.yml
- label: ":right-facing_fist:"
  agents:
    docker: "true"
    arch: "$arch"
    os: "linux"
  timeout_in_minutes: 60
  retry:
    automatic: true
  command:
  - mkdir -p $tag/results
  - docker pull $hub:$tag
  - rm -rf output && mkdir -p output
  - docker run --privileged --rm -v \`pwd\`/output:/mnt -v opam2-archive:/home/opam/.opam/download-cache $hub:$tag obi-ci-install $i
  - cp output/$i.txt $tag/results/$i.txt
  - cp output/$i.json $tag/results/$i.json
  - buildkite-agent artifact upload $tag/results/$i.txt
  - buildkite-agent artifact upload $tag/results/$i.json
  - rm -rf output
EOL
  done
  cat tobuild.yml
  buildkite-agent pipeline upload tobuild.yml
fi

echo "--- Building packages"
for i in $tobuild; do
  echo --- Building $i
  mkdir -p $tag/results
  docker pull $hub:$tag
  rm -rf output && mkdir -p output
  docker run --privileged --rm -v `pwd`/output:/mnt -v opam2-archive:/home/opam/.opam/download-cache $hub:$tag obi-ci-install $i
  cp output/$i.txt $tag/results/$i.txt
  cp output/$i.json $tag/results/$i.json
  buildkite-agent artifact upload $tag/results/$i.txt
  buildkite-agent artifact upload $tag/results/$i.json
  rm -rf output
done

echo "--- Recording uninstallable results"
for i in `cat uninstallable.txt`; do
  echo --- Building $i
  mkdir -p $tag/results
  rm -rf output && mkdir -p output
  docker run --privileged --rm -v `pwd`/output:/mnt -v opam2-archive:/home/opam/.opam/download-cache $hub:$tag obi-ci-install $i
  cp output/$i.txt $tag/results/$i.txt
  cp output/$i.json $tag/results/$i.json
  buildkite-agent artifact upload $tag/results/$i.txt
  buildkite-agent artifact upload $tag/results/$i.json
  rm -rf output
done
