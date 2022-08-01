#! /usr/bin/env bash
set -e
cd ../c

# Build inside Docker image
IMAGE="gf/build-c-runtime-wasm"
docker build . --file Dockerfile-wasm --tag $IMAGE

# Copy bulit files from container to host
docker run --rm --volume "$PWD":/tmp/host $IMAGE bash -c "cp pgf.js pgf.wasm /tmp/host/"
