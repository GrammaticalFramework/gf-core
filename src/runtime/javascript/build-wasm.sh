#! /usr/bin/env bash
set -e

# Build inside Docker image
IMAGE="gf/build-c-runtime-wasm"
docker build ../c --file Dockerfile --tag $IMAGE

# Copy bulit files from container to host
mkdir -p .libs
docker run --rm --volume "$PWD":/tmp/host $IMAGE bash -c "cp pgf.js pgf.wasm /tmp/host/.libs/"
