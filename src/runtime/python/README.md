# Python bindings to C runtime

## Compatibility

These bindings do not support Python 2. Python 3 is assumed in all these instructions.

## Pre-requisites

1. You must have installed the PGF C runtime (see `../c/README.md`)
2. You may need to set the environment variable `LD_LIBRARY_PATH=/usr/local/lib`
3. You will need the system Python development package, e.g.:
  - RedHat: `yum install python-devel`
  - Debian: `apt install python-dev`

## Installation

```sh
python setup.py build
sudo python setup.py install
```

## Usage

See: https://www.grammaticalframework.org/doc/runtime-api.html#python

## Running tests

```sh
pytest
```

Requires: `pip install pytest`
