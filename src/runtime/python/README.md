# Python bindings to C runtime

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


## Debugging

valgrind --leak-check=full --show-leak-kinds=all pytest 2> valgrind.txt

valgrind --leak-check=full --show-leak-kinds=all -- python -c 'import pgf; s = pgf.readType("A -> B"); print(s)' 2> valgrind.txt
