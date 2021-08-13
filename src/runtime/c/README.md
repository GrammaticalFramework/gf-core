# "Majestic" C Runtime

## Requirements

Required system packages (Debian/Ubuntu):
```
autoconf
automake
libtool
make
g++
```

## Installation

Installing the runtime (puts libraries in `/usr/local/lib`):
```
autoreconf -i
./configure
make
make install
```

## Using

- Compiling GF with this runtime will require flag `--extra-lib-dirs=/usr/local/lib`.
- Running GF with this runtime will require environment variable `LD_LIBRARY_PATH=/usr/local/lib`
