# "Majestic" C Runtime

## Requirements

### Debian/Ubuntu

Required system packages (`apt install ...`):
```
autoconf
automake
libtool
make
g++
```

### macOS

- Install XCode from App Store
- Install XCode command line tools: `xcode-select --install`

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
