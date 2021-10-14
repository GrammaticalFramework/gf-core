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
- Required system packages (`brew install ...`):
```
autoconf
automake
libtool
```

## Installation

**Note for macOS**: you should first run `glibtoolize`, followed by the commands below.

```
autoreconf -i
./configure
make
make install
```
The shared libraries are installed in `/usr/local/lib`.

## Using

- Compiling GF with this runtime will require flag `--extra-lib-dirs=/usr/local/lib`.
- Running GF with this runtime will require environment variable `LD_LIBRARY_PATH=/usr/local/lib`
