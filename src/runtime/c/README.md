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

### Windows

- Install MSYS2
- MSYS2 provides two shells MSYS2 MSYS and MSYS2 MinGW. Open MSYS2 MinGW.
- Use pacman to install additional packages:
```
pacman -Syu
pacman -S --needed base-devel mingw-w64-x86_64-toolchain mingw-w64-x86_64-libtool
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

## Uninstalling

To remove the _old_ C runtime from your system, do:
```
rm /usr/local/lib/libpgf.*
rm /usr/local/lib/libgu.*
rm /usr/local/lib/libsg.*
rm -rf /usr/local/include/pgf
```

To remove _this_ version of the runtime from your system, do:
```
rm /usr/local/lib/libpgf.*
rm -rf /usr/local/include/pgf
```

To clean all generated build files from this directory, use:
```
git clean -Xdf  
```
