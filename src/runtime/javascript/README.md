# JavaScript (Node.js) bindings to PGF runtime

## Using in your project

1. You must have installed the PGF C runtime (see `../c/README.md`)
2. You may need to set the environment variable `LD_LIBRARY_PATH=/usr/local/lib`
3. Add to your project the usual way with:
```
npm install path/to/this/folder
```
4. Or install globally with:
```
npm install --global .
```

## Building the package (development)

**Pre-requisites**
```
npm install
```

**Compiling**

```
npm run build
```

**Running tests**
```
npm run test
```

## Known issues

There is a known issue with random crashes with Node 14 and 16 on macOS. See here:
- https://github.com/nodejs/node/issues/32463
- https://github.com/node-ffi-napi/node-ffi-napi/issues/97
- https://github.com/node-ffi-napi/ref-napi/issues/47

It seems to work with Node 12 on macOS, as well as on other platforms.
