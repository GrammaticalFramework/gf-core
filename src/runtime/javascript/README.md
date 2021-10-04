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
