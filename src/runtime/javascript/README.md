# JavaScript bindings to the C runtime

A Node.js package for accessing the C runtime.

## Dev notes

- Assumes runtime is already installed on system (does not compile itself)
- ~~`npm install`~~ `npm run build`  will create `./build/Release/native.node` (see `binding.gyp > target_name`
- not sure if `--napi-modules` flag is necessary when running node
- test with: `npm run build && node index.js`

### Resources

https://medium.com/@tarkus/how-to-call-c-c-code-from-node-js-86a773033892
https://medium.com/@atulanand94/beginners-guide-to-writing-nodejs-addons-using-c-and-n-api-node-addon-api-9b3b718a9a7f

---

**What about the pure JS runtime?**

As of June 2019, the JavaScript version of the GF runtime
has been replaced by a TypeScript version at: <https://github.com/GrammaticalFramework/gf-typescript>

This folder previously contained an example web application using the old JavaScript runtime,
which you can access [here](https://github.com/GrammaticalFramework/gf-core/tree/12079550f847a9f98eb0e1eca2fd0ea3d986a94a/src/runtime/javascript).
