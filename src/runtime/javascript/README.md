# JavaScript runtime using Web Assembly

This folder contains very early work experimenting with a pure JavaScript runtime,
compiled to Web Assembly (WASM) using [Emscripten](https://emscripten.org/).

1. Compile the WASM files (inside Docker) using `build-wasm.sh`, placing them in `.libs/`
2. Test in Node.js by running `node test-node.js [path to PGF]`
3. Test in a web browser
    1. Start a server with `npx serve -l 41296`
    2. Browse to `http://localhost:41296/test-web.html`
    3. Check JavaScript console
