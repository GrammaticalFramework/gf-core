# GF TypeScript Runtime

`gflib.ts` is a TypeScript implementation of the GF runtime.
It is ported from an older JavaScript implementation [`gflib.js`](../javascript/gflib.js),
with some small improvements.

Importantly, all **future** updates will only be made to this TypeScript version.

## Applicability

This runtime allows you use GF in pure JavaScript, and thus have GF-powered apps without the need for a server backend.
However, it has not been actively maintained as the other runtimes have been.
So its features are limited and it is not efficient, making it really only useful for smaller grammars.

## Using

`gflib.ts` can be transpiled to JavaScript by running `tsc` in this folder (of course you need TypeScript installed).
It has no module depenedencies.
You can then include the generated `gflib.js` file in your application as usual.

Your GF grammar should be compiled with: `gf --make --output-format=js`

## What happened to `gflib.d.ts`?

There was once a file here called `gflib.d.ts`, which contained TypeScript type definitions for the **old** `gflib.js`.
Since the runtime is now ported to TypeScript, those type definitions are no longer necessary (plus they contained many errors).
