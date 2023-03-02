/**
 * This module is the high-level JavaScript wrapper around the WASM-compiled version.
 */

async function mkAPI() {

    const sizeof_GuMapItor = 4;
    const offsetof_GuMapItor_fn = 0;

    var asm = null;
    var wasmTable = null;
    var freeTableIndexes = [];

    function setErrNo(value) {
        HEAP32[asm.__errno_location() >> 2] = value;
        return value;
    }

    function abortOnCannotGrowMemory(requestedSize) {
        abort('Cannot enlarge memory arrays to size ' + requestedSize + ' bytes (OOM). Either (1) compile with  -s INITIAL_MEMORY=X  with X higher than the current value ' + HEAP8.length + ', (2) compile with  -s ALLOW_MEMORY_GROWTH=1  which allows increasing the size at runtime, or (3) if you want malloc to return NULL (0) instead of this abort, compile with  -s ABORTING_MALLOC=0 ');
    }

    function _emscripten_resize_heap(requestedSize) {
        var oldSize = HEAPU8.length;
        requestedSize = requestedSize >>> 0;
        abortOnCannotGrowMemory(requestedSize);
    }

    var tempRet0 = 0;
    var urlData = {};
    var fdData  = {};
    var fdMax   = 0;
    var asmLibraryArg = {
        "__syscall_fcntl64":
            function (fd, cmd, varargs) {
                setErrNo(134);
                return -1;
            },

        "__syscall_ioctl":
            function (fd, op, varargs) {
                setErrNo(134);
                return -1;
            },

        "__syscall_open":
            function (pathPtr, flags, varargs) {
                const path = UTF8ToString(pathPtr);
                const data = urlData[path];
                if (data == null) {
                    setErrNo(129);
                    return -1;
                }
                fdMax++;
                fdData[fdMax] = {data: data, pos: 0};
                delete urlData[path];
                return fdMax;
            },

        "_munmap_js":
            function (addr, len, prot, flags, fd, offset) {
                setErrNo(134);
                return -1;
            },

        "abort":
            function () {
                console.log('native code called abort()');
            },

        "emscripten_memcpy_big":
            function (dest, src, num) {
                HEAPU8.copyWithin(dest, src, src + num);
            },

        "emscripten_resize_heap":
            function _emscripten_resize_heap(requestedSize) {
                var oldSize = HEAPU8.length;
                requestedSize = requestedSize >>> 0;
                abortOnCannotGrowMemory(requestedSize);
            },

      "fd_close":
            function (fd) {
                delete fdData[fd];
                return 0;
            },

      "fd_read":
            function (fd, iov, iovcnt, pnum) {
                const info = fdData[fd];
                if (info == null) {
                    setErrNo(121);
                    return -1;
                }

                let num = 0;
                for (let i = 0; i < iovcnt; i++) {
                  const ptr = HEAP32[(((iov)+(i*8))>>2)];
                  const len = HEAP32[(((iov)+(i*8 + 4))>>2)];

                  let cnt = 0;
                  while (cnt < len && info.pos < info.data.length) {
                    HEAP8[ptr+cnt] = info.data[info.pos];
                    info.pos++
                    cnt++;
                  }

                  num += cnt;
                  if (cnt < len) break; // nothing more to read
                }

                HEAP32[((pnum)>>2)] = num;
                return 0;
            },

      "fd_seek":
            function (fd, offset_low, offset_high, whence, newOffset) {
                setErrNo(134);
                return -1;
            },

      "fd_write":
            function _fd_write(fd, iov, iovcnt, pnum) {
                setErrNo(134);
                return -1;
            },

      "setTempRet0":
            function (value) {
                tempRet0 = value;
            },

      "__assert_fail":
            function (condition, filename, line, func) {
                abort('Assertion failed: ' + UTF8ToString(condition) + ', at: ' + [filename ? UTF8ToString(filename) : 'unknown filename', line, func ? UTF8ToString(func) : 'unknown function']);
            }
    };

    // Wraps a JS function as a wasm function with a given signature.
    function convertJsFunctionToWasm(func, sig) {

        // If the type reflection proposal is available, use the new
        // "WebAssembly.Function" constructor.
        // Otherwise, construct a minimal wasm module importing the JS function and
        // re-exporting it.
        if (typeof WebAssembly.Function == "function") {
            var typeNames = {
                'i': 'i32',
                'j': 'i64',
                'f': 'f32',
                'd': 'f64'
            };
            var type = {
                parameters: [],
                results: sig[0] == 'v' ? [] : [typeNames[sig[0]]]
            };
            for (var i = 1; i < sig.length; ++i) {
                type.parameters.push(typeNames[sig[i]]);
            }
            return new WebAssembly.Function(type, func);
        }

        // The module is static, with the exception of the type section, which is
        // generated based on the signature passed in.
        var typeSection = [
            0x01, // id: section,
            0x00, // length: 0 (placeholder)
            0x01, // count: 1
            0x60, // form: func
        ];
        var sigRet = sig.slice(0, 1);
        var sigParam = sig.slice(1);
        var typeCodes = {
            'i': 0x7f, // i32
            'j': 0x7e, // i64
            'f': 0x7d, // f32
            'd': 0x7c, // f64
        };

        // Parameters, length + signatures
        typeSection.push(sigParam.length);
        for (var i = 0; i < sigParam.length; ++i) {
            typeSection.push(typeCodes[sigParam[i]]);
        }

        // Return values, length + signatures
        // With no multi-return in MVP, either 0 (void) or 1 (anything else)
        if (sigRet == 'v') {
            typeSection.push(0x00);
        } else {
            typeSection = typeSection.concat([0x01, typeCodes[sigRet]]);
        }

        // Write the overall length of the type section back into the section header
        // (excepting the 2 bytes for the section id and length)
        typeSection[1] = typeSection.length - 2;

        // Rest of the module is static
        var bytes = new Uint8Array([
            0x00, 0x61, 0x73, 0x6d, // magic ("\0asm")
            0x01, 0x00, 0x00, 0x00, // version: 1
        ].concat(typeSection, [
            0x02, 0x07, // import section
            // (import "e" "f" (func 0 (type 0)))
            0x01, 0x01, 0x65, 0x01, 0x66, 0x00, 0x00,
            0x07, 0x05, // export section
            // (export "f" (func 0 (type 0)))
            0x01, 0x01, 0x66, 0x00, 0x00,
        ]));

        // We can compile this wasm module synchronously because it is very small.
        // This accepts an import (at "e.f"), that it reroutes to an export (at "f")
        var module = new WebAssembly.Module(bytes);
        var instance = new WebAssembly.Instance(module, {
                'e': {'f': func}
            });
        var wrappedFunc = instance.exports['f'];
        return wrappedFunc;
    }

    function addFunction(func, sig) {
        func = convertJsFunctionToWasm(func, sig);

        let index;

        // Reuse a free index if there is one, otherwise grow.
        if (freeTableIndexes.length) {
            index = freeTableIndexes.pop();
        } else {
            // Grow the table
            try {
                wasmTable.grow(1);
            } catch (err) {
                if (!(err instanceof RangeError)) {
                    throw err;
                }
                throw 'Unable to grow wasm table. Set ALLOW_TABLE_GROWTH.';
            }
            index = wasmTable.length - 1;
        }

        wasmTable.set(index, func);
        return index;
    }

    function removeFunction(index) {
      freeTableIndexes.push(index);
    }

    const response = await fetch("pgf.wasm", { credentials: 'same-origin' });

    const info = {
        'env': asmLibraryArg,
        'wasi_snapshot_preview1': asmLibraryArg,
    };

    // Suppress closure warning here since the upstream definition for
    // instantiateStreaming only allows Promise<Repsponse> rather than
    // an actual Response.
    // TODO(https://github.com/google/closure-compiler/pull/3913): Remove if/when upstream closure is fixed.
    /** @suppress {checkTypes} */
    const result = await WebAssembly.instantiateStreaming(response, info);

    asm = result["instance"].exports;
    wasmTable = asm['__indirect_function_table'];
    const buf = asm['memory'].buffer;
    const HEAP8 = new Int8Array(buf);
    const HEAP16 = new Int16Array(buf);
    const HEAP32 = new Int32Array(buf);
    const HEAPU8 = new Uint8Array(buf);
    const HEAPU16 = new Uint16Array(buf);
    const HEAPU32 = new Uint32Array(buf);
    const HEAPF32 = new Float32Array(buf);
    const HEAPF64 = new Float64Array(buf);

    // Returns the number of bytes the given Javascript string takes if encoded as a UTF8 byte array, EXCLUDING the null terminator byte.
    function lengthBytesUTF8(str) {
        var len = 0;
        for (var i = 0; i < str.length; ++i) {
            // Gotcha: charCodeAt returns a 16-bit word that is a UTF-16 encoded code unit, not a Unicode code point of the character! So decode UTF16->UTF32->UTF8.
            // See http://unicode.org/faq/utf_bom.html#utf16-3
            var u = str.charCodeAt(i); // possibly a lead surrogate
            if (u >= 0xD800 && u <= 0xDFFF) u = 0x10000 + ((u & 0x3FF) << 10) | (str.charCodeAt(++i) & 0x3FF);
            if (u <= 0x7F) ++len;
            else if (u <= 0x7FF) len += 2;
            else if (u <= 0xFFFF) len += 3;
            else len += 4;
        }
        return len;
    }

    function stringToUTF8Array(str, heap, outIdx, maxBytesToWrite) {
        if (!(maxBytesToWrite > 0)) // Parameter maxBytesToWrite is not optional. Negative values, 0, null, undefined and false each don't write out any bytes.
            return 0;

        var startIdx = outIdx;
        var endIdx = outIdx + maxBytesToWrite - 1; // -1 for string null terminator.
        for (var i = 0; i < str.length; ++i) {
            // Gotcha: charCodeAt returns a 16-bit word that is a UTF-16 encoded code unit, not a Unicode code point of the character! So decode UTF16->UTF32->UTF8.
            // See http://unicode.org/faq/utf_bom.html#utf16-3
            // For UTF8 byte structure, see http://en.wikipedia.org/wiki/UTF-8#Description and https://www.ietf.org/rfc/rfc2279.txt and https://tools.ietf.org/html/rfc3629
            var u = str.charCodeAt(i); // possibly a lead surrogate
            if (u >= 0xD800 && u <= 0xDFFF) {
              var u1 = str.charCodeAt(++i);
              u = 0x10000 + ((u & 0x3FF) << 10) | (u1 & 0x3FF);
            }
            if (u <= 0x7F) {
              if (outIdx >= endIdx) break;
              heap[outIdx++] = u;
            } else if (u <= 0x7FF) {
              if (outIdx + 1 >= endIdx) break;
              heap[outIdx++] = 0xC0 | (u >> 6);
              heap[outIdx++] = 0x80 | (u & 63);
            } else if (u <= 0xFFFF) {
              if (outIdx + 2 >= endIdx) break;
              heap[outIdx++] = 0xE0 | (u >> 12);
              heap[outIdx++] = 0x80 | ((u >> 6) & 63);
              heap[outIdx++] = 0x80 | (u & 63);
            } else {
              if (outIdx + 3 >= endIdx) break;
              if (u > 0x10FFFF) warnOnce('Invalid Unicode code point 0x' + u.toString(16) + ' encountered when serializing a JS string to a UTF-8 string in wasm memory! (Valid unicode code points should be in range 0-0x10FFFF).');
              heap[outIdx++] = 0xF0 | (u >> 18);
              heap[outIdx++] = 0x80 | ((u >> 12) & 63);
              heap[outIdx++] = 0x80 | ((u >> 6) & 63);
              heap[outIdx++] = 0x80 | (u & 63);
            }
        }

        // Null-terminate the pointer to the buffer.
        heap[outIdx] = 0;
        return outIdx - startIdx;
    }

    function allocateUTF8(pool,str) {
        var size = lengthBytesUTF8(str) + 1;
        var ptr = asm.gu_malloc(pool,size);
        if (ptr) stringToUTF8Array(str, HEAP8, ptr, size);
        return ptr;
    }

    const UTF8Decoder = typeof TextDecoder != 'undefined' ? new TextDecoder('utf8') : undefined;

    /**
     * @param {number} idx
     * @param {number=} maxBytesToRead
     * @return {string}
     */
    function UTF8ArrayToString(heap, idx, maxBytesToRead) {
        var endIdx = idx + maxBytesToRead;
        var endPtr = idx;
        // TextDecoder needs to know the byte length in advance, it doesn't stop on null terminator by itself.
        // Also, use the length info to avoid running tiny strings through TextDecoder, since .subarray() allocates garbage.
        // (As a tiny code save trick, compare endPtr against endIdx using a negation, so that undefined means Infinity)
        while (heap[endPtr] && !(endPtr >= endIdx)) ++endPtr;

        if (endPtr - idx > 16 && heap.subarray && UTF8Decoder) {
            return UTF8Decoder.decode(heap.subarray(idx, endPtr));
        } else {
            var str = '';
            // If building with TextDecoder, we have already computed the string length above, so test loop end condition against that
            while (idx < endPtr) {
                // For UTF8 byte structure, see:
                // http://en.wikipedia.org/wiki/UTF-8#Description
                // https://www.ietf.org/rfc/rfc2279.txt
                // https://tools.ietf.org/html/rfc3629
                var u0 = heap[idx++];
                if (!(u0 & 0x80)) { str += String.fromCharCode(u0); continue; }
                var u1 = heap[idx++] & 63;
                if ((u0 & 0xE0) == 0xC0) { str += String.fromCharCode(((u0 & 31) << 6) | u1); continue; }
                var u2 = heap[idx++] & 63;
                if ((u0 & 0xF0) == 0xE0) {
                    u0 = ((u0 & 15) << 12) | (u1 << 6) | u2;
                } else {
                    if ((u0 & 0xF8) != 0xF0) warnOnce('Invalid UTF-8 leading byte 0x' + u0.toString(16) + ' encountered when deserializing a UTF-8 string in wasm memory to a JS string!');
                    u0 = ((u0 & 7) << 18) | (u1 << 12) | (u2 << 6) | (heap[idx++] & 63);
                }

                if (u0 < 0x10000) {
                    str += String.fromCharCode(u0);
                } else {
                    var ch = u0 - 0x10000;
                    str += String.fromCharCode(0xD800 | (ch >> 10), 0xDC00 | (ch & 0x3FF));
                }
            }
        }
        return str;
    }

    function UTF8ToString(ptr, maxBytesToRead) {
      return ptr ? UTF8ArrayToString(HEAPU8, ptr, maxBytesToRead) : '';
    }

    const GuErrnoStrPtr = asm.malloc(8);
    stringToUTF8Array("GuErrno", HEAP8, GuErrnoStrPtr, 8);

    const PgfExnStrPtr = asm.malloc(8);
    stringToUTF8Array("PgfExn", HEAP8, PgfExnStrPtr, 8);

    function pgfError(err) {
        if (asm.gu_exn_caught_(err, GuErrnoStrPtr)) {
            errDataPtr = asm.gu_exn_caught_data(err);
            return new Error("errno="+HEAP32[errDataPtr >> 2]);
        } else if (asm.gu_exn_caught_(err, PgfExnStrPtr)) {
            msgPtr = asm.gu_exn_caught_data(err);
            return new Error(UTF8ToString(msgPtr));
        }
        return new Error();
    }

    const registry = new FinalizationRegistry((pool) => {
        asm.gu_pool_free(pool);
    });

    function PGF(pgfPtr,name,pool) {
        this.pgfPtr = pgfPtr;
        this.abstractName = name;
        this.pool   = pool;
        this.languages = {};
        registry.register(this,pool);
    }

    function Concr(pgf,concrPtr,name) {
        this.pgf  = pgf;
        this.name = name;
        this.concrPtr = concrPtr;
    }
    Concr.prototype.linearize = function(expr) {
        const tmp_pool = asm.gu_new_pool();
        const err = asm.gu_new_exn(tmp_pool);
        const sb = asm.gu_new_string_buf(tmp_pool);
        const out = asm.gu_string_buf_out(sb);

        asm.pgf_linearize(this.concrPtr, expr.exprPtr, out, err);
        if (asm.gu_exn_is_raised(err)) {
            const e = pgfError(err);
            asm.gu_pool_free(tmp_pool);
            throw e;
        }

        const strPtr = asm.gu_string_buf_data(sb);
        const len = asm.gu_string_buf_length(sb);
        const str = UTF8ToString(strPtr,len);
        asm.gu_pool_free(tmp_pool);

        return str
    }

    async function readPGF(pgfURL) {
        const response = await fetch(pgfURL);
        urlData[pgfURL] = new Int8Array(await response.arrayBuffer());

        const pool = asm.gu_new_pool();

        const tmp_pool = asm.gu_new_pool();
        const err = asm.gu_new_exn(tmp_pool);
        const strPtr = allocateUTF8(tmp_pool,pgfURL);
        const pgfPtr = asm.pgf_read(strPtr,pool,err);
        if (asm.gu_exn_is_raised(err)) {
            const e = pgfError(err);
            asm.gu_pool_free(tmp_pool);
            throw e;
        }

        const namePtr = asm.pgf_abstract_name(pgfPtr);
        const abstractName = UTF8ToString(namePtr);

        const pgf = new PGF(pgfPtr,abstractName,pool);

        const itor = asm.gu_malloc(tmp_pool,sizeof_GuMapItor);
        const fn =
          addFunction(
             (itor,namePtr,concrPtrPtr,err) => {
                 const name = UTF8ToString(namePtr);
                 const concrPtr = HEAP32[concrPtrPtr >> 2];
                 pgf.languages[name] = new Concr(pgf,concrPtr,name);
             },
             "viiii"
             );
        HEAP32[(itor+offsetof_GuMapItor_fn) >> 2] = fn;
        asm.pgf_iter_languages(pgfPtr,itor,err);
        removeFunction(fn);

        asm.gu_pool_free(tmp_pool);
        return pgf;
    }

    function Expr(exprPtr,pool) {
        this.exprPtr = exprPtr;
        this.pool = pool;
        registry.register(this,pool);
    }
    Expr.prototype.toString = function() {
        const tmp_pool = asm.gu_new_pool();

        const sb = asm.gu_new_string_buf(tmp_pool);
        const out = asm.gu_string_buf_out(sb);
        const err = asm.gu_new_exn(tmp_pool);
        asm.pgf_print_expr(this.exprPtr, 0, 0, out, err);
        if (asm.gu_exn_is_raised(err)) {
            const e = pgfError(err);
            asm.gu_pool_free(tmp_pool);
            throw e;
        }

        const strPtr = asm.gu_string_buf_data(sb);
        const len = asm.gu_string_buf_length(sb);
        const str = UTF8ToString(strPtr,len);
        asm.gu_pool_free(tmp_pool);

        return str;
    };
    Expr.prototype.arity = function(expr) {
        return asm.pgf_expr_arity(this.expr);
    }

    function readExpr(exprStr) {
        const tmp_pool = asm.gu_new_pool();

        const strPtr = allocateUTF8(tmp_pool,exprStr);
        const in_ = asm.gu_data_in(strPtr, exprStr.length, tmp_pool);
        const err = asm.gu_new_exn(tmp_pool);
        const pool = asm.gu_new_pool();
        const expr = asm.pgf_read_expr(in_, pool, tmp_pool, err);
        asm.gu_pool_free(tmp_pool);

        if (asm.gu_exn_is_raised(err)) {
            throw pgfError(err);
        }
        if (expr == 0) {
            throw new Error('Expression cannot be parsed');
        }

        return new Expr(expr,pool);
    }

    return { readPGF, readExpr };
}

// This allows us to use both from Node and in browser
if (typeof module != 'undefined') {
    module.exports = mkAPI;
}