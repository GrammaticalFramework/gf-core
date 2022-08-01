/**
 * This module is the high-level JavaScript wrapper around the WASM-compiled version.
 */

function mkAPI(Module) {

    function checkError(err) {
        if (Module._gu_exn_is_raised(err)) {
            console.error('raised');
    
            const isCaught = Module.ccall(
                'gu_exn_caught_',
                'boolean',
                ['number', 'string'],
                [err, 'GuErrno']
            );
            if (isCaught) {
                console.error('caught');
    
                errDataPtr = Module._gu_exn_caught_data(err);
                errno = Module.getValue(errDataPtr, '*');
                console.error('errno', errno);
            }
    
            Module._gu_exn_clear(err);
            return true;
        }
        return false;
    }

    function readPGF(pgfPath) {
        const pool = Module._gu_new_pool();

        const tmp_pool = Module._gu_new_pool();
        const err = Module._gu_new_exn(tmp_pool);

        // const pgf = Module.ccall(
        //     'pgf_read',
        //     'number',
        //     ['string', 'number', 'number'],
        //     [pgfPath, pool, err]
        // );
        const strPtr = Module.allocateUTF8(pgfPath);
        const pgf = Module._pgf_read(strPtr, pool, err);

        if (checkError(err)) {
            Module._free(tmp_pool);
            throw new Error('Cannot read PGF');
        }
        Module._free(tmp_pool);
        return pgf;
    }

    function abstractName(pgf) {
        const namePtr = Module._pgf_abstract_name(pgf);
        return Module.UTF8ToString(namePtr);
    }

    function readExpr(exprStr) {
        const tmp_pool = Module._gu_new_pool();

        const strPtr = Module.allocateUTF8(exprStr);
        const in_ = Module._gu_data_in(strPtr, exprStr.length, tmp_pool);
        const err = Module._gu_new_exn(tmp_pool);
        const pool = Module._gu_new_pool();
        const expr = Module._pgf_read_expr(in_, pool, tmp_pool, err);
        Module._free(strPtr);
        if (checkError(err)) {
            throw new Error();
        }
        if (expr == 0) {
            throw new Error('Expression cannot be parsed');
        }
        Module._free(tmp_pool);
        return expr;
    }

    function arity(expr) {
        return Module._pgf_expr_arity(expr);
    }

    function showExpr(expr) {
        const tmp_pool = Module._gu_new_pool();

        const sb = Module._gu_new_string_buf(tmp_pool);
        const out = Module._gu_string_buf_out(sb);
        const err = Module._gu_new_exn(tmp_pool);
        Module._pgf_print_expr(expr, 0, 0, out, err);
        if (checkError(err)) {
            Module._free(tmp_pool);
            throw new Error('Cannot print expression');
        }

        const str = Module._gu_string_buf_data(sb);
        return Module.UTF8ToString(str);
    }

    return { readPGF, abstractName, readExpr, arity, showExpr };
}

// This allows us to use both from Node and in browser
if (typeof module != 'undefined') {
    module.exports = mkAPI;
}
