const Module = require('./.libs/pgf.js');
const JSPGF = require('./jspgf.js')(Module);
const fs = require('fs');
const path = require('path');

Module.onRuntimeInitialized = () => {

    // Read PGF path from args
    if (process.argv.length > 2) {
        const pgfPathHost = process.argv[2];

        // Copy file into filesystem
        const pgfPathFS = '/tmp/' + path.basename(pgfPathHost);
        const rawPgf = fs.readFileSync(pgfPathHost);
        Module.FS.writeFile(pgfPathFS, rawPgf);

        // Read PGF
        const pgf = JSPGF.readPGF(pgfPathFS);

        // Print its name
        console.log(JSPGF.abstractName(pgf));
    }

    // Parse expression
    const expr = JSPGF.readExpr("Pred (Another (x f))");

    // Show it
    console.log(JSPGF.showExpr(expr));

    // Print its arity
    console.log('arity', JSPGF.arity(expr));
}

