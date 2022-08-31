Module.onRuntimeInitialized = () => {
    const JSPGF = mkAPI(Module);

    fetch('Foods.pgf')
      .then((response) => response.arrayBuffer())
      .then((data) => {
        const pgfPathFS = '/tmp/Foods.pgf';
        Module.FS.writeFile(pgfPathFS, new Uint8Array(data));

        // Read PGF
        const pgf = JSPGF.readPGF(pgfPathFS);

        // Print its name
        console.log(JSPGF.abstractName(pgf));
      })
    
    // Parse expression
    const expr = JSPGF.readExpr("Pred (Another (x f))");

    // Show it
    console.log(JSPGF.showExpr(expr));

    // Print its arity
    console.log('arity', JSPGF.arity(expr));
}

