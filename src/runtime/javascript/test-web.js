Module.onRuntimeInitialized = () => {
    const JSPGF = mkAPI(Module);

    // Parse expression
    const expr = JSPGF.readExpr("Pred (Another (x f))");

    // Show it
    console.log(JSPGF.showExpr(expr));

    // Print its arity
    console.log('arity', JSPGF.arity(expr));
}

