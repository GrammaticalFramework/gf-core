mkAPI().then((pgf) => {
    // Parse expression
    const expr = pgf.readExpr("Pred (Another (x f))");

    // Show it
    console.log(expr.toString());

    // Print its arity
    console.log('arity', expr.arity());

    pgf.readPGF("Foods.pgf").then((gr) => {
        // Print its name
        console.log(gr.abstractName);
    });
});
