mkAPI().then((pgf) => {
    // Parse expression
    const expr = pgf.readExpr("Pred (This Fish) Fresh");

    // Show it
    console.log(expr.toString());

    // Print its arity
    console.log('arity', expr.arity());

    pgf.readPGF("Foods.pgf").then((gr) => {
        // Print its name
        console.log(gr.abstractName);
        console.log(gr.languages["FoodsEng"].name);
        console.log(gr.languages["FoodsEng"].linearize(expr));
    });
});
