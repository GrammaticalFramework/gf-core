mkAPI().then((pgf) => {
    // Parse expression
    console.log(pgf.readExpr("Pred (This Fish) Fresh"));
/*
    // Show it
    console.log(expr.toString());

    // Print its arity
    console.log('arity', expr.arity());

    pgf.readPGF("Foods.pgf").then((gr) => {
        // Print the grammar name
        console.log(gr.abstractName);
        
        // Access a language and print the concrete name
        console.log(gr.languages["FoodsEng"].name);
        
        // Linearize an expression
        console.log(gr.languages["FoodsEng"].linearize(expr));
    });*/
});
