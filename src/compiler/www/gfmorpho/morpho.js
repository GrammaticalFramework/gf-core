
var local=appLocalStorage("gf.morpho.")

function quote(s) {
    return s[0]=="_" ? s.substr(1) : '"'+s+'"'
}

function show_output(output) {
    morpho_output.className=output ? "morpho_output" : ""
    replaceChildren(morpho_output,text(output))
}

function submitmorpho() {
    clear(morpho_output)
    var args=morpho.args.value.split(/ +/)
    var lang=args[0]
    var cat=args[1]
    var wordforms=args.slice(2).map(quote).join(" ")
    //console.log("submitmorpho",lang,cat,wordforms)
    switch("") {
    case lang: show_output("No language"); break;
    case cat: show_output("No category"); break;
    case wordforms: show_output("No word forms"); break;
    default:
	gfshell("e",function() {
	    gfshell("i -retain alltenses/Paradigms"+lang+".gfo",function() {
		gfshell("cc -table -unqual mk"+cat+wordforms,show_output)
	    })
	})
    }
    return false;
}

function resetmorpho() {
    show_output("")
}

function submit_example(b) {
    //console.log("submit_example",b.value)
    morpho.args.value=b.value
    submitmorpho()
}
