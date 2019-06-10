"use strict";
var GFGrammar = (function () {
    function GFGrammar(abstract, concretes) {
        this.abstract = abstract;
        this.concretes = concretes;
    }
    GFGrammar.prototype.translate = function (input, fromLang, toLang) {
        var outputs = {};
        var fromConcs = this.concretes;
        if (fromLang) {
            fromConcs = {};
            fromConcs[fromLang] = this.concretes[fromLang];
        }
        var toConcs = this.concretes;
        if (toLang) {
            toConcs = {};
            toConcs[toLang] = this.concretes[toLang];
        }
        for (var c1 in fromConcs) {
            var concrete = this.concretes[c1];
            var trees = concrete.parseString(input, this.abstract.startcat);
            if (trees.length > 0) {
                outputs[c1] = [];
                for (var i in trees) {
                    outputs[c1][i] = {};
                    for (var c2 in toConcs) {
                        outputs[c1][i][c2] = this.concretes[c2].linearize(trees[i]);
                    }
                }
            }
        }
        return outputs;
    };
    return GFGrammar;
}());
var Fun = (function () {
    function Fun(name) {
        var args = [];
        for (var _i = 1; _i < arguments.length; _i++) {
            args[_i - 1] = arguments[_i];
        }
        this.name = name;
        this.args = [];
        for (var i = 1; i < args.length; i++) {
            this.args[i - 1] = args[i];
        }
    }
    Fun.prototype.print = function () {
        return this.show(0);
    };
    Fun.prototype.show = function (prec) {
        if (this.isMeta()) {
            if (isUndefined(this.type)) {
                return '?';
            }
            else {
                var s = '?:' + this.type;
                if (prec > 0) {
                    s = '(' + s + ')';
                }
                return s;
            }
        }
        else {
            var s = this.name;
            var cs = this.args;
            for (var i in cs) {
                s += ' ' + (isUndefined(cs[i]) ? 'undefined' : cs[i].show(1));
            }
            if (prec > 0 && cs.length > 0) {
                s = '(' + s + ')';
            }
            return s;
        }
    };
    Fun.prototype.getArg = function (i) {
        return this.args[i];
    };
    Fun.prototype.setArg = function (i, c) {
        this.args[i] = c;
    };
    Fun.prototype.isMeta = function () {
        return this.name == '?';
    };
    Fun.prototype.isComplete = function () {
        if (this.isMeta()) {
            return false;
        }
        else {
            for (var i in this.args) {
                if (!this.args[i].isComplete()) {
                    return false;
                }
            }
            return true;
        }
    };
    Fun.prototype.isLiteral = function () {
        return (/^[\"\-\d]/).test(this.name);
    };
    Fun.prototype.isString = function () {
        return (/^\".*\"$/).test(this.name);
    };
    Fun.prototype.isInt = function () {
        return (/^\-?\d+$/).test(this.name);
    };
    Fun.prototype.isFloat = function () {
        return (/^\-?\d*(\.\d*)?$/).test(this.name) && this.name != '.' && this.name != '-.';
    };
    Fun.prototype.isEqual = function (obj) {
        if (this.name != obj.name)
            return false;
        for (var i in this.args) {
            if (!this.args[i].isEqual(obj.args[i]))
                return false;
        }
        return true;
    };
    return Fun;
}());
var GFAbstract = (function () {
    function GFAbstract(startcat, types) {
        this.startcat = startcat;
        this.types = types;
    }
    GFAbstract.prototype.addType = function (fun, args, cat) {
        this.types[fun] = new Type(args, cat);
    };
    GFAbstract.prototype.getArgs = function (fun) {
        return this.types[fun].args;
    };
    GFAbstract.prototype.getCat = function (fun) {
        return this.types[fun].cat;
    };
    GFAbstract.prototype.annotate = function (tree, type) {
        if (tree.name == '?') {
            tree.type = type;
        }
        else {
            var typ = this.types[tree.name];
            for (var i in tree.args) {
                this.annotate(tree.args[i], typ.args[i]);
            }
        }
        return tree;
    };
    GFAbstract.prototype.handleLiterals = function (tree, type) {
        if (tree.name != '?') {
            if (type == 'String' || type == 'Int' || type == 'Float') {
                tree.name = type + '_Literal_' + tree.name;
            }
            else {
                var typ = this.types[tree.name];
                for (var i in tree.args) {
                    this.handleLiterals(tree.args[i], typ.args[i]);
                }
            }
        }
        return tree;
    };
    GFAbstract.prototype.copyTree = function (x) {
        var t = new Fun(x.name);
        if (!isUndefined(x.type)) {
            t.type = x.type;
        }
        var cs = x.args;
        if (!isUndefined(cs)) {
            for (var i = 0; i < cs.length; i++) {
                t.setArg(i, this.copyTree(cs[i]));
            }
        }
        return t;
    };
    GFAbstract.prototype.parseTree = function (str, type) {
        var pt = this.parseTree_(str.match(/[\w\'\.\"]+|\(|\)|\?|\:/g) || [], 0);
        return pt ? this.annotate(pt, type) : null;
    };
    GFAbstract.prototype.parseTree_ = function (tokens, prec) {
        if (tokens.length == 0 || tokens[0] == ')') {
            return null;
        }
        var t = tokens.shift();
        if (!t)
            return null;
        if (t == '(') {
            var tree = this.parseTree_(tokens, 0);
            tokens.shift();
            return tree;
        }
        else if (t == '?') {
            return new Fun('?');
        }
        else {
            var tree = new Fun(t);
            if (prec == 0) {
                var c = void 0;
                var i = void 0;
                for (i = 0; (c = this.parseTree_(tokens, 1)) !== null; i++) {
                    tree.setArg(i, c);
                }
            }
            return tree;
        }
    };
    return GFAbstract;
}());
var Type = (function () {
    function Type(args, cat) {
        this.args = args;
        this.cat = cat;
    }
    return Type;
}());
var GFConcrete = (function () {
    function GFConcrete(flags, productions, functions, sequences, startCats, totalFIds) {
        this.flags = flags;
        this.functions = functions;
        this.startCats = startCats;
        this.totalFIds = totalFIds;
        this.pproductions = productions;
        this.lproductions = {};
        var _loop_1 = function (fid0) {
            var fid = parseInt(fid0);
            var _loop_2 = function (i) {
                var rule = productions[fid][i];
                if (rule.id == 'Apply') {
                    rule = rule;
                    var fun_1 = this_1.functions[rule.fun];
                    var lproductions_1 = this_1.lproductions;
                    rule.fun = fun_1;
                    var register_1 = function (args, key, i) {
                        if (i < args.length) {
                            var c = 0;
                            var arg = args[i].fid;
                            for (var k in productions[arg]) {
                                var rule_1 = productions[arg][k];
                                if (rule_1.id == 'Coerce') {
                                    rule_1 = rule_1;
                                    register_1(args, key + '_' + rule_1.arg, i + 1);
                                    c++;
                                }
                            }
                            if (c == 0) {
                                register_1(args, key + '_' + arg, i + 1);
                            }
                        }
                        else {
                            var set = lproductions_1[key];
                            if (set == null) {
                                set = [];
                                lproductions_1[key] = set;
                            }
                            set.push({ fun: fun_1, fid: fid });
                        }
                    };
                    register_1(rule.args, rule.fun.name, 0);
                }
            };
            for (var i in productions[fid]) {
                _loop_2(i);
            }
        };
        var this_1 = this;
        for (var fid0 in productions) {
            _loop_1(fid0);
        }
        for (var _i = 0, functions_1 = functions; _i < functions_1.length; _i++) {
            var fun = functions_1[_i];
            for (var j in fun.lins) {
                fun.lins[j] = sequences[fun.lins[j]];
            }
        }
    }
    GFConcrete.prototype.linearizeSyms = function (tree, tag) {
        var res = [];
        if (tree.isString()) {
            var sym = new SymKS(tree.name);
            sym.tag = tag;
            res.push({ fid: -1, table: [[sym]] });
        }
        else if (tree.isInt()) {
            var sym = new SymKS(tree.name);
            sym.tag = tag;
            res.push({ fid: -2, table: [[sym]] });
        }
        else if (tree.isFloat()) {
            var sym = new SymKS(tree.name);
            sym.tag = tag;
            res.push({ fid: -3, table: [[sym]] });
        }
        else if (tree.isMeta()) {
            var cat = this.startCats[tree.type];
            var sym = new SymKS(tree.name);
            sym.tag = tag;
            for (var fid = cat.s; fid <= cat.e; fid++) {
                res.push({ fid: fid, table: [[sym]] });
            }
        }
        else {
            var cs_1 = [];
            for (var i in tree.args) {
                cs_1.push(this.linearizeSyms(tree.args[i], tag + '-' + i)[0]);
            }
            var key = tree.name;
            for (var i in cs_1) {
                key = key + '_' + cs_1[i].fid;
            }
            for (var i in this.lproductions[key]) {
                var rule = this.lproductions[key][i];
                var row = {
                    fid: rule.fid,
                    table: []
                };
                var _loop_3 = function (j) {
                    var lin = rule.fun.lins[j];
                    var toks = [];
                    row.table[j] = toks;
                    lin.forEach(function (sym0) {
                        switch (sym0.id) {
                            case 'Arg':
                            case 'Lit': {
                                var sym = sym0;
                                var ts = cs_1[sym.i].table[sym.label];
                                for (var l in ts) {
                                    toks.push(ts[l]);
                                }
                                break;
                            }
                            case 'KS':
                            case 'KP': {
                                var sym = sym0;
                                toks.push(sym.tagWith(tag));
                                break;
                            }
                        }
                    });
                };
                for (var j in rule.fun.lins) {
                    _loop_3(j);
                }
                res.push(row);
            }
        }
        return res;
    };
    GFConcrete.prototype.syms2toks = function (syms) {
        var ts = [];
        var _loop_4 = function (i) {
            var sym0 = syms[i];
            switch (sym0.id) {
                case 'KS': {
                    var sym = sym0;
                    for (var j in sym.tokens) {
                        ts.push(sym.tokens[j].tagWith(sym.tag));
                    }
                    break;
                }
                case 'KP': {
                    var sym_1 = sym0;
                    var addedAlt_1 = false;
                    if (i < syms.length - 1) {
                        var nextSym = syms[i + 1];
                        if (nextSym.id == 'KS') {
                            var nextToken_1 = nextSym.tokens[0];
                            sym_1.alts.forEach(function (alt) {
                                if (alt.prefixes.some(function (p) { return nextToken_1.startsWith(p); })) {
                                    alt.tokens.forEach(function (symks) {
                                        symks.tokens.forEach(function (t) {
                                            ts.push(t.tagWith(sym_1.tag));
                                        });
                                    });
                                    addedAlt_1 = true;
                                    return;
                                }
                            });
                        }
                    }
                    if (addedAlt_1)
                        break;
                    sym_1.tokens.forEach(function (symks) {
                        symks.tokens.forEach(function (t) {
                            ts.push(t.tagWith(sym_1.tag));
                        });
                    });
                    break;
                }
            }
        };
        for (var i = 0; i < syms.length; i++) {
            _loop_4(i);
        }
        return ts;
    };
    GFConcrete.prototype.linearizeAll = function (tree) {
        var _this = this;
        return this.linearizeSyms(tree, '0').map(function (r) {
            return _this.unlex(_this.syms2toks(r.table[0]));
        });
    };
    GFConcrete.prototype.linearize = function (tree) {
        var res = this.linearizeSyms(tree, '0');
        return this.unlex(this.syms2toks(res[0].table[0]));
    };
    GFConcrete.prototype.tagAndLinearize = function (tree) {
        var res = this.linearizeSyms(tree, '0');
        return this.syms2toks(res[0].table[0]);
    };
    GFConcrete.prototype.unlex = function (ts) {
        if (ts.length == 0) {
            return '';
        }
        var noSpaceAfter = /^[\(\-\[]/;
        var noSpaceBefore = /^[\.\,\?\!\)\:\;\-\]]/;
        var s = '';
        for (var i = 0; i < ts.length; i++) {
            var t = ts[i];
            var after = i < ts.length - 1 ? ts[i + 1] : null;
            s += t;
            if (after != null
                && !t.match(noSpaceAfter)
                && !after.match(noSpaceBefore)) {
                s += ' ';
            }
        }
        return s;
    };
    GFConcrete.prototype.tokenize = function (string) {
        var inToken = false;
        var start = 0;
        var end;
        var tokens = [];
        var i;
        for (i = 0; i < string.length; i++) {
            if (string.charAt(i) == ' '
                || string.charAt(i) == '\f'
                || string.charAt(i) == '\n'
                || string.charAt(i) == '\r'
                || string.charAt(i) == '\t'
                || string.charAt(i) == '\v'
                || string.charAt(i) == String.fromCharCode(160)) {
                if (inToken) {
                    end = i - 1;
                    inToken = false;
                    tokens.push(string.substr(start, end - start + 1));
                }
            }
            else {
                if (!inToken) {
                    start = i;
                    inToken = true;
                }
            }
        }
        if (inToken) {
            end = i - 1;
            inToken = false;
            tokens.push(string.substr(start, end - start + 1));
        }
        return tokens;
    };
    GFConcrete.prototype.parseString = function (string, cat) {
        var tokens = this.tokenize(string);
        var ps = new ParseState(this, cat);
        for (var i in tokens) {
            if (!ps.next(tokens[i]))
                return [];
        }
        return ps.extractTrees();
    };
    GFConcrete.prototype.complete = function (input, cat) {
        if (input == null)
            input = '';
        var tokens = input.trim().split(' ');
        for (var i = tokens.length - 1; i >= 0; i--) {
            if (tokens[i] == '') {
                tokens.splice(i, 1);
            }
        }
        var current = tokens.pop();
        if (current == null)
            current = '';
        var ps = new ParseState(this, cat);
        var ps2 = new ParseState(this, cat);
        for (var i = 0; i < tokens.length; i++) {
            if (!ps.next(tokens[i])) {
                return { 'consumed': [], 'suggestions': [] };
            }
            ps2.next(tokens[i]);
        }
        if (ps2.next(current)) {
            ps.next(current);
            tokens.push(current);
            current = '';
        }
        var acc = ps.complete(current);
        var suggs = [];
        if (acc.value) {
            acc.value.forEach(function (a) {
                a.seq.forEach(function (s) {
                    switch (s.id) {
                        case 'KS': {
                            s.tokens.forEach(function (t) {
                                suggs.push(t);
                            });
                            break;
                        }
                        case 'KP': {
                            s.tokens.forEach(function (symks) {
                                symks.tokens.forEach(function (t) {
                                    suggs.push(t);
                                });
                            });
                            break;
                        }
                    }
                });
            });
        }
        return { 'consumed': tokens, 'suggestions': suggs };
    };
    return GFConcrete;
}());
String.prototype.tagWith = function (tag) {
    var s2 = this;
    s2.tag = tag;
    return s2;
};
var Apply = (function () {
    function Apply(fun, args) {
        this.id = 'Apply';
        this.fun = fun;
        this.args = args;
    }
    Apply.prototype.show = function (cat) {
        var recStr = [];
        recStr.push(cat, ' -> ', this.fun.name, ' [', this.args, ']');
        return recStr.join('');
    };
    Apply.prototype.isEqual = function (obj) {
        if (this.id != obj.id || this.fun != obj.fun || this.args.length != obj.args.length)
            return false;
        for (var i in this.args) {
            if (this.args[i] != obj.args[i])
                return false;
        }
        return true;
    };
    return Apply;
}());
var Coerce = (function () {
    function Coerce(arg) {
        this.id = 'Coerce';
        this.arg = arg;
    }
    Coerce.prototype.show = function (cat) {
        var recStr = [];
        recStr.push(cat, ' -> _ [', this.arg, ']');
        return recStr.join('');
    };
    return Coerce;
}());
var PArg = (function () {
    function PArg() {
        var hypos = [];
        for (var _i = 0; _i < arguments.length; _i++) {
            hypos[_i] = arguments[_i];
        }
        this.fid = hypos[hypos.length - 1];
        if (hypos.length > 1)
            this.hypos = hypos.slice(0, hypos.length - 1);
        else
            this.hypos = [];
    }
    return PArg;
}());
var Const = (function () {
    function Const(lit, toks) {
        this.id = 'Const';
        this.lit = lit;
        this.toks = toks;
    }
    Const.prototype.show = function (cat) {
        var recStr = [];
        recStr.push(cat, ' -> ', this.lit.print());
        return recStr.join('');
    };
    Const.prototype.isEqual = function (obj) {
        if (this.id != obj.id || this.lit.isEqual(obj.lit) || this.toks.length != obj.toks.length)
            return false;
        for (var i in this.toks) {
            if (this.toks[i] != obj.toks[i])
                return false;
        }
        return true;
    };
    return Const;
}());
var CncFun = (function () {
    function CncFun(name, lins) {
        this.name = name;
        this.lins = lins;
    }
    return CncFun;
}());
var SymCat = (function () {
    function SymCat(i, label) {
        this.id = 'Arg';
        this.i = i;
        this.label = label;
    }
    SymCat.prototype.show = function () {
        var argStr = [];
        argStr.push(this.i, this.label);
        return argStr.join('.');
    };
    return SymCat;
}());
var SymKS = (function () {
    function SymKS() {
        var tokens = [];
        for (var _i = 0; _i < arguments.length; _i++) {
            tokens[_i] = arguments[_i];
        }
        this.id = 'KS';
        this.tokens = tokens;
    }
    SymKS.prototype.show = function () {
        var terminalStr = [];
        terminalStr.push('"', this.tokens, '"');
        return terminalStr.join('');
    };
    SymKS.prototype.tagWith = function (tag) {
        var s = new SymKS();
        s.tokens = this.tokens.slice();
        s.tag = tag;
        return s;
    };
    return SymKS;
}());
var SymKP = (function () {
    function SymKP(tokens, alts) {
        this.id = 'KP';
        this.tokens = tokens;
        this.alts = alts;
    }
    SymKP.prototype.show = function () {
        var terminalStr = [];
        terminalStr.push('"', this.tokens, '"');
        return terminalStr.join('');
    };
    SymKP.prototype.tagWith = function (tag) {
        var s = new SymKP(this.tokens.slice(), this.alts.slice());
        s.tag = tag;
        return s;
    };
    return SymKP;
}());
var Alt = (function () {
    function Alt(tokens, prefixes) {
        this.tokens = tokens;
        this.prefixes = prefixes;
    }
    return Alt;
}());
var SymLit = (function () {
    function SymLit(i, label) {
        this.id = 'Lit';
        this.i = i;
        this.label = label;
    }
    SymLit.prototype.getId = function () {
        return this.id;
    };
    SymLit.prototype.show = function () {
        var argStr = [];
        argStr.push(this.i, this.label);
        return argStr.join('.');
    };
    return SymLit;
}());
var Trie = (function () {
    function Trie() {
        this.value = null;
        this.items = {};
    }
    Trie.prototype.insertChain = function (keys, obj) {
        var node = this;
        keys.forEach(function (key) {
            var nnode = node.items[key];
            if (nnode == null) {
                nnode = new Trie();
                node.items[key] = nnode;
            }
            node = nnode;
        });
        node.value = obj;
    };
    Trie.prototype.insertChain1 = function (keys, obj) {
        var node = this;
        keys.forEach(function (key) {
            var nnode = node.items[key];
            if (nnode == null) {
                nnode = new Trie();
                node.items[key] = nnode;
            }
            node = nnode;
        });
        if (node.value == null)
            node.value = [obj];
        else
            node.value.push(obj);
    };
    Trie.prototype.lookup = function (key) {
        return this.items[key];
    };
    Trie.prototype.isEmpty = function () {
        if (this.value != null)
            return false;
        for (var _ in this.items) {
            return false;
        }
        return true;
    };
    return Trie;
}());
var ParseState = (function () {
    function ParseState(concrete, startCat) {
        this.concrete = concrete;
        this.startCat = startCat;
        this.items = new Trie();
        this.chart = new Chart(concrete);
        var items = [];
        var fids = concrete.startCats[startCat];
        if (fids != null) {
            var fid = void 0;
            for (fid = fids.s; fid <= fids.e; fid++) {
                var exProds = this.chart.expandForest(fid);
                for (var j in exProds) {
                    var rule = exProds[j];
                    var fun = rule.fun;
                    for (var lbl in fun.lins) {
                        items.push(new ActiveItem(0, 0, rule.fun, fun.lins[lbl], rule.args, fid, parseInt(lbl)));
                    }
                }
            }
        }
        this.items.insertChain([], items);
    }
    ParseState.prototype.next = function (token) {
        var acc = this.items.lookup(token);
        if (acc == null) {
            acc = new Trie();
        }
        this.process(this.items.value, function (fid) {
            switch (fid) {
                case -1:
                    return new Const(new Fun('"' + token + '"'), [token]);
                case -2: {
                    var x = parseInt(token, 10);
                    if (token == '0' || (x != 0 && !isNaN(x)))
                        return new Const(new Fun(token), [token]);
                    else
                        return null;
                }
                case -3: {
                    var x = parseFloat(token);
                    if (token == '0' || token == '0.0' || (x != 0 && !isNaN(x)))
                        return new Const(new Fun(token), [token]);
                    else
                        return null;
                }
            }
            return null;
        }, function (tokens, item) {
            if (tokens[0] == token) {
                var tokens1 = [];
                for (var i = 1; i < tokens.length; i++) {
                    tokens1[i - 1] = tokens[i];
                }
                acc.insertChain1(tokens1, item);
            }
        });
        this.items = acc;
        this.chart.shift();
        return !this.items.isEmpty();
    };
    ParseState.prototype.complete = function (currentToken) {
        var acc = this.items.lookup(currentToken);
        if (acc == null)
            acc = new Trie();
        this.process(this.items.value, function (_fid) {
            return null;
        }, function (tokens, item) {
            if (currentToken == '' || tokens[0].indexOf(currentToken) == 0) {
                var tokens1 = [];
                for (var i = 1; i < tokens.length; i++) {
                    tokens1[i - 1] = tokens[i];
                }
                acc.insertChain1(tokens1, item);
            }
        });
        return acc;
    };
    ParseState.prototype.extractTrees = function () {
        this.process(this.items.value, function (_fid) {
            return null;
        }, function (_tokens, _item) {
        });
        var totalFIds = this.concrete.totalFIds;
        var forest = this.chart.forest;
        function go(fid) {
            if (fid < totalFIds) {
                return [new Fun('?')];
            }
            else {
                var trees_1 = [];
                var rules = forest[fid];
                for (var j in rules) {
                    var rule = rules[j];
                    if (rule.id == 'Const') {
                        trees_1.push(rule.lit);
                    }
                    else {
                        rule = rule;
                        var arg_ix = [];
                        var arg_ts = [];
                        for (var k in rule.args) {
                            arg_ix[k] = 0;
                            arg_ts[k] = go(rule.args[k].fid);
                        }
                        while (true) {
                            var t = new Fun(rule.fun.name);
                            for (var k = 0; k < arg_ts.length; k++) {
                                t.setArg(k, arg_ts[k][arg_ix[k]]);
                            }
                            trees_1.push(t);
                            var i = 0;
                            while (i < arg_ts.length) {
                                arg_ix[i]++;
                                if (arg_ix[i] < arg_ts[i].length)
                                    break;
                                arg_ix[i] = 0;
                                i++;
                            }
                            if (i >= arg_ts.length)
                                break;
                        }
                    }
                }
                return trees_1;
            }
        }
        var trees = [];
        var fids = this.concrete.startCats[this.startCat];
        if (fids != null) {
            var _loop_5 = function (fid0) {
                var labels = {};
                var rules = this_2.chart.expandForest(fid0);
                rules.forEach(function (rule) {
                    for (var lbl in rule.fun.lins) {
                        labels[lbl] = true;
                    }
                });
                for (var lbl0 in labels) {
                    var lbl = parseInt(lbl0);
                    var fid = this_2.chart.lookupPC(fid0, lbl, 0);
                    var arg_ts = go(fid);
                    for (var i in arg_ts) {
                        var isMember = false;
                        for (var j in trees) {
                            if (arg_ts[i].isEqual(trees[j])) {
                                isMember = true;
                                break;
                            }
                        }
                        if (!isMember)
                            trees.push(arg_ts[i]);
                    }
                }
            };
            var this_2 = this;
            for (var fid0 = fids.s; fid0 <= fids.e; fid0++) {
                _loop_5(fid0);
            }
        }
        return trees;
    };
    ParseState.prototype.process = function (agenda, literalCallback, tokenCallback) {
        if (agenda != null) {
            var _loop_6 = function () {
                var item = agenda.pop();
                var lin = item.seq;
                if (item.dot < lin.length) {
                    var sym0 = lin[item.dot];
                    switch (sym0.id) {
                        case 'Arg': {
                            var sym = sym0;
                            var fid = item.args[sym.i].fid;
                            var label = sym.label;
                            var items = this_3.chart.lookupAC(fid, label);
                            if (items == null) {
                                var rules = this_3.chart.expandForest(fid);
                                for (var j in rules) {
                                    var rule = rules[j];
                                    agenda.push(new ActiveItem(this_3.chart.offset, 0, rule.fun, rule.fun.lins[label], rule.args, fid, label));
                                }
                                this_3.chart.insertAC(fid, label, [item]);
                            }
                            else {
                                var isMember = false;
                                for (var j in items) {
                                    if (items[j].isEqual(item)) {
                                        isMember = true;
                                        break;
                                    }
                                }
                                if (!isMember) {
                                    items.push(item);
                                    var fid2 = this_3.chart.lookupPC(fid, label, this_3.chart.offset);
                                    if (fid2 != null) {
                                        agenda.push(item.shiftOverArg(sym.i, fid2));
                                    }
                                }
                            }
                            break;
                        }
                        case 'KS': {
                            var sym = sym0;
                            tokenCallback(sym.tokens, item.shiftOverTokn());
                            break;
                        }
                        case 'KP': {
                            var sym = sym0;
                            var pitem_1 = item.shiftOverTokn();
                            sym.tokens.forEach(function (symks) {
                                tokenCallback(symks.tokens, pitem_1);
                            });
                            sym.alts.forEach(function (alt) {
                                alt.tokens.forEach(function (symks) {
                                    tokenCallback(symks.tokens, pitem_1);
                                });
                            });
                            break;
                        }
                        case 'Lit': {
                            var sym = sym0;
                            var fid = item.args[sym.i].fid;
                            var rules = this_3.chart.forest[fid];
                            if (rules != null) {
                                tokenCallback(rules[0].toks, item.shiftOverTokn());
                            }
                            else {
                                var rule = literalCallback(fid);
                                if (rule != null) {
                                    fid = this_3.chart.nextId++;
                                    this_3.chart.forest[fid] = [rule];
                                    tokenCallback(rule.toks, item.shiftOverArg(sym.i, fid));
                                }
                            }
                            break;
                        }
                    }
                }
                else {
                    var fid_1 = this_3.chart.lookupPC(item.fid, item.lbl, item.offset);
                    if (fid_1 == null) {
                        fid_1 = this_3.chart.nextId++;
                        var items = this_3.chart.lookupACo(item.offset, item.fid, item.lbl);
                        if (items != null) {
                            items.forEach(function (pitem) {
                                var i = pitem.seq[pitem.dot].i;
                                agenda.push(pitem.shiftOverArg(i, fid_1));
                            });
                        }
                        this_3.chart.insertPC(item.fid, item.lbl, item.offset, fid_1);
                        this_3.chart.forest[fid_1] = [new Apply(item.fun, item.args)];
                    }
                    else {
                        var labels = this_3.chart.labelsAC(fid_1);
                        if (labels != null) {
                            for (var lbl in labels) {
                                agenda.push(new ActiveItem(this_3.chart.offset, 0, item.fun, item.fun.lins[lbl], item.args, fid_1, parseInt(lbl)));
                            }
                        }
                        var rules = this_3.chart.forest[fid_1];
                        var rule_2 = new Apply(item.fun, item.args);
                        var isMember_1 = false;
                        rules.forEach(function (rule1) {
                            if (rule1.isEqual(rule_2))
                                isMember_1 = true;
                        });
                        if (!isMember_1)
                            rules.push(rule_2);
                    }
                }
            };
            var this_3 = this;
            while (agenda.length > 0) {
                _loop_6();
            }
        }
    };
    return ParseState;
}());
var Chart = (function () {
    function Chart(concrete) {
        this.active = {};
        this.actives = [];
        this.passive = {};
        this.forest = {};
        this.nextId = concrete.totalFIds;
        this.offset = 0;
        for (var fid in concrete.pproductions) {
            this.forest[fid] = concrete.pproductions[fid];
        }
    }
    Chart.prototype.lookupAC = function (fid, label) {
        var tmp = this.active[fid];
        if (tmp == null)
            return null;
        return tmp[label];
    };
    Chart.prototype.lookupACo = function (offset, fid, label) {
        var tmp;
        if (offset == this.offset)
            tmp = this.active[fid];
        else
            tmp = this.actives[offset][fid];
        if (tmp == null)
            return null;
        return tmp[label];
    };
    Chart.prototype.labelsAC = function (fid) {
        return this.active[fid];
    };
    Chart.prototype.insertAC = function (fid, label, items) {
        var tmp = this.active[fid];
        if (tmp == null) {
            tmp = {};
            this.active[fid] = tmp;
        }
        tmp[label] = items;
    };
    Chart.prototype.lookupPC = function (fid, label, offset) {
        var key = fid + '.' + label + '-' + offset;
        return this.passive[key];
    };
    Chart.prototype.insertPC = function (fid1, label, offset, fid2) {
        var key = fid1 + '.' + label + '-' + offset;
        this.passive[key] = fid2;
    };
    Chart.prototype.shift = function () {
        this.actives.push(this.active);
        this.active = {};
        this.passive = {};
        this.offset++;
    };
    Chart.prototype.expandForest = function (fid) {
        var rules = [];
        var forest = this.forest;
        var go = function (rules0) {
            for (var i in rules0) {
                var rule = rules0[i];
                switch (rule.id) {
                    case 'Apply':
                        rules.push(rule);
                        break;
                    case 'Coerce':
                        go(forest[rule.arg]);
                        break;
                }
            }
        };
        go(this.forest[fid]);
        return rules;
    };
    return Chart;
}());
var ActiveItem = (function () {
    function ActiveItem(offset, dot, fun, seq, args, fid, lbl) {
        this.offset = offset;
        this.dot = dot;
        this.fun = fun;
        this.seq = seq;
        this.args = args;
        this.fid = fid;
        this.lbl = lbl;
    }
    ActiveItem.prototype.isEqual = function (obj) {
        return (this.offset == obj.offset &&
            this.dot == obj.dot &&
            this.fun == obj.fun &&
            this.seq == obj.seq &&
            this.args == obj.args &&
            this.fid == obj.fid &&
            this.lbl == obj.lbl);
    };
    ActiveItem.prototype.shiftOverArg = function (i, fid) {
        var nargs = [];
        for (var k in this.args) {
            nargs[k] = this.args[k];
        }
        nargs[i] = new PArg(fid);
        return new ActiveItem(this.offset, this.dot + 1, this.fun, this.seq, nargs, this.fid, this.lbl);
    };
    ActiveItem.prototype.shiftOverTokn = function () {
        return new ActiveItem(this.offset, this.dot + 1, this.fun, this.seq, this.args, this.fid, this.lbl);
    };
    return ActiveItem;
}());
function isUndefined(a) {
    return typeof a == 'undefined';
}
Object.defineProperty(String.prototype, 'startsWith', {
    value: function (search, pos) {
        pos = !pos || pos < 0 ? 0 : +pos;
        return this.substring(pos, pos + search.length) === search;
    }
});
