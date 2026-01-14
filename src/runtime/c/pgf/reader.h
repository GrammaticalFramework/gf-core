#ifndef READER_H_
#define READER_H_

#include "db.h"

// reader for PGF files

class PGF_INTERNAL_DECL PgfReader
{
public:
    PgfReader(FILE *in,PgfProbsCallback *probs_callback);

    uint8_t read_uint8();
    uint16_t read_u16be();
    uint64_t read_u64be();
    double read_double();
    prob_t read_prob(PgfText *name);
    uint64_t read_uint();
    int64_t read_int() { return (int64_t) read_uint(); };
    size_t  read_len() { return (size_t) read_uint(); };

    uint8_t read_tag() { return read_uint8(); }

    template<class V>
    ref<V> read_name(PgfText V::* field) {
        return read_text_internal((size_t) &(((V*) NULL)->*field));
    };

    ref<PgfText> read_name() {
        return read_text_internal(0);
    };

    template<class V>
    ref<V> read_text(PgfText V::* field) {
        return read_text_internal((size_t) &(((V*) NULL)->*field));
    };

    ref<PgfText> read_text() {
        return read_text_internal(0);
    };

    template<class V>
    Namespace<V> read_namespace(ref<V> (PgfReader::*read_value)(), size_t len);

    template<class V>
    Namespace<V> read_namespace(ref<V> (PgfReader::*read_value)());

    template<class V>
    void merge_namespace(ref<V> (PgfReader::*read_value)());

    template <class C, class V>
    ref<C> read_vector(inline_vector<V> C::* field, void (PgfReader::*read_value)(ref<V> val));

    template<class V>
    vector<V> read_null_vector(void (PgfReader::*read_value)(ref<V> val));

    template<class V>
    vector<V> read_vector(void (PgfReader::*read_value)(ref<V> val));

    PgfLiteral read_literal();
    PgfExpr read_expr();
    void read_expr(ref<PgfExpr> r) { auto res = read_expr(); *r = res; };

    void read_hypo(ref<PgfHypo> hypo);
    ref<PgfDTyp> read_type();

    ref<PgfFlag> read_flag();

    ref<PgfAbsFun> read_absfun();
    ref<PgfAbsFun> merge_absfun();
    ref<PgfAbsFun> read_absfun_only();
    ref<PgfAbsCat> read_abscat();
    void read_abstract(ref<PgfAbstr> abstract);
    void merge_abstract(ref<PgfAbstr> abstract);

    ref<PgfConcrRule> read_rule();
    ref<PgfConcrLincat> read_lincat();
    vector<ref<PgfText>> read_lincat_fields(ref<PgfConcrLincat> lincat);
    ref<PgfLParam> read_lparam();
    void read_variable_range(ref<size_t> var_range);
    void read_parg(ref<PgfPArg> parg);
    PgfSymbol read_symbol();
    ref<PgfConcrLin> read_lin();
    ref<PgfConcrPrintname> read_printname();

    ref<PgfConcr> read_concrete();

    ref<PgfPGF> read_pgf();
    void merge_pgf(ref<PgfPGF> pgf);

private:
    FILE *in;
    PgfProbsCallback *probs_callback;
    ref<PgfAbstr> abstract;
    ref<PgfConcr> concrete;
    object container;

    class PgfParseTableMaker *table_maker;

    object read_name_internal(size_t struct_size);
    object read_text_internal(size_t struct_size);

    void read_text2(ref<ref<PgfText>> r) { auto text = read_text(); *r = text; }
    void read_lparam(ref<ref<PgfLParam>> r) { auto lparam = read_lparam(); *r = lparam; }
    void read_rule2(ref<ref<PgfConcrRule>> r) { auto rule = read_rule(); *r = rule; }
    void read_symbol2(ref<PgfSymbol> r) { auto sym = read_symbol(); *r = sym; }

    template<class I>
    ref<I> read_symbol_idx();
};

#endif
