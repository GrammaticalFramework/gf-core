#ifndef READER_H_
#define READER_H_

#include "db.h"

// reader for PGF files

class PGF_INTERNAL_DECL PgfReader
{
public:
    PgfReader(FILE *in);

    uint8_t read_uint8();
    uint16_t read_u16be();
    uint64_t read_u64be();
    double read_double();
    uint64_t read_uint();
    int64_t read_int() { return (int64_t) read_uint(); };
    size_t  read_len() { return (size_t) read_uint(); };

    uint8_t read_tag() { return read_uint8(); }

    template<class V>
    ref<V> read_name(PgfText V::* field) {
        return read_name_internal((size_t) &(((V*) NULL)->*field));
    };

    ref<PgfText> read_name() {
        return read_name_internal(0);
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

    template <class C, class V>
    ref<C> read_vector(Vector<V> C::* field, void (PgfReader::*read_value)(ref<V> val));

    template<class V>
    ref<Vector<V>> read_vector(void (PgfReader::*read_value)(ref<V> val));

    PgfLiteral read_literal();
    PgfExpr read_expr();
    void read_expr(ref<PgfExpr> r) { *r = read_expr(); };

    void read_hypo(ref<PgfHypo> hypo);
    ref<PgfDTyp> read_type();

    ref<PgfFlag> read_flag();

    ref<PgfAbsFun> read_absfun();
    ref<PgfAbsCat> read_abscat();
    void read_abstract(ref<PgfAbstr> abstract);

    ref<PgfConcrLincat> read_lincat();
    ref<PgfLParam> read_lparam();
    void read_variable_range(ref<PgfVariableRange> var_info);
    void read_parg(ref<PgfPArg> parg);
    ref<PgfPResult> read_presult();
    PgfSymbol read_symbol();
    ref<PgfConcrLin> read_lin();
    ref<PgfConcrPrintname> read_printname();

    ref<PgfConcr> read_concrete();

    ref<PgfPGF> read_pgf();

private:
    FILE *in;
    ref<PgfAbstr> abstract;

    object read_name_internal(size_t struct_size);
    object read_text_internal(size_t struct_size);

    void read_text2(ref<ref<PgfText>> r) { auto text = read_text(); *r = text; };
    void read_lparam(ref<ref<PgfLParam>> r) { auto lparam = read_lparam(); *r = lparam; };
    void read_symbol2(ref<PgfSymbol> r) { auto sym = read_symbol(); *r = sym; };
    void read_seq2(ref<ref<Vector<PgfSymbol>>> r) { auto seq = read_vector(&PgfReader::read_symbol2); *r = seq; }
    void read_presult2(ref<ref<PgfPResult>> r) { auto res = read_presult(); *r = res; }

    template<class I>
    ref<I> read_symbol_idx();
};

#endif
