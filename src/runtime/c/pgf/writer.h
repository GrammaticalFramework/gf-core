#ifndef WRITER_H
#define WRITER_H

class PGF_INTERNAL_DECL PgfWriter
{
public:
    PgfWriter(FILE *out, const char *filepath);

    void write_uint8(uint8_t b);
    void write_u16be(uint16_t u);
    void write_u64be(uint64_t u);
    void write_double(double d);
    void write_uint(uint64_t u);
    void write_int(int64_t i) { write_uint((uint64_t) i); };
    void write_len(size_t len) { write_uint((uint64_t) len); };

    void write_tag(uint8_t t) { write_uint8(t); }

    void write_name(PgfText *text);
    void write_text(PgfText *text);

    template<class V>
    void write_namespace(Namespace<V> nmsp, void (PgfWriter::*write_value)(ref<V>));

    template<class V>
    void write_vector(ref<PgfVector<V>> vec, void (PgfWriter::*write_value)(ref<V> val));

    void write_literal(PgfLiteral literal);
    void write_expr(PgfExpr expr);
    void write_expr(ref<PgfExpr> r) { write_expr(*r); };

    void write_hypo(ref<PgfHypo> hypo);
    void write_type(ref<PgfDTyp> ty);

    void write_patt(PgfPatt patt);
    void write_patt(ref<PgfPatt> r) { write_patt(*r); };
    void write_defn(ref<ref<PgfEquation>> r);

    void write_flag(ref<PgfFlag> flag);

    void write_absfun(ref<PgfAbsFun> absfun);
    void write_abscat(ref<PgfAbsCat> abscat);
    void write_abstract(ref<PgfAbstr> abstract);

    void write_pgf(ref<PgfPGF> pgf);

private:
    template<class V>
    void write_namespace_helper(Namespace<V> nmsp, void (PgfWriter::*write_value)(ref<V>));

    FILE *out;
    const char* filepath;
    ref<PgfAbstr> abstract;
};

#endif
