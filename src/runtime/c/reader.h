#ifndef READER_H_
#define READER_H_

#include <fstream>
#include <stdint.h>
#include "db.h"

// reader for PGF files

class PGF_INTERNAL_DECL PgfReader
{
public:
    PgfReader(std::istream *in);

    uint8_t read_uint8();
    uint16_t read_u16be();
    uint64_t read_u64be();
    double read_double();
    uint64_t read_uint();
    int64_t read_int() { return (int64_t) read_uint(); };
    uint8_t read_tag() { return read_uint8(); }
    size_t  read_len() { return (size_t) read_uint(); };

    template<class V>
    ref<V> read_name() { return read_name(offsetof(V,name)); };

    template<class V>
    Namespace<V> read_namespace(ref<V> (PgfReader::*read_value)());

    PgfLiteral read_literal();
    ref<PgfFlag> read_flag();

    void read_abstract(PgfAbstr* abstract);

    void read_pgf(PgfPGFRoot* pgf);

private:
    std::istream *in;

    moffset read_name(size_t size);
};

#endif
