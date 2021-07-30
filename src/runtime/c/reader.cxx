#include "data.h"
#include "reader.h"
#include <math.h>
#include <string.h>

PgfReader::PgfReader(std::istream *in)
{
    this->in = in;
}

uint8_t PgfReader::read_uint8()
{
    uint8_t b;
    in->read((char*) &b, sizeof(b));
    if (in->eof())
        throw pgf_error("reached end of file while reading a grammar");
    if (in->fail())
        throw std::system_error(errno, std::generic_category());

	return b;
}

uint16_t PgfReader::read_u16be()
{
    uint8_t buf[2];
    in->read((char*) &buf, sizeof(buf));
    if (in->eof())
        throw pgf_error("reached end of file while reading a grammar");
    if (in->fail())
        throw std::system_error(errno, std::generic_category());

	return (((uint16_t) buf[0]) << 8 | buf[1]);
}

uint64_t PgfReader::read_u64be()
{
    uint8_t buf[8];
    in->read((char*) &buf, sizeof(buf));
    if (in->eof())
        throw pgf_error("reached end of file while reading a grammar");
    if (in->fail())
        throw std::system_error(errno, std::generic_category());

	return (((uint64_t) buf[0]) << 56 |
            ((uint64_t) buf[1]) << 48 |
            ((uint64_t) buf[2]) << 40 |
            ((uint64_t) buf[3]) << 32 |
            ((uint64_t) buf[4]) << 24 |
            ((uint64_t) buf[5]) << 16 |
            ((uint64_t) buf[6]) << 8  |
            ((uint64_t) buf[7]));
}

double PgfReader::read_double()
{
    uint64_t u = read_u64be();

    bool sign = u >> 63;
	unsigned rawexp = u >> 52 & 0x7ff;
	uint64_t mantissa = u & 0xfffffffffffff;
	double ret;

	if (rawexp == 0x7ff) {
		ret = (mantissa == 0) ? INFINITY : NAN;
	} else {
		uint64_t m = rawexp ? 1ULL << 52 | mantissa : mantissa << 1;
		ret = ldexp((double) m, rawexp - 1075);
	}
	return sign ? copysign(ret, -1.0) : ret;
}

uint64_t PgfReader::read_uint()
{
	uint64_t u = 0;
	int shift = 0;
	uint8_t b = 0;
	do {
		b = read_uint8();
		u |= (b & ~0x80) << shift;
		shift += 7;
	} while (b & 0x80);
	return u;
}

moffset PgfReader::read_name(size_t size)
{
    size_t len = read_len();

    char* buf = (char*) alloca(len*6+1);
	char* p   = buf;
	for (size_t i = 0; i < len; i++) {
        uint8_t c = read_uint8();
        *(p++) = (char) c;

        if (c < 0x80) {
            continue;
        }
        if (c < 0xc2) {
            throw pgf_error("utf8 decoding error");
        }

        int len = (c < 0xe0 ? 1 :
                   c < 0xf0 ? 2 :
                   c < 0xf8 ? 3 :
                   c < 0xfc ? 4 :
                              5
                   );
        // If reading the extra bytes causes EOF, it is an encoding
        // error, not a legitimate end of character stream.
        in->read(p, len);
        if (in->eof())
            throw pgf_error("utf8 decoding error");
        if (in->fail())
            throw std::system_error(errno, std::generic_category());

        p += len;
	}
	*p++ = 0;

	moffset offs = current_db->malloc(size+(p-buf));
	strcpy((char*) (current_base+offs+size), buf);

	return offs;
}

template<class V>
Namespace<V> PgfReader::read_namespace(ref<V> (PgfReader::*read_value)())
{
    size_t len = read_len();
    Namespace<V> nmsp = 0;
    for (size_t i = 0; i < len; i++) {
        ref<V> value = (this->*read_value)();
        nmsp = namespace_insert(nmsp, value);
    }
    return nmsp;
}

PgfLiteral PgfReader::read_literal()
{
    PgfLiteral lit = 0;

    uint8_t tag = read_tag();
    switch (tag) {
	case PGF_LITERAL_STR: {
		ref<PgfLiteralStr> lit_str =
            read_name(offsetof(PgfLiteralStr,val));
        lit = variant_close(lit_str,PGF_LITERAL_STR);
		break;
	}
	case PGF_LITERAL_INT: {
		ref<PgfLiteralInt> lit_int =
			current_db->malloc<PgfLiteralInt>();
		lit_int->val = read_int();
        lit = variant_close(lit_int,PGF_LITERAL_INT);
		break;
	}
	case PGF_LITERAL_FLT: {
		ref<PgfLiteralFlt> lit_flt =
			current_db->malloc<PgfLiteralFlt>();
		lit_flt->val = read_double();
        lit = variant_close(lit_flt,PGF_LITERAL_FLT);
		break;
	}
	default:
		throw pgf_error("tag error");
	}
	return lit;
}

ref<PgfFlag> PgfReader::read_flag()
{
    ref<PgfFlag> flag = read_name<PgfFlag>();
    flag->value = read_literal();
    return flag;
}

void PgfReader::read_abstract(PgfAbstr* abstract)
{
    abstract->name = read_name(0);
	abstract->aflags = read_namespace<PgfFlag>(&PgfReader::read_flag);
}

void PgfReader::read_pgf(PgfPGFRoot *pgf)
{
    pgf->major_version = read_u16be();
    pgf->minor_version = read_u16be();
    pgf->gflags = read_namespace<PgfFlag>(&PgfReader::read_flag);

    read_abstract(&pgf->abstract);
}
