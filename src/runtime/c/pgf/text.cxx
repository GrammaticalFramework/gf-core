#include "data.h"

PGF_INTERNAL
int textcmp(PgfText *t1, PgfText *t2)
{
    for (size_t i = 0; ; i++) {
        if (i >= t1->size)
            return (i - t2->size);
        if (i >= t2->size)
            return 1;

        if (t1->text[i] > t2->text[i])
            return 1;
        else if (t1->text[i] < t2->text[i])
            return -1;
    }
}

PGF_INTERNAL
PgfText* textdup(PgfText *t1)
{
    size_t size = sizeof(PgfText)+t1->size+1;
    PgfText *t2 = (PgfText *) malloc(size);
    if (t2 != NULL)
        memcpy(t2, t1, size);
    return t2;
}

PGF_API uint32_t
pgf_utf8_decode(const uint8_t** src_inout)
{
	const uint8_t* src = *src_inout;
	uint8_t c = src[0];
	if (c < 0x80) {
		*src_inout = src + 1;
		return c;
	}
	size_t len = (c < 0xe0 ? 1 :
	              c < 0xf0 ? 2 :
	              c < 0xf8 ? 3 :
	              c < 0xfc ? 4 :
	                         5
	             );
	uint64_t mask = 0x0103070F1f7f;
	uint32_t u = c & (mask >> (len * 8));
	for (size_t i = 1; i <= len; i++) {
		c = src[i];
		u = u << 6 | (c & 0x3f);
	}
	*src_inout = &src[len + 1];
	return u;
}

PGF_API void
pgf_utf8_encode(uint32_t ucs, uint8_t** buf)
{
	uint8_t* p = *buf;
	if (ucs < 0x80) {
		p[0] = (uint8_t) ucs;
		*buf = p+1;
	} else if (ucs < 0x800) {
		p[0] = 0xc0 | (ucs >> 6);
		p[1] = 0x80 | (ucs & 0x3f);
		*buf = p+2;
	} else if (ucs < 0x10000) {
		p[0] = 0xe0 | (ucs >> 12);
		p[1] = 0x80 | ((ucs >> 6) & 0x3f);
		p[2] = 0x80 | (ucs & 0x3f);
		*buf = p+3;
	} else if (ucs < 0x200000) {
		p[0] = 0xf0 | (ucs >> 18);
		p[1] = 0x80 | ((ucs >> 12) & 0x3f);
		p[2] = 0x80 | ((ucs >> 6) & 0x3f);
		p[3] = 0x80 | (ucs & 0x3f);
		*buf = p+4;
	} else if (ucs < 0x4000000) {
		p[0] = 0xf8 | (ucs >> 24);
		p[1] = 0x80 | ((ucs >> 18) & 0x3f);
		p[2] = 0x80 | ((ucs >> 12) & 0x3f);
		p[3] = 0x80 | ((ucs >>  6) & 0x3f);
		p[4] = 0x80 | (ucs & 0x3f);
		*buf = p+5;
	} else {
		p[0] = 0xfc | (ucs >> 30);
		p[1] = 0x80 | ((ucs >> 24) & 0x3f);
		p[2] = 0x80 | ((ucs >> 18) & 0x3f);
		p[3] = 0x80 | ((ucs >> 12) & 0x3f);
		p[4] = 0x80 | ((ucs >> 6) & 0x3f);
		p[5] = 0x80 | (ucs & 0x3f);
		*buf = p+6;
	}
}
