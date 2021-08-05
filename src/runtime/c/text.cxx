#include "data.h"

PGF_INTERNAL
int textcmp(PgfText &t1, PgfText &t2)
{
    for (size_t i = 0; ; i++) {
        if (i >= t1.size)
            return (i - t2.size);
        if (i >= t2.size)
            return 1;

        if (t1.text[i] > t2.text[i])
            return 1;
        else if (t1.text[i] < t2.text[i])
            return -1;

        i++;
    }
}

PGF_INTERNAL
PgfText* textdup(PgfText *t1)
{
    PgfText *t2 = (PgfText *) malloc(sizeof(PgfText) + t1->size + 1);
    t2->size = t1->size;
    memcpy(t2->text, t1->text, t1->size+1);
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


