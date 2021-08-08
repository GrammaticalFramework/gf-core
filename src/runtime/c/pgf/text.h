#ifndef TEXT_H
#define TEXT_H

PGF_INTERNAL_DECL
int textcmp(PgfText *t1, PgfText *t2);

PGF_INTERNAL_DECL
PgfText* textdup(PgfText *t1);

PGF_API uint32_t
pgf_utf8_decode(const uint8_t** src_inout);

#endif
