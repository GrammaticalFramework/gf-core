#ifndef TEXT_H
#define TEXT_H

PGF_INTERNAL_DECL
int textcmp(PgfText *t1, PgfText *t2);

PGF_INTERNAL_DECL
bool textstarts(PgfText *t, PgfText *prefix);

PGF_INTERNAL_DECL
PgfText* textdup(PgfText *t1);

PGF_INTERNAL_DECL
ref<PgfText> textdup_db(PgfText *t1);

PGF_API uint32_t
pgf_utf8_decode(const uint8_t** src_inout);

PGF_API void
pgf_utf8_encode(uint32_t ucs, uint8_t** buf);

PGF_INTERNAL_DECL
uint32_t pgf_utf8_to_upper(uint32_t c);

#endif
