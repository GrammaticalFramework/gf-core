#ifndef TEXT_H
#define TEXT_H

typedef struct {
    size_t size;
    char text[];
} PgfText;

PGF_INTERNAL_DECL
int textcmp(PgfText &t1, PgfText &t2);

#endif
