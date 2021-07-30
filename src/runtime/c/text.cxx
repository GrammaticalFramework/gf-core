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
