#ifndef ALIGNER_H
#define ALIGNER_H

#include <vector>

class PGF_INTERNAL_DECL PgfAlignerOutput : public PgfLinearizationOutputIface {
public:
    PgfAlignerOutput();
    ~PgfAlignerOutput();

    virtual void symbol_token(PgfText *tok);
    virtual void begin_phrase(PgfText *cat, int fid, PgfText *ann, PgfText *fun);
    virtual void end_phrase(PgfText *cat, int fid, PgfText *ann, PgfText *fun);
    virtual void symbol_ne();
    virtual void symbol_bind();
    virtual void flush();

    PgfAlignmentPhrase **get_phrases(size_t *n_phrases);

    static void free_phrases(PgfAlignmentPhrase **phrases, size_t n_phrases);

private:
    bool bind;
    bool nonexist;
    std::vector<int> parent_current;
    std::vector<int> parent_stack;
    size_t n_phrases;
    PgfAlignmentPhrase *last_phrase;
    PgfAlignmentPhrase **phrases;
    size_t n_matches;
    PgfPrinter printer;

    void push_parent(int fid);
};

#endif
