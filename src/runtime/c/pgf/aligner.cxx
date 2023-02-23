#include "data.h"
#include "printer.h"
#include "aligner.h"

PgfAlignerOutput::PgfAlignerOutput() : printer(NULL, 0, NULL)
{
    n_phrases = 0;
    last_phrase = NULL;
    phrases = NULL;
    n_matches = 0;
    bind = true;
    nonexist = false;
}

PgfAlignerOutput::~PgfAlignerOutput()
{
    free_phrases(phrases, n_phrases);
}

void PgfAlignerOutput::free_phrases(PgfAlignmentPhrase **phrases, size_t n_phrases)
{
    if (phrases) {
        for (size_t i = 0; i < n_phrases; i++) {
            free(phrases[i]->phrase);
            free(phrases[i]);
        }
        free(phrases);
    }
}

PgfAlignmentPhrase **PgfAlignerOutput::get_phrases(size_t *n_phrases)
{
    if (nonexist) {
        *n_phrases = 0;
        return NULL;
    }

    *n_phrases = this->n_phrases;
    PgfAlignmentPhrase **res = phrases;
    this->n_phrases = 0;
    this->last_phrase = NULL;
    this->phrases = NULL;
    return res;
}

void PgfAlignerOutput::push_parent(int fid)
{
    parent_current.push_back(fid);

	if (last_phrase != NULL) {
		for (size_t i = 0; i < last_phrase->n_fids; i++) {
			if (fid == last_phrase->fids[i]) {
				n_matches++;
				break;
			}
		}
	}
}

void PgfAlignerOutput::symbol_token(PgfText *tok)
{
    if (nonexist)
        return;

    size_t n_parents = parent_stack.size();
    int fid = parent_stack.back();

    // how many nodes so far are involved in the current compound word
	size_t n_fids = parent_current.size();

    if (bind) {
		// here we glue tokens

		bind = false;
        
        bool found = false;
		for (int current_fid : parent_current) {
			if (fid == current_fid) {
				found = true;
				break;
			}
		}
        
        // add the tree node id to the list of parents if it has not
		// been added already.
		if (!found) {
			push_parent(fid);
		}
	} else {
		// here we start a new (compound) word
        flush();
        parent_current.clear();
        push_parent(fid);
    }

    printer.puts(tok);
}

void PgfAlignerOutput::begin_phrase(PgfText *cat, int fid, PgfText *ann, PgfText *fun)
{
    parent_stack.push_back(fid);
}

void PgfAlignerOutput::end_phrase(PgfText *cat, int fid, PgfText *ann, PgfText *fun)
{
    parent_stack.pop_back();
}

void PgfAlignerOutput::symbol_ne()
{
    nonexist = true;
}

void PgfAlignerOutput::symbol_bind()
{
    bind = true;
}

void PgfAlignerOutput::flush()
{
    size_t n_fids = parent_current.size();

	if (n_matches == n_fids &&
		n_matches == last_phrase->n_fids) {
		// if the current compound word has the same parents
		// as the last one then we just combine them with a space

        PgfText *phrase = printer.get_text();
        printer.puts(last_phrase->phrase); free(last_phrase->phrase);
        printer.puts(" ");
        printer.puts(phrase);              free(phrase);

		last_phrase->phrase = printer.get_text();
	} else {
		// push the current word to the buffer of words

		PgfAlignmentPhrase* phrase = (PgfAlignmentPhrase*)
            malloc(sizeof(PgfAlignmentPhrase)+n_fids*sizeof(int));
		phrase->phrase = printer.get_text();
		phrase->n_fids = n_fids;
		for (size_t i = 0; i < n_fids; i++) {
			phrase->fids[i] = parent_current[i];
		}

        phrases = (PgfAlignmentPhrase**)
            realloc(phrases, (n_phrases+1)*sizeof(PgfAlignmentPhrase*));
		phrases[n_phrases++] = phrase;

		last_phrase = phrase;
	}

	n_matches = 0;
}
