#include <HsFFI.h>
#include <pgf/pgf.h>

typedef struct {
	PgfLiteralCallback callback;
	GuFinalizer fin;
} HSPgfLiteralCallback;

static void 
hspgf_literal_callback_fin(GuFinalizer* self)
{
	HSPgfLiteralCallback* callback = gu_container(self, HSPgfLiteralCallback, fin);
	
	if (callback->callback.match != NULL)
		hs_free_fun_ptr((HsFunPtr) callback->callback.match);
	if (callback->callback.predict != NULL)
		hs_free_fun_ptr((HsFunPtr) callback->callback.predict);
}

void
hspgf_callbacks_map_add_literal(PgfConcr* concr, PgfCallbacksMap* callbacks,
                                PgfCId cat, HsFunPtr match, HsFunPtr predict,
                                GuPool* pool)
{
	HSPgfLiteralCallback* callback = gu_new(HSPgfLiteralCallback, pool);
	callback->callback.match   = (void*) match;
	callback->callback.predict = (void*) predict;
	callback->fin.fn = hspgf_literal_callback_fin;
	gu_pool_finally(pool, &callback->fin);
	pgf_callbacks_map_add_literal(concr, callbacks, cat, &callback->callback);
}
