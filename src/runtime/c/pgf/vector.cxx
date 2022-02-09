#include "data.h"

static
uint8_t next_padovan_tbl[] =
  {0,1,2,3,4,5
  ,7,7
  ,9,9
  ,12,12,12
  ,16,16,16,16
  ,21,21,21,21,21
  ,28,28,28,28,28,28,28
  ,37,37,37,37,37,37,37,37,37
  };

PGF_INTERNAL size_t
get_next_padovan(size_t min)
{
	if (min <= 37)
		return next_padovan_tbl[min];

	size_t a = 49, b = 65, c = 86;
	while (min > a) {
		if (b < a) {
			// overflow
			throw pgf_systemerror(ENOMEM);
		}
		size_t tmp = a + b;
		a = b;
		b = c;
		c = tmp;
	}
	return a;
}
