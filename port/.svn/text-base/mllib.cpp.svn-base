#include <cstdlib>
#include "mllib.h"

int is_empty(ETERM* list)
{
	return erl_hd(list) == NULL;
}

lst next(ETERM* list, lst* res)
{
	res->head = erl_hd(list);
	res->tail = erl_tl(list);
}

att_type* get_atts_types(ETERM* term)
{
	lst point;
	int len = erl_length(term), i;
	att_type* res = (att_type*) malloc(len * sizeof(att_type));
	att_type tmp;
	next(term, &point);
	
	for (i=0; i<len; i++) 
	{
		switch (ERL_INT_VALUE(point.head)) 
		{
			case 0:
				tmp = nominal;
				break;

			case 1:
				tmp = continuous;
				break;
		}
		res[i] = tmp;
		next(term, &point);
	}

	return res;
}


