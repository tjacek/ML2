#ifndef __MLLIB__
#define __MLLIB__

extern "C" {
#include <erl_interface.h>
#include <ei.h>
}


#define ATTR_NOMINAL 0
#define ATTR_CONTINUOUS 1

enum att_type {
	nominal,
	ordered,
	continuous
};

struct lst {
	ETERM* head;
	ETERM* tail;
};

att_type* get_atts_types(ETERM* buf);

#endif


