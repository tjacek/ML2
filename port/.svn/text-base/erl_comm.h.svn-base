#ifndef __ERL_COMM__
#define __ERL_COMM__

extern "C" {
#include <erl_interface.h>
#include <ei.h>
#include <unistd.h>
}

typedef unsigned char byte;


namespace ErlComm
{
	int read_cmd(byte **buf);
	int write_cmd(byte *buf, int len);
	int read_exact(byte *buf, int len);
	int write_exact(byte *buf, int len);
}
#endif

