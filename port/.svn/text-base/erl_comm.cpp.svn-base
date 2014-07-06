#include <cstdlib>
#include <cstdio>

#include "erl_comm.h"

namespace ErlComm {

int read_cmd(byte **buf)
{
  int len;
  byte len_buf[4];

  if (read_exact(len_buf, 4) != 4)
    return(-1);
  len = (len_buf[0] << 24) | (len_buf[1] << 16) | (len_buf[2] << 8) | len_buf[3];
  *buf = (byte*) malloc(len * sizeof(byte));
  return read_exact(*buf, len);
}

int write_cmd(byte *buf, int len)
{
  byte li;

  li = (len >> 24) & 0xff;
  write_exact(&li, 1);

  li = (len >> 16) & 0xff;
  write_exact(&li, 1);

  li = (len >> 8) & 0xff;
  write_exact(&li, 1);
  
  li = len & 0xff;
  write_exact(&li, 1);

  return write_exact(buf, len);
}

int read_exact(byte *buf, int len)
{
  int i, got=0;

  do {
    if ((i = read(0, buf+got, len-got)) <= 0)
      return(i);
    got += i;
  } while (got<len);

  return(len);
}

int write_exact(byte *buf, int len)
{
  int i, wrote = 0;

  do {
    if ((i = write(1, buf+wrote, len-wrote)) <= 0)
      return (i);
    wrote += i;
  } while (wrote<len);

  return (len);
}


} // namespace
