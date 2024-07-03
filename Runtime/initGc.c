#include "pict.h"
#include <stdio.h>
#include <stdlib.h>

void initGc (void)
{
  char c; int sz = 0;
  /*
   * The environment variable PICTHEAP overrides the builtin heap size.
   */
  char *s = getenv("PICTHEAP");
  /*
   * Convert s to an integer (we interpret the integer as the number of
   * kilobytes of heap required).  We ignore s if it is not a valid integer
   * and use the builtin value.
   */
  if (s != NULL) {
    while ((c = *s++)) {
      if (c >= '0' && c <= '9') {
	sz = sz * 10 + (c - '0') * 1024;
      } else {
	sz = heapSizeInWords;
	break;
      }
    }
  } else {
    sz = heapSizeInWords;
  }
  /*
   * Allocate the two semi-spaces.
   */
  fromSpace = malloc(sz*sizeof(Val)); fromLimit = fromSpace + sz;
  toSpace = malloc(sz*sizeof(Val)); toLimit = toSpace + sz;
  /*
   * Exit if the allocations failed.
   */
  if (fromSpace == NULL || toSpace == NULL) {
    fprintf(stderr,"Couldn't allocate heap of size %d words\n",sz);
    exit(1);
  } else {
    Free = fromSpace; StartQ = fromLimit-1; EndQ = fromLimit-1;
  }
}
