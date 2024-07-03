#include "pict.h"
#include <stdio.h>

void ComplexInput (Val ch, Val closure)
{
#ifdef PROFILING
  cc.INPUT++;
#endif
  switch (STATUS(ch)) {
  case Empty:
#ifdef PROFILING
    cc.INPUTe++;
#endif
    STATUS(ch) = OneReader;
    VAL(ch) = closure;
    return;
  case OneWriter:
#ifdef PROFILING
    cc.INPUT1w++; cc.runqAlloc += 2;
#endif
    STATUS(ch) = Empty;
    EndQ[0] = closure;
    EndQ[-1] = VAL(ch);
    EndQ -= 2;
    return;
  case OneReader:
#ifdef PROFILING
    cc.INPUT1r++; cc.fifoAlloc += 9;
#endif
    Free[0] = TUPLE(3);
    Free[1] = TAG(Free+3);
    Free[2] = TAG(Free+6);
    Free[3] = TUPLE(3);
    Free[4] = VAL(ch);
    Free[5] = TAG(Free+6);
    Free[6] = TUPLE(3);
    Free[7] = closure;
    Free[8] = Zero;
    STATUS(ch) = ManyReaders;
    VAL(ch) = TAG(Free);
    Free += 9;
    return;
  case ManyReaders:
#ifdef PROFILING
    cc.INPUTmr++; cc.fifoAlloc += 3;
#endif
    {
      Val fifo = VAL(ch);
      Val last = OFFSET(fifo,2);
      OFFSET(last,2) = TAG(Free);
      OFFSET(fifo,2) = TAG(Free);
    }
    Free[0] = TUPLE(3);
    Free[1] = closure;
    Free[2] = Zero;
    Free += 3;
    return;
  case ManyWriters:
#ifdef PROFILING
    cc.INPUTmw++; cc.runqAlloc += 2;
#endif
    {
      Val fifo = VAL(ch);
      Val first = OFFSET(fifo,1);
      Val next = OFFSET(first,2);
      EndQ[0] = closure;
      EndQ[-1] = OFFSET(first,1);
      EndQ -= 2;
      if (next == OFFSET(fifo,2)) {
        STATUS(ch) = OneWriter;
        VAL(ch) = OFFSET(next,1);
      } else {
        OFFSET(fifo,1) = next;
      }
    }
    return;
  default:
    fprintf(stderr,"Internal error: bad complex input\n");
    exit(1);
    return;
  }
}
