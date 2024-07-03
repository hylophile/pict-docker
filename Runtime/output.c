#include "pict.h"
#include <stdio.h>

void ComplexOutput (Val ch, Val a)
{
#ifdef PROFILING
  cc.OUTPUT++;
#endif
  switch (STATUS(ch)) {
  case Empty:
#ifdef PROFILING
    cc.OUTPUTe++;
#endif
    STATUS(ch) = OneWriter;
    VAL(ch) = a;
    return;
  case OneReader:
#ifdef PROFILING
    cc.OUTPUT1r++; cc.runqAlloc += 2;
#endif
    STATUS(ch) = Empty;
    EndQ[0] = VAL(ch);
    EndQ[-1] = a;
    EndQ -= 2;
    return;
  case OneWriter:
#ifdef PROFILING
    cc.OUTPUT1w++; cc.fifoAlloc += 9;
#endif
    Free[0] = TUPLE(3);
    Free[1] = TAG(Free+3);
    Free[2] = TAG(Free+6);
    Free[3] = TUPLE(3);
    Free[4] = VAL(ch);
    Free[5] = TAG(Free+6);
    Free[6] = TUPLE(3);
    Free[7] = a;
    Free[8] = Zero;
    STATUS(ch) = ManyWriters;
    VAL(ch) = TAG(Free);
    Free += 9;
    return;
  case ManyWriters:
#ifdef PROFILING
    cc.OUTPUTmw++; cc.fifoAlloc += 3;
#endif
    {
      Val fifo = VAL(ch);
      Val last = OFFSET(fifo,2);
      OFFSET(last,2) = TAG(Free);
      OFFSET(fifo,2) = TAG(Free);
    }
    Free[0] = TUPLE(3);
    Free[1] = a;
    Free[2] = Zero;
    Free += 3;
    return;
  case ManyReaders:
#ifdef PROFILING
    cc.OUTPUTmr++; cc.runqAlloc += 2;
#endif
    {
      Val fifo = VAL(ch);
      Val first = OFFSET(fifo,1);
      Val next = OFFSET(first,2);
      EndQ[0] = OFFSET(first,1);
      EndQ[-1] = a;
      EndQ -= 2;
      if (next == OFFSET(fifo,2)) {
        STATUS(ch) = OneReader;
        VAL(ch) = OFFSET(next,1);
      } else {
        OFFSET(fifo,1) = next;
      }
    }
    return;
  default:
#ifdef PROFILING
    cc.OUTPUTdef++; cc.runqAlloc += 2;
#endif
    EndQ[0] = ch;
    EndQ[-1] = a;
    EndQ -= 2;
    return;
  }
}
