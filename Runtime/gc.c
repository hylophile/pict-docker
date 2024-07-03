#include "pict.h"
#include <stdio.h>
#include <string.h>

External *freeExternals = 0;

static External dummyExternal = {
  /*value*/0, /*previous*/&liveExternals, /*next*/0
};

External liveExternals = {
  /*value*/0, /*previous*/0, /*next*/&dummyExternal
};

Val *fromSpace, *fromLimit, *toSpace, *toLimit;

static inline Val forward (Val val)
{
  if ((val & 1) == 0 || val < (Val)fromSpace || val > (Val)fromLimit) {
#ifdef DEBUG
    fprintf(stderr,"Gc: Ignoring non-pointer (%lx)\n",val);
#endif
    return val;
  }
  {
    Val status = STATUS(val);
    switch (status) {
    case Empty: case OneReader: case OneWriter:
    case ManyReaders: case ManyWriters:
      {
	Val new = TAG(Free);
	if (Free + 2 > toLimit) {
	  fprintf(stderr,"Ran out of heap space\n"); exit(1);
	}
#ifdef DEBUG
	fprintf(stderr,"Gc: Copying channel <%lx> to <%lx>\n",val,new);
#endif
	memcpy(Free,(void *)(val-1),2*sizeof(Val));
	STATUS(val) = Forwarded; VAL(val) = new;
	Free += 2; return new;
      }
    case String:
      {
	Val new = TAG(Free);
	int bytes = SIZE(status);
	int size = (bytes + sizeof(Val) + sizeof(Val) - 1) / sizeof(Val);
	if (Free + size > toLimit) {
	  fprintf(stderr,"Ran out of heap space\n"); exit(1);
	}
#ifdef DEBUG
	fprintf(stderr,"Gc: Copying string <%lx> (size %i) to <%lx>\n",
		val,size,new);
#endif
	memcpy(Free,(void *)(val-1),size*sizeof(Val));
	STATUS(val) = Forwarded; VAL(val) = new;
	Free += size; return new;
      }
    case Forwarded:
#ifdef DEBUG
      fprintf(stderr,"Gc: Forwarding (%lx) to (%lx)\n",val,OFFSET(val,1));
#endif
      return OFFSET(val,1);
    default:
      {
	Val new = TAG(Free);
	int size = SIZE(status);
	if (Free + size > toLimit) {
	  fprintf(stderr,"Ran out of heap space\n"); exit(1);
	}
#ifdef DEBUG
	fprintf(stderr,"Gc: Copying value <%lx> (size %i) to <%lx>\n",
		val,size,new);
#endif
	memcpy(Free,(void *)(val-1),size*sizeof(Val));
	STATUS(val) = Forwarded; VAL(val) = new;
	Free += size; return new;
      }
    }
  }
}

void Gc (int requiredSizeInWords)
{
  int gapSize = fromLimit - StartQ;
  int runqSize = StartQ - EndQ;
#ifdef DEBUG
  fprintf(stderr,"Gc: Gap size %i words\n",gapSize);
  fprintf(stderr,"Gc: Run queue size %i words\n",runqSize);
#endif
  /*
   * If there is a process waiting for external events, then we
   * increase the required size by two, since we might need to unblock
   * the signal handler.
   */
  if (signalHandler) requiredSizeInWords += 2;
  /*
   * If there is enought free space in the gap (between the start of
   * the run queue and the end of fromSpace) then we simply shift the
   * run queue back up to the end of fromSpace.
   */
  if (gapSize > requiredSizeInWords) {
#ifdef PROFILING
    cc.compact++; cc.gap += gapSize;
#endif
#ifdef DEBUG
    fprintf(stderr,"Gc: Shifting run queue\n");
#endif
    COPY((char *)(fromLimit-runqSize),(char *)(EndQ+1),runqSize*sizeof(Val));
    EndQ = fromLimit-runqSize-1; StartQ = fromLimit-1;
    goto CHECK;
  }
  /*
   * The global variable Free keeps track of where the next free
   * address is in toSpace.
   */
  Free = toSpace;
  /*
   * All processes in the run queue are roots, so we copy the objects they
   * point to to the start of toSpace, and copy the processes themselves
   * to the end of toSpace (where the new run queue will live).
   */
  {
    Val *from = EndQ+1, *to = toLimit-runqSize;
#ifdef DEBUG
    fprintf(stderr,"Gc: Scanning run queue\n");
#endif
    while (from <= StartQ) *to++ = forward(*from++);
    StartQ = toLimit-1; EndQ = (toLimit-runqSize)-1;
  }
  /*
   * The signal handler process is also a root.
   */
#ifdef DEBUG
  fprintf(stderr,"Gc: Scanning signal handler process\n");
#endif
  signalHandler = forward(signalHandler);
  /*
   * All Pict values which are stored outside the normal heap are linked
   * together using data structure liveExternals. Everything in liveExternals
   * is consider as a root for garbage collection and copied to the start
   * of the toSpace.
   */
  {
    External *ext = &liveExternals;
#ifdef DEBUG
    fprintf(stderr,"Scanning external values\n");
#endif
    while (ext) {
      ext->value = forward(ext->value);
      ext = ext->next;
    }
  }
  /*
   * Now that we have forwarded all the roots into toSpace, we start scanning
   * toSpace, to forward everything else.
   */
  {
    int status;
    Val *scanPtr = toSpace;
#ifdef DEBUG
    fprintf(stderr,"Scanning forwarded values\n");
#endif
    while (scanPtr < Free) {
      status = scanPtr[0];
      switch (status) {
      case Empty:
#ifdef DEBUG
	fprintf(stderr,"Gc: Scanning empty channel <%lx>\n",(Val)scanPtr);
#endif
	scanPtr += 2; break;
      case OneReader: case OneWriter:
      case ManyReaders: case ManyWriters:
#ifdef DEBUG
	fprintf(stderr,"Gc: Scanning channel <%lx>\n",(Val)scanPtr);
#endif
	scanPtr[1] = forward(scanPtr[1]); scanPtr += 2; break;
      case String:
	{
	  int bytes = SIZE(status);
	  int size = (bytes + sizeof(Val) + sizeof(Val) - 1) / sizeof(Val);
#ifdef DEBUG
	  fprintf(stderr,"Gc: Scanning string (size %d) [%lx]\n",
		  SIZE(status),(Val)scanPtr);
#endif
	  scanPtr += size;
	  break;
	}
      default:
#ifdef DEBUG
	fprintf(stderr,"Gc: Scanning tuple (size %d) [%lx]\n",
		SIZE(status),(Val)scanPtr);
#endif
	{
	  Val *limit = scanPtr + SIZE(status);
	  while (++scanPtr < limit) *scanPtr = forward(*scanPtr);
	  break;
	}
      }
    }
    /*
     * Once there is no more data left to be forwarded, we swap the
     * to and from spaces.
     */
    {
      Val *temp;
      temp = fromSpace; fromSpace = toSpace; toSpace = temp;
      temp = fromLimit; fromLimit = toLimit; toLimit = temp;
    }
    /*
     * If there is still not enough space left, we fail.
     */
#ifdef PROFILING
    cc.gc++; cc.live += (Free-fromSpace)+(StartQ-EndQ);
#endif
#ifdef DEBUG
    fprintf(stderr,"Gc: Reclaimed %d words",(int)(EndQ-Free));
#endif
    if (Free + requiredSizeInWords > EndQ) {
      fprintf(stderr,"Ran out of heap space (needed %d, reclaimed %d)\n",
	      requiredSizeInWords,(int)(EndQ-Free));
      exit(1);
    }
    /*
     * Now that we have reclaimed enough memory, we check if there is
     * a process waiting for external events.  If so, we test whether we
     * should unblock that process.
     */
  CHECK:
    if (wakeupHandler || StartQ == EndQ) {
      if (signalHandler) { 
#ifdef DEBUG
	fprintf(stderr,"GC: starting signal handler\n");
#endif
#ifdef PROFILING
	cc.idle++;
#endif
	/*
	 * Put signal handler on the end of the run queue.
	 */
	EndQ[0] = signalHandler; EndQ[-1] = (StartQ == EndQ); EndQ -= 2;
	/*
	 * By setting signalHandler to zero and then setting wakeupHandler
	 * to zero we ensure that subsequent signals will not cause another
	 * copy of signalHandler to be run.  The signal handler is responsible
	 * for resetting the value of signalHandler, once it has finished
	 * processing any interrupts.  Note that it is essential that we set
	 * signalHandler to zero first (wakeupHandler might otherwise end up
	 * being non-zero, if an interrupt occurs between setting wakeupHandler
	 * to zero and setting signalHandler to zero).
	 */
	signalHandler = 0; wakeupHandler = 0;
      } else {
	exit(0);
      }
    }
  }
}
