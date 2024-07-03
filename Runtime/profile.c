/*
 * Pict low-level profiling.
 */

#include "pict.h"
#include <stdio.h>
#include <sys/types.h>
#include <sys/times.h>

#ifdef PROFILING

CostCentre cc = {};
static struct tms buffer;

void printProfile (void)
{
  long temp;
  if ((int)times(&buffer) != -1) {
    fprintf(stderr,"user time:   %9li ticks\n",
	    (long)(buffer.tms_utime + buffer.tms_cutime));
    fprintf(stderr,"system time: %9li ticks\n",
	    (long)(buffer.tms_stime + buffer.tms_cstime));
  }
  temp = cc.closureAlloc + cc.defAlloc + cc.runqAlloc + cc.chanAlloc +
    cc.ccodeAlloc + cc.tupleAlloc + cc.recordAlloc + cc.fifoAlloc;
  fprintf(stderr,"allocation:  %9li words\n", temp);
  fprintf(stderr,"heap size:   %9li words\n", (long)(fromLimit-fromSpace));
  if (cc.gc) {
    fprintf(stderr,
	    "live data:   %9li words (average over %li collections)\n",
	    cc.live/cc.gc, cc.gc);
  }
  if (cc.compact) {
    fprintf(stderr,
	    "gap size:    %9li words (average over %li compactions)\n",
	    cc.gap/cc.compact, cc.compact);
  }
  fprintf(stderr,"idle:        %9li\n",cc.idle);
  fprintf(stderr,"\n");
  fprintf(stderr,"input unknown : %9li\n",cc.INPUT);
  fprintf(stderr,"input known   : %9li\n",cc.INPUTk);
  fprintf(stderr,"input linear  : %9li\n",cc.INPUTl);
  fprintf(stderr,"input( e) %9li\n",cc.INPUTe);
  fprintf(stderr,"input(1r) %9li\n",cc.INPUT1r);
  fprintf(stderr,"input(1w) %9li\n",cc.INPUT1w);
  fprintf(stderr,"input(mr) %9li\n",cc.INPUTmr);
  fprintf(stderr,"input(mw) %9li\n",cc.INPUTmw);
  fprintf(stderr,"\n");
  fprintf(stderr,"output unknown : %9li\n",cc.OUTPUT);
  fprintf(stderr,"output known   : %9li\n",cc.OUTPUTk);
  fprintf(stderr,"output linear  : %9li\n",cc.OUTPUTl);
  fprintf(stderr,"output( e):  %9li\n",cc.OUTPUTe);
  fprintf(stderr,"output(1r):  %9li\n",cc.OUTPUT1r);
  fprintf(stderr,"output(1w):  %9li\n",cc.OUTPUT1w);
  fprintf(stderr,"output(mr):  %9li\n",cc.OUTPUTmr);
  fprintf(stderr,"output(mw):  %9li\n",cc.OUTPUTmw);
  fprintf(stderr,"output( d):  %9li\n",cc.OUTPUTdef);
  fprintf(stderr,"\n             Allocations   Average     Total\n");
  if (cc.runqAlloc) {
    fprintf(stderr,"run queue:     %9li %9li %9li words\n",
	    cc.OUTPUT1r + cc.OUTPUTmr + cc.OUTPUTdef +
	    cc.INPUT1w + cc.INPUTmw,
	    cc.runqAlloc/
	    (cc.OUTPUT1r + cc.OUTPUTmr + cc.OUTPUTdef +
	     cc.INPUT1w + cc.INPUTmw),
	    cc.runqAlloc);
  }
  if (cc.defAlloc) {
    fprintf(stderr,"definitions:   %9li %9li %9li words\n",
	    cc.def, cc.defAlloc/cc.def, cc.defAlloc);
  }
  if (cc.chanAlloc) {
    fprintf(stderr,"channels:      %9li %9li %9li words\n",
	    cc.new, cc.chanAlloc/cc.new, cc.chanAlloc);
  }
  if (cc.fifoAlloc) {
    fprintf(stderr,"fifos:         %9li %9li %9li words\n",
	    cc.INPUTmr+cc.INPUT1r+cc.OUTPUTmw+cc.OUTPUT1w,
	    cc.fifoAlloc/(cc.INPUTmr+cc.INPUT1r+cc.OUTPUTmw+cc.OUTPUT1w),
	    cc.fifoAlloc);
  }
  if (cc.ccodeAlloc) {
    fprintf(stderr,"ccode:         %9li %9li %9li words\n",
	    cc.ccode,cc.ccodeAlloc/cc.ccode,cc.ccodeAlloc);
  }
  if (cc.tupleAlloc) {
    fprintf(stderr,"tuples:        %9li %9li %9li words\n",
	    cc.tuple, cc.tupleAlloc/cc.tuple, cc.tupleAlloc);
  }
  if (cc.recordAlloc) {
    fprintf(stderr,"records:       %9li %9li %9li words\n",
	    cc.record, cc.recordAlloc/cc.record,
	    cc.recordAlloc);
  }
  if (cc.closureAlloc) {
    fprintf(stderr,"closures:      %9li %9li %9li words\n",
	    cc.closures, cc.closureAlloc/cc.closures, cc.closureAlloc);
  }
}

#endif
