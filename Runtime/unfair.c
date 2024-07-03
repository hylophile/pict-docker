/*
 * An unfair scheduler loop.
 */

#include "pict.h"
#include <stdlib.h>

void unfair (void)
{
  while (1) {
    if (wakeupHandler || StartQ == EndQ) {
      if (signalHandler) {
#ifdef PROFILING
	cc.idle++;
#endif
	/*
	 * Make sure there is space to put a signal handler on the run queue.
	 */
	if (Free + 2 > EndQ) Gc(2);
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
	/*
	 * Run the last thing on the run queue.
	 */
	((void(*)(void))(OFFSET(EndQ[2],1)))();
      } else {
	exit(0);
      }
    } else {
      /*
       * Run the last thing on the run queue.
       */
      ((void(*)(void))(OFFSET(EndQ[2],1)))();
    } 
  }
}
