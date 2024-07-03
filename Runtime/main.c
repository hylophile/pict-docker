#include "pict.h"
#include <stdlib.h>

Val signalHandler = 0;
Val wakeupHandler = 0;

Val *Free, *StartQ, *EndQ, *StartH, *EndH;

int ArgC;
char **ArgV;

void main(int argc, char *argv[]) {
  /* Save argc and argv */
  ArgC = argc; ArgV = argv;
  /* Initialise the heap */
  initGc();
  /*
   * If profiling, call printProfile on exit.
   */
#ifdef PROFILING
  atexit(printProfile);
#endif
  /*
   * Call the initial process and then call the scheduler to continue
   * running the rest of the program.
   */
  pictMain0(); scheduler();
}
