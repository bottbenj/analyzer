//SKIP! PARAM: --set ana.activated "['mallocWrapper','base','threadid','threadflag','escape','fmode', 'OSEK', 'OSEK2', 'stack_trace_set']" --set ana.osek.oil 17-multicore.oil --set ana.osek.taskprefix function_of_ --set ana.osek.isrprefix function_of_

int x = 0;
int y;
int z;

TASK(Task1Name) {
  /* x = 1; */
  /* assert(x == 1); // UNKNOWN! */
  GetSpinlock(SpinLock1Name);
  x = 2;
  assert(x == 2);
  ReleaseSpinlock(SpinLock1Name);
  return;
}

TASK(Task2Name) {
  // x = 2;
  // assert(x == 2);
  GetSpinlock(SpinLock1Name);
  x = 3;
  assert(x == 3);
  ReleaseSpinlock(SpinLock1Name);
  return;
}
