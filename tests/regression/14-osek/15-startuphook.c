// NOMARSHAL PARAM: --set ana.activated "['base','threadid','threadflag','escape','fmode', 'OSEK', 'OSEK2', 'stack_trace_set','mallocWrapper']" --set ana.osek.oil 01-privatize.oil --set ana.osek.taskprefix function_of_ --set ana.osek.isrprefix function_of_

int x;
int y;
int z;
int t;

void StartupHook() {
   x = 1;
}

ISR( ii ) {
   x = 1;
   assert(x == 1);
   return;
}


ISR( i) {
   GetResource(r);
   x++;
   assert(x == 2);
   x--;
   ReleaseResource(r);
   assert(x == 1);
   return;
}

TASK(t) {
   y=0;
   assert(y == 0);
   x=1;
   assert(x == 1);
   y=x;
   assert(y == 1);
   return;
}
