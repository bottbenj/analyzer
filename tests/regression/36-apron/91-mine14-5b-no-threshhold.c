// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag --set ana.activated[-] threadJoins --disable ana.apron.threshold_widening
// Fig 5 from Miné 2014
// This only succeeds if we use a different widening strategy (no widening :())
#include <pthread.h>
#include <stdio.h>

int x;
int y;
int hundred = 100;
pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  int top;
  while(top) {
    pthread_mutex_lock(&mutex);
    if(x>0) {
      x = x-1;
      y = y-1;
    }
    pthread_mutex_unlock(&mutex);
  }
  return NULL;
}

void *t_fun2(void *arg) {
  int top;
  while(top) {
    pthread_mutex_lock(&mutex);
    if(x<10) {
      x = x+1;
      y = y+1;
    }
    pthread_mutex_unlock(&mutex);
  }
  return NULL;
}


int main(void) {
  int top, top2;


  pthread_t id;
  pthread_t id2;
  pthread_create(&id, NULL, t_fun, NULL);
  pthread_create(&id2, NULL, t_fun2, NULL);
  pthread_mutex_lock(&mutex);
  assert(x==y); //TODO
  pthread_mutex_unlock(&mutex);
  return 0;
}
