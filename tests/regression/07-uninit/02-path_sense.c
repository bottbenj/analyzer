// PARAM: --set ana.activated "['base','threadid','threadflag','escape','uninit','mallocWrapper']"  --set ana.base.privatization none
#include <stdio.h>

void f() {};

int main() {
  int y = 42;
  int change_y = 0;
  int save_y;
  scanf("%d",&change_y);

  if (change_y) {
    save_y = y;
    y = 10;
  }

  change_y++;
  printf("Doing some other work");
  change_y--;

  if (change_y)
    y = save_y; // NOWARN

  return 0;
}
