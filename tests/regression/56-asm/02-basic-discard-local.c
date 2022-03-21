#include <assert.h>

int main()
{
  int x = 0;

  asm("nop");

  assert(x == 0); // UNKNOWN

  return 0;
}
