// PARAM: --set solver td3 --enable ana.int.interval --disable ana.int.def_exc
#include <stdio.h>
#include <assert.h>
int main(){
    int i;
    if(i<0){
      assert(i<0);
    } else {
      assert(i>=0);
    }
    return 0;
}
