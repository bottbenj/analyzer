// NOMARSHAL PARAM: --enable ana\.library --sets ana\.activated\[-\] threadid  --sets ana\.activated\[\+\] mallocWrapperTypeBased  --sets ana\.activated\[\+\] typecasts --sets ana\.activated\[-\] mallocWrapper --sets ana\.activated\[\+\] writtenLvals --sets ana\.activated\[\+\] varArgs

#include <assert.h>
int foo(int *p){
    long x = 0;
    long *xp = &x;
    *xp = 2;
    *p = 3;
    return *p;
}

int bar(int *p, long *l){
    return foo(p);
}

int main(){
    int pp = 3;
    int *p = &pp;
    int q = 3;
    int *x = &q;
    long ll = 4;
    long *l = &ll;
    *x = 323;
    bar(p, l);
    assert(*p==3); // UNKNOWN!
    assert(*l==4);
    return 0;
}
