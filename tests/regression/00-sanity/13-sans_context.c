// PARAM: --analysis base --no-context base
void f(int v, int i){
  assert(v   == 2);
  assert(i*i == 9); // UNKNOWN
}

int main(){
  f(2,-3);
  f(2, 3);
  return 0;
}
