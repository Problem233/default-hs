#include <iostream>

using namespace std;

bool check(int x) {
  bool result = true;
  for(int i = 2; i <= 20; i ++)
    if(x % i) return false;
  return true;
}

int main() {
  int r = 20;
  while(!check(r)) r += 20;
  cout << r << endl;
  return 0;
}
