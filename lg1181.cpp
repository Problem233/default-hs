#include <iostream>

using namespace std;

int main() {
  int n, m;
  cin >> n >> m;
  int sum = 0;
  int s = 0;
  int i;
  while(n > 0) {
    cin >> i;
    if(sum + i <= m) sum += i;
    else {
      sum = i;
      s++;
    }
    n--;
  }
  s++;
  cout << s << endl;
  return 0;
}
