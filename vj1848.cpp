#include <iostream>

using namespace std;

int main() {
  int n, x;
  cin >> n >> x;
  int c = 0;
  for (int i = 1; i <= n; i++) {
    int a = i;
    while (a > 0) {
      if (a % 10 == x) c++;
      a /= 10;
    }
  }
  cout << c;
  return 0;
}
