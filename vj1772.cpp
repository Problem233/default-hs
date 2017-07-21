#include <iostream>

using namespace std;

int main() {
  for (int i = 123; i <= 987 / 3; i++)
    if (i % 10 % 5 != 0 && i / 10 % 10 != 0) {
      bool ints[9] = {false};
      for (int j = 1; j <= 3; j++) {
        int n = i * j;
        if (n % 10 != 0 && n / 10 % 10 != 0) {
          ints[n % 10 - 1] = true;
          n /= 10;
          ints[n % 10 - 1] = true;
          n /= 10;
          ints[n - 1] = true;
        }
      }
      if (ints[0] && ints[1] && ints[2] && ints[3] && ints[4] &&
          ints[5] && ints[6] && ints[7] && ints[8])
        cout << i << ' ' << i * 2 << ' ' << i * 3 << endl;
    }
  return 0;
}
